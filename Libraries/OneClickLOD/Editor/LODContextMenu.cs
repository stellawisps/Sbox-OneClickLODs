using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Editor;
using MeshOptimizer;

namespace Sandbox;

public class LODContextMenu
{
	private static readonly HashSet<string> MeshExtensions =
		new HashSet<string>( StringComparer.OrdinalIgnoreCase ) { "vmdl" };
	
	[Event("asset.contextmenu", Priority = 50)]
	private protected static void OnMeshFileAssetContext(AssetContextMenu e)
	{
		var meshes = e.SelectedList
			.Where(x => x.Asset is not null && MeshExtensions.Contains(x.AssetType.FileExtension))
			.Select(x => x.Asset)
			.ToList();

		if (meshes.Count != 0)
		{
			if (meshes.Count == 1)
			{
				var mdl = meshes.First();
				e.Menu.AddOption("Create LODs..", "open_in_new", () =>
				{
					if (mdl.AbsolutePath == null) return;
                    
					var pathName = Path.GetFileNameWithoutExtension(mdl.AbsolutePath);
					var relativePath = mdl.AbsolutePath.Replace(Path.GetFileName(mdl.AbsolutePath), "");
					var targetPath = Path.Combine(relativePath, pathName + "_LODs.obj");
                    
					Log.Info($"Creating LODs for: {mdl.AbsolutePath}");
					Log.Info($"Target path: {targetPath}");
                    
					try
					{
						VMDLEditor.LODCreator.CreateLODs(mdl.AbsolutePath, targetPath);
                        
						// Also update the VMDL file to include LOD configuration
						var vmdlPath = mdl.AbsolutePath.Replace(".vmdl", ".vmdl");
						if (File.Exists(vmdlPath))
						{

							VMDLEditor.AddLODsToVMDL( vmdlPath, pathName + "_LODs.obj", GetLODSize(Model.Load( mdl.RelativePath ).Bounds) );
							Log.Info("VMDL file updated with LOD configuration!");
						}
                        
						Log.Info("LODs created successfully!");
					}
					catch (Exception ex)
					{
						Log.Error($"Failed to create LODs: {ex.Message}");
					}
				});
			}
		}
	}

	private static float GetLODSize(BBox bounds)
	{
		var average = (bounds.Size.z + bounds.Size.x + bounds.Size.y)/3.0f;
		return average;
	}
}

public static class VMDLEditor
{
	public static void AddLODsToVMDL( string vmdlPath, string lodObjFilename, float lodSize )
	{
		try
		{
			var content = File.ReadAllText( vmdlPath );
			var vmdlDir = Path.GetDirectoryName( vmdlPath );

			vmdlDir = System.IO.Path.GetRelativePath( Project.Current.GetAssetsPath(), vmdlPath );
			vmdlDir = vmdlDir.Replace( Path.GetFileName( vmdlDir ) ,"");
			var modifiedContent = AddLODConfiguration( content, lodObjFilename, lodSize, vmdlDir );
			File.WriteAllText( vmdlPath, modifiedContent );

			Log.Info( $"Updated VMDL file: {vmdlPath}" );
		}
		catch ( Exception ex )
		{
			Log.Error( $"Failed to update VMDL file: {ex.Message}" );
		}
	}

	private static string AddLODConfiguration( string content, string lodObjFilename, float lodSize, string pathName )
	{
		var lines = content.Split( '\n' ).ToList();

		// First, completely remove any existing LOD configuration
		CleanExistingLODs( lines );

		// Find the RenderMeshList section and add LOD mesh files
		int renderMeshListIndex = FindRenderMeshListIndex( lines );
		if ( renderMeshListIndex == -1 )
		{
			throw new Exception( "Could not find RenderMeshList in VMDL file" );
		}

		// Find the closing bracket of the children array in RenderMeshList
		int childrenEndIndex = FindRenderMeshListChildrenEnd( lines, renderMeshListIndex );
		if ( childrenEndIndex == -1 )
		{
			throw new Exception( "Could not find RenderMeshList children end" );
		}

		// Get original mesh name before inserting LODs
		var originalMeshName = FindOriginalMeshName( lines, renderMeshListIndex );

		// Insert LOD mesh files before the closing bracket
		var lodMeshEntries = GenerateLODMeshEntries( lodObjFilename, pathName );
		lines.InsertRange( childrenEndIndex, lodMeshEntries );

		// Find the root node's children array and add LODGroupList there
		int rootChildrenEndIndex = FindRootChildrenEndIndex( lines );
		if ( rootChildrenEndIndex == -1 )
		{
			throw new Exception( "Could not find root node children array in VMDL file" );
		}

		var lodGroupEntries = GenerateLODGroupEntries( lodObjFilename, originalMeshName, lodSize );
		lines.InsertRange( rootChildrenEndIndex, lodGroupEntries );

		return string.Join( '\n', lines );
	}

	private static void CleanExistingLODs( List<string> lines )
	{
		// Remove existing LODGroupList - do this first as it's at root level
		RemoveAllLODGroupLists( lines );

		// Remove existing LOD RenderMeshFiles
		RemoveAllLODRenderMeshFiles( lines );

		Log.Info( "Cleaned existing LOD configuration" );
	}

	private static void RemoveAllLODGroupLists( List<string> lines )
	{
		// Work backwards to maintain indices while removing
		for ( int i = lines.Count - 1; i >= 0; i-- )
		{
			if ( lines[i].Contains( "_class = \"LODGroupList\"" ) )
			{
				// Find the complete LODGroupList block
				int blockStart = FindBlockStart( lines, i );
				int blockEnd = FindBlockEnd( lines, blockStart );

				if ( blockStart >= 0 && blockEnd >= 0 && blockStart <= blockEnd )
				{
					Log.Info( $"Removing complete LODGroupList block from line {blockStart} to {blockEnd}" );

					// Remove the entire block including trailing comma
					for ( int j = blockEnd; j >= blockStart; j-- )
					{
						lines.RemoveAt( j );
					}
				}
			}
		}
	}

	private static void RemoveAllLODRenderMeshFiles( List<string> lines )
	{
		// Work backwards to maintain indices while removing
		for ( int i = lines.Count - 1; i >= 0; i-- )
		{
			if ( lines[i].Contains( "_class = \"RenderMeshFile\"" ) )
			{
				// Check if this RenderMeshFile block contains LOD content
				bool isLODMesh = false;
				int blockStart = FindBlockStart( lines, i );
				int blockEnd = FindBlockEnd( lines, blockStart );

				if ( blockStart >= 0 && blockEnd >= 0 )
				{
					// Check the entire block for LOD indicators
					for ( int j = blockStart; j <= blockEnd; j++ )
					{
						if ( lines[j].Contains( "_LODs" ) )
						{
							isLODMesh = true;
							break;
						}
					}

					if ( isLODMesh )
					{
						Log.Info( $"Removing LOD RenderMeshFile block from line {blockStart} to {blockEnd}" );

						// Remove the entire block including trailing comma
						for ( int j = blockEnd; j >= blockStart; j-- )
						{
							lines.RemoveAt( j );
						}
					}
				}
			}
		}
	}

	private static int FindBlockStart( List<string> lines, int classLineIndex )
	{
		// Go backwards to find the opening brace that starts this block
		for ( int i = classLineIndex; i >= 0; i-- )
		{
			var line = lines[i].Trim();
			if ( line == "{" )
			{
				return i;
			}
		}

		return -1;
	}

	private static int FindBlockEnd( List<string> lines, int blockStartIndex )
	{
		// Find matching closing brace
		int braceCount = 0;

		for ( int i = blockStartIndex; i < lines.Count; i++ )
		{
			var line = lines[i].Trim();

			if ( line == "{" )
			{
				braceCount++;
			}
			else if ( line == "}" || line == "}," )
			{
				braceCount--;
				if ( braceCount == 0 )
				{
					return i;
				}
			}
		}

		return -1;
	}

	private static int FindRenderMeshListIndex( List<string> lines )
	{
		for ( int i = 0; i < lines.Count; i++ )
		{
			if ( lines[i].Contains( "_class = \"RenderMeshList\"" ) )
			{
				return i;
			}
		}

		return -1;
	}

	private static int FindRenderMeshListChildrenEnd( List<string> lines, int renderMeshListIndex )
	{
		// Find the children array within the RenderMeshList
		int childrenStart = -1;

		for ( int i = renderMeshListIndex; i < lines.Count; i++ )
		{
			if ( lines[i].Contains( "children =" ) )
			{
				childrenStart = i;
				break;
			}

			// Stop if we hit another section
			if ( i > renderMeshListIndex && lines[i].Contains( "_class =" ) )
			{
				break;
			}
		}

		if ( childrenStart == -1 ) return -1;

		// Find the closing bracket of this children array
		int bracketCount = 0;
		bool foundOpenBracket = false;

		for ( int i = childrenStart; i < lines.Count; i++ )
		{
			var line = lines[i].Trim();

			if ( line.Contains( "[" ) )
			{
				bracketCount++;
				foundOpenBracket = true;
			}

			if ( line.Contains( "]" ) )
			{
				bracketCount--;
				if ( foundOpenBracket && bracketCount == 0 )
				{
					return i; // Return the line with the closing bracket
				}
			}
		}

		return -1;
	}

	private static int FindRootChildrenEndIndex( List<string> lines )
	{
		// Find the root node first
		int rootNodeIndex = -1;
		for ( int i = 0; i < lines.Count; i++ )
		{
			if ( lines[i].Contains( "_class = \"RootNode\"" ) )
			{
				rootNodeIndex = i;
				break;
			}
		}

		if ( rootNodeIndex == -1 ) return -1;

		// Find the children array in the root node
		int childrenStart = -1;

		for ( int i = rootNodeIndex; i < lines.Count; i++ )
		{
			if ( lines[i].Contains( "children =" ) )
			{
				childrenStart = i;
				break;
			}
		}

		if ( childrenStart == -1 ) return -1;

		// Find the matching closing bracket for the root children array
		int bracketCount = 0;
		bool foundOpenBracket = false;

		for ( int i = childrenStart; i < lines.Count; i++ )
		{
			var line = lines[i].Trim();

			if ( line.Contains( "[" ) )
			{
				bracketCount++;
				foundOpenBracket = true;
			}

			if ( line.Contains( "]" ) )
			{
				bracketCount--;
				if ( foundOpenBracket && bracketCount == 0 )
				{
					return i; // Return index of closing bracket
				}
			}
		}

		return -1;
	}

	private static string FindOriginalMeshName( List<string> lines, int renderMeshListIndex )
	{
		// Look for the first non-LOD RenderMeshFile in the RenderMeshList
		for ( int i = renderMeshListIndex; i < lines.Count; i++ )
		{
			if ( lines[i].Contains( "_class = \"RenderMeshFile\"" ) )
			{
				// Check if this is an LOD mesh by looking ahead
				bool isLODMesh = false;
				string foundName = null;

				for ( int j = i + 1; j < Math.Min( i + 15, lines.Count ); j++ )
				{
					var line = lines[j];

					// Check for LOD indicators
					if ( line.Contains( "_LODs" ) )
					{
						isLODMesh = true;
						break;
					}

					// Extract filename first (priority)
					if ( line.Contains( "filename = " ) && !isLODMesh )
					{
						continue;
						var filenameLine = line.Trim();
						var startQuote = filenameLine.IndexOf( '"' );
						var endQuote = filenameLine.LastIndexOf( '"' );
						if ( startQuote >= 0 && endQuote > startQuote )
						{
							var filename = filenameLine.Substring( startQuote + 1, endQuote - startQuote - 1 );
							foundName = Path.GetFileNameWithoutExtension( filename );
						}

						break;
					}

					// Extract name if no filename found yet
					if ( line.Contains( "name = " ) && foundName == null && !isLODMesh )
					{
						var nameLine = line.Trim();
						var startQuote = nameLine.IndexOf( '"' );
						var endQuote = nameLine.LastIndexOf( '"' );
						if ( startQuote >= 0 && endQuote > startQuote )
						{
							foundName = nameLine.Substring( startQuote + 1, endQuote - startQuote - 1 );
						}
					}

					// Stop if we hit another section
					if ( line.Contains( "_class =" ) || line.Trim() == "}," )
					{
						break;
					}
				}

				// If this is not an LOD mesh, return the found name
				if ( !isLODMesh )
				{
					return foundName ?? "unnamed_1";
				}
			}
		}

		return "unnamed_1";
	}

	private static List<string> GenerateLODMeshEntries( string lodObjFilename, string pathName )
	{
		var entries = new List<string>();

		// Extract base name from the LOD filename (remove _LODs.obj)
		var baseName = Path.GetFileNameWithoutExtension( lodObjFilename ).Replace( "_LODs", "" );

		// Generate 5 LOD mesh entries (LOD_0 through LOD_4)
		for ( int i = 0; i < 5; i++ )
		{
			var exclusionList = new List<string>();

			// Add all other LOD levels to exclusion list
			for ( int j = 0; j < 5; j++ )
			{
				if ( j != i )
				{
					exclusionList.Add( $"\t\t\t\t\t\t\t\t\"LOD_{j}\"," );
				}
			}

			var lodName = i == 0 ? $"{baseName}_LODs" : $"{baseName}_LODs{i}";

			entries.AddRange( new[]
			{
				"\t\t\t\t\t{", "\t\t\t\t\t\t_class = \"RenderMeshFile\"", $"\t\t\t\t\t\tname = \"{lodName}\"",
				$"\t\t\t\t\t\tfilename = \"{pathName.Replace( "\\", "/" )}{lodObjFilename}\"",
				"\t\t\t\t\t\timport_translation = [ 0.0, 0.0, 0.0 ]",
				"\t\t\t\t\t\timport_rotation = [ 0.0, 0.0, 0.0 ]", "\t\t\t\t\t\timport_scale = 1.0",
				"\t\t\t\t\t\talign_origin_x_type = \"None\"", "\t\t\t\t\t\talign_origin_y_type = \"None\"",
				"\t\t\t\t\t\talign_origin_z_type = \"None\"", "\t\t\t\t\t\tparent_bone = \"\"",
				"\t\t\t\t\t\timport_filter = ", "\t\t\t\t\t\t{", "\t\t\t\t\t\t\texclude_by_default = false",
				"\t\t\t\t\t\t\texception_list = ", "\t\t\t\t\t\t\t["
			} );
			
			// Add exclusion list
			entries.AddRange( exclusionList );

			// Close the structures properly
			if ( i == 4 ) // Last entry
			{
				entries.AddRange( new[] { "\t\t\t\t\t\t\t]", "\t\t\t\t\t\t}", "\t\t\t\t\t}" } );
			}
			else
			{
				entries.AddRange( new[] { "\t\t\t\t\t\t\t]", "\t\t\t\t\t\t}", "\t\t\t\t\t}," } );
			}
		}

		return entries;
	}

	private static List<string> GenerateLODGroupEntries( string lodObjFilename, string originalMeshName,
		float baseSize )
	{
		// Extract base name from the LOD filename
		var baseName = Path.GetFileNameWithoutExtension( lodObjFilename ).Replace( "_LODs", "" );

		var entries = new List<string>
		{
			"\t\t\t{", "\t\t\t\t_class = \"LODGroupList\"", "\t\t\t\tchildren = ", "\t\t\t\t["
		};

		// LOD distances - you can adjust these values as needed
		var lodDistances = new float[]
		{
			0.0f * baseSize, 2.0f * baseSize, 4.0f * baseSize, 6.0f * baseSize, 9.0f * baseSize, 13.0f * baseSize
		};
		var lodMeshNames = new string[]
		{
			originalMeshName, // Use the actual original mesh name
			$"{baseName}_LODs", $"{baseName}_LODs1", $"{baseName}_LODs2", $"{baseName}_LODs3", $"{baseName}_LODs4"
		};

		for ( int i = 0; i < lodDistances.Length; i++ )
		{
			var comma = i == lodDistances.Length - 1 ? "" : ","; // No comma on last entry

			entries.AddRange( new[]
			{
				"\t\t\t\t\t{", "\t\t\t\t\t\t_class = \"LODGroup\"",
				$"\t\t\t\t\t\tswitch_threshold = {lodDistances[i]:F1}", "\t\t\t\t\t\tmeshes = ", "\t\t\t\t\t\t[",
				$"\t\t\t\t\t\t\t\"{lodMeshNames[i]}\",", "\t\t\t\t\t\t]", $"\t\t\t\t\t}}{comma}"
			} );
		}

		entries.AddRange( new[] { "\t\t\t\t]", "\t\t\t}," } );

		return entries;
	}


	public static class LODCreator
	{
		public static void CreateLODs( string inputPath, string outputPath )
		{
			var originalMesh = LoadMesh( inputPath );
			if ( originalMesh == null )
			{
				Log.Error( "Failed to load mesh" );
				return;
			}

			// More aggressive LOD levels with hybrid approach
			var lodConfigs = new[]
			{
				new LODConfig { Level = 0.8f, Method = LODMethod.Conservative, Error = 0.005f },
				new LODConfig { Level = 0.55f, Method = LODMethod.Conservative, Error = 0.01f },
				new LODConfig { Level = 0.3f, Method = LODMethod.Aggressive, Error = 0.02f },
				new LODConfig { Level = 0.18f, Method = LODMethod.Aggressive, Error = 0.05f },
				new LODConfig { Level = 0.06f, Method = LODMethod.Sloppy, Error = 0.1f }
			};

			var lodMeshes = new List<MeshData>();

			var currentMesh = originalMesh;

			foreach ( var config in lodConfigs )
			{
				var lodMesh = GenerateAdaptiveLOD( currentMesh, config );
				lodMeshes.Add( lodMesh );

				// Use the previous LOD as input for the next one (cascading)
				// This helps break through topology barriers
				currentMesh = lodMesh;

				Log.Info( $"Generated LOD {config.Level} using {config.Method}: {lodMesh.Faces.Count} triangles" );
			}

			WriteOBJFile( outputPath, lodMeshes );
		}

		private enum LODMethod
		{
			Conservative, // Standard meshoptimizer
			Aggressive, // Meshoptimizer with relaxed constraints
			Sloppy // Grid-based decimation for extreme reduction
		}

		private struct LODConfig
		{
			public float Level;
			public LODMethod Method;
			public float Error;
		}

		private static MeshData GenerateAdaptiveLOD( MeshData originalMesh, LODConfig config )
		{
			switch ( config.Method )
			{
				case LODMethod.Conservative:
					return GenerateConservativeLOD( originalMesh, config.Level, config.Error );

				case LODMethod.Aggressive:
					return GenerateAggressiveLOD( originalMesh, config.Level, config.Error );

				case LODMethod.Sloppy:
					return GenerateSloppyLOD( originalMesh, config.Level, config.Error );

				default:
					return originalMesh.Clone();
			}
		}

		private static MeshData GenerateConservativeLOD( MeshData originalMesh, float lodLevel, float targetError )
		{
			// Standard meshoptimizer approach
			var positions = originalMesh.Vertices.Select( v => v.ToMeshOptVector3() ).ToArray();
			var indices = originalMesh.Faces.SelectMany( face => face.Take( 3 ).Select( i => (uint)i ) ).ToArray();

			if ( indices.Length == 0 ) return originalMesh.Clone();

			uint targetIndexCount = Math.Max( 3, (uint)(indices.Length * lodLevel) );
			targetIndexCount = (targetIndexCount / 3) * 3;

			var simplifiedIndices = MeshSimplifier.Simplify(
				indices,
				positions,
				targetIndexCount,
				targetError,
				MeshSimplifier.SimplifyOptions.None
			);

			return ConvertToMeshData( originalMesh, simplifiedIndices );
		}

		private static MeshData GenerateAggressiveLOD( MeshData originalMesh, float lodLevel, float targetError )
		{
			// Try multiple passes with increasingly aggressive settings
			var positions = originalMesh.Vertices.Select( v => v.ToMeshOptVector3() ).ToArray();
			var indices = originalMesh.Faces.SelectMany( face => face.Take( 3 ).Select( i => (uint)i ) ).ToArray();

			if ( indices.Length == 0 ) return originalMesh.Clone();

			uint targetIndexCount = Math.Max( 3, (uint)(indices.Length * lodLevel) );
			targetIndexCount = (targetIndexCount / 3) * 3;

			uint[] currentIndices = indices;

			// Pass 1: Standard simplification
			currentIndices = MeshSimplifier.Simplify(
				currentIndices,
				positions,
				targetIndexCount,
				targetError,
				MeshSimplifier.SimplifyOptions.None
			);

			// If we didn't reach the target, try more aggressive approaches
			if ( currentIndices.Length > targetIndexCount * 1.5f ) // Allow 50% tolerance
			{
				// Pass 2: Allow border vertex movement
				currentIndices = MeshSimplifier.Simplify(
					currentIndices,
					positions,
					targetIndexCount,
					targetError * 2.0f, // Double the error tolerance
					MeshSimplifier.SimplifyOptions.None // Remove LockBorder if it was set
				);
			}

			// If still not reaching target, apply grid-based pre-processing
			if ( currentIndices.Length > targetIndexCount * 1.5f )
			{
				var preprocessedMesh = ApplyGridBasedPreprocessing( originalMesh, 0.7f );
				return GenerateSloppyLOD( preprocessedMesh, lodLevel / 0.7f, targetError );
			}

			return ConvertToMeshData( originalMesh, currentIndices );
		}

		private static MeshData GenerateSloppyLOD( MeshData originalMesh, float lodLevel, float targetError )
		{
			// Use a grid-based approach for extreme decimation
			// This sacrifices quality for guaranteed triangle reduction

			var gridSize = CalculateOptimalGridSize( originalMesh, lodLevel );
			var clusteredMesh = ClusterVerticesInGrid( originalMesh, gridSize );

			// Apply final pass with meshoptimizer if needed
			if ( clusteredMesh.Faces.Count > originalMesh.Faces.Count * lodLevel * 1.2f )
			{
				return GenerateConservativeLOD( clusteredMesh, 0.8f, targetError );
			}

			return clusteredMesh;
		}

		private static MeshData ApplyGridBasedPreprocessing( MeshData mesh, float reductionFactor )
		{
			// Pre-reduce the mesh using grid clustering before applying meshoptimizer
			var gridSize = CalculateOptimalGridSize( mesh, reductionFactor );
			return ClusterVerticesInGrid( mesh, gridSize );
		}

		private static int CalculateOptimalGridSize( MeshData mesh, float targetReduction )
		{
			// Calculate bounding box
			if ( mesh.Vertices.Count == 0 ) return 10;

			var min = mesh.Vertices[0];
			var max = mesh.Vertices[0];

			foreach ( var vertex in mesh.Vertices )
			{
				if ( vertex.x < min.x ) min.x = vertex.x;
				if ( vertex.y < min.y ) min.y = vertex.y;
				if ( vertex.z < min.z ) min.z = vertex.z;
				if ( vertex.x > max.x ) max.x = vertex.x;
				if ( vertex.y > max.y ) max.y = vertex.y;
				if ( vertex.z > max.z ) max.z = vertex.z;
			}

			var extent = Math.Max( Math.Max( max.x - min.x, max.y - min.y ), max.z - min.z );

			// Estimate grid size based on target vertex count
			var targetVertexCount = mesh.Vertices.Count * targetReduction;
			var estimatedGridSize = (int)Math.Ceiling( Math.Pow( targetVertexCount, 1.0 / 3.0 ) );

			// Clamp to reasonable bounds
			return Math.Max( 8, Math.Min( 256, estimatedGridSize ) );
		}

		private static MeshData ClusterVerticesInGrid( MeshData mesh, int gridSize )
		{
			if ( mesh.Vertices.Count == 0 ) return mesh.Clone();

			// Calculate bounding box
			var min = mesh.Vertices[0];
			var max = mesh.Vertices[0];

			foreach ( var vertex in mesh.Vertices )
			{
				if ( vertex.x < min.x ) min.x = vertex.x;
				if ( vertex.y < min.y ) min.y = vertex.y;
				if ( vertex.z < min.z ) min.z = vertex.z;
				if ( vertex.x > max.x ) max.x = vertex.x;
				if ( vertex.y > max.y ) max.y = vertex.y;
				if ( vertex.z > max.z ) max.z = vertex.z;
			}

			var extent = max - min;
			var cellSize = new Vector3(
				extent.x / gridSize,
				extent.y / gridSize,
				extent.z / gridSize
			);

			// Avoid division by zero
			if ( cellSize.x == 0 ) cellSize.x = 1;
			if ( cellSize.y == 0 ) cellSize.y = 1;
			if ( cellSize.z == 0 ) cellSize.z = 1;

			// Grid cell to vertex mapping
			var cellVertices = new Dictionary<(int, int, int), List<int>>();

			// Assign vertices to grid cells
			for ( int i = 0; i < mesh.Vertices.Count; i++ )
			{
				var vertex = mesh.Vertices[i];
				var gridPos = (
					Math.Min( gridSize - 1, (int)((vertex.x - min.x) / cellSize.x) ),
					Math.Min( gridSize - 1, (int)((vertex.y - min.y) / cellSize.y) ),
					Math.Min( gridSize - 1, (int)((vertex.z - min.z) / cellSize.z) )
				);

				if ( !cellVertices.ContainsKey( gridPos ) )
					cellVertices[gridPos] = new List<int>();

				cellVertices[gridPos].Add( i );
			}

			// Create representative vertices for each cell
			var vertexRemap = new Dictionary<int, int>();
			var newVertices = new List<Vector3>();
			var newNormals = new List<Vector3>();
			var newUVs = new List<Vector2>();

			foreach ( var (gridPos, vertexIndices) in cellVertices )
			{
				// Average position, normal, and UV
				var avgPos = Vector3.Zero;
				var avgNormal = Vector3.Zero;
				var avgUV = Vector2.Zero;

				foreach ( var vertexIndex in vertexIndices )
				{
					avgPos += mesh.Vertices[vertexIndex];
					if ( vertexIndex < mesh.Normals.Count )
						avgNormal += mesh.Normals[vertexIndex];
					if ( vertexIndex < mesh.UVs.Count )
						avgUV += mesh.UVs[vertexIndex];
				}

				avgPos /= vertexIndices.Count;
				if ( avgNormal != Vector3.Zero )
					avgNormal = avgNormal.Normal;
				avgUV /= vertexIndices.Count;

				int newVertexIndex = newVertices.Count;
				newVertices.Add( avgPos );
				newNormals.Add( avgNormal );
				newUVs.Add( avgUV );

				// Map all old vertices in this cell to the new representative
				foreach ( var vertexIndex in vertexIndices )
				{
					vertexRemap[vertexIndex] = newVertexIndex;
				}
			}

			// Rebuild faces with new vertex indices
			var newFaces = new List<List<int>>();
			foreach ( var face in mesh.Faces )
			{
				var newFace = new List<int>();
				var uniqueVertices = new HashSet<int>();

				foreach ( var vertexIndex in face )
				{
					if ( vertexRemap.ContainsKey( vertexIndex ) )
					{
						int newIndex = vertexRemap[vertexIndex];
						if ( !uniqueVertices.Contains( newIndex ) )
						{
							newFace.Add( newIndex );
							uniqueVertices.Add( newIndex );
						}
					}
				}

				// Only add valid triangles
				if ( newFace.Count >= 3 )
				{
					// Take only the first 3 vertices if it's a polygon
					newFaces.Add( newFace.Take( 3 ).ToList() );
				}
			}

			return new MeshData { Vertices = newVertices, Normals = newNormals, UVs = newUVs, Faces = newFaces };
		}

		private static MeshData ConvertToMeshData( MeshData originalMesh, uint[] indices )
		{
			var resultMesh = new MeshData
			{
				Vertices = new List<Vector3>( originalMesh.Vertices ),
				Normals = new List<Vector3>( originalMesh.Normals ),
				UVs = new List<Vector2>( originalMesh.UVs ),
				Faces = new List<List<int>>()
			};

			// Convert indices to faces
			for ( int i = 0; i < indices.Length; i += 3 )
			{
				resultMesh.Faces.Add( new List<int> { (int)indices[i], (int)indices[i + 1], (int)indices[i + 2] } );
			}

			// Compact unused vertices
			CompactMesh( resultMesh );
			return resultMesh;
		}

		private static void CompactMesh( MeshData mesh )
		{
			var usedVertices = new HashSet<int>();
			foreach ( var face in mesh.Faces )
			{
				foreach ( var vertexIndex in face )
				{
					usedVertices.Add( vertexIndex );
				}
			}

			var vertexMap = new Dictionary<int, int>();
			var newVertices = new List<Vector3>();
			var newNormals = new List<Vector3>();
			var newUVs = new List<Vector2>();

			int newIndex = 0;
			for ( int i = 0; i < mesh.Vertices.Count; i++ )
			{
				if ( usedVertices.Contains( i ) )
				{
					vertexMap[i] = newIndex;
					newVertices.Add( mesh.Vertices[i] );

					if ( i < mesh.Normals.Count )
						newNormals.Add( mesh.Normals[i] );
					if ( i < mesh.UVs.Count )
						newUVs.Add( mesh.UVs[i] );

					newIndex++;
				}
			}

			foreach ( var face in mesh.Faces )
			{
				for ( int i = 0; i < face.Count; i++ )
				{
					if ( vertexMap.ContainsKey( face[i] ) )
						face[i] = vertexMap[face[i]];
				}
			}

			mesh.Vertices = newVertices;
			mesh.Normals = newNormals;
			mesh.UVs = newUVs;
		}

		// Keep existing utility methods...
		private static MeshData LoadMesh( string path )
		{
			try
			{
				var extension = Path.GetExtension( path ).ToLower();

				if ( extension == ".fbx" )
				{
					return LoadFBXMesh( path );
				}
				else if ( extension == ".obj" )
				{
					return LoadOBJMesh( path );
				}
				else if ( extension == ".vmdl" )
				{
					return LoadFBXMesh( path );
				}

				return null;
			}
			catch ( Exception ex )
			{
				Log.Error( $"Error loading mesh: {ex.Message}" );
				return null;
			}
		}

		private static MeshData LoadFBXMesh( string path )
		{
			Log.Info(
				$"Loading FBX mesh from: {System.IO.Path.GetRelativePath( Project.Current.GetAssetsPath(), path )}" );

			try
			{
				var model = Model.Load( System.IO.Path.GetRelativePath( Project.Current.GetAssetsPath(), path ) );
				if ( model == null )
				{
					Log.Error( $"Failed to load model from: {path}" );
					return null;
				}

				var meshData = new MeshData();
				var vertices = model.GetVertices();
				var indices = model.GetIndices();

				foreach ( var vertex in vertices )
				{
					meshData.Vertices.Add( vertex.Position );

					if ( vertex.Normal != Vector3.Zero )
					{
						meshData.Normals.Add( vertex.Normal );
					}

					meshData.UVs.Add( new Vector2( vertex.TexCoord0 ) );
				}

				for ( int i = 0; i < indices.Length; i += 3 )
				{
					if ( i + 2 < indices.Length )
					{
						var face = new List<int> { (int)indices[i], (int)indices[i + 1], (int)indices[i + 2] };
						meshData.Faces.Add( face );
					}
				}

				Log.Info( $"Loaded FBX: {meshData.Vertices.Count} vertices, {meshData.Faces.Count} faces" );

				return meshData;
			}
			catch ( Exception ex )
			{
				Log.Error( $"Error loading FBX mesh: {ex.Message}" );
				return null;
			}
		}

		private static MeshData LoadOBJMesh( string path )
		{
			var meshData = new MeshData();
			var lines = File.ReadAllLines( path );

			foreach ( var line in lines )
			{
				var parts = line.Split( ' ', StringSplitOptions.RemoveEmptyEntries );
				if ( parts.Length == 0 ) continue;

				switch ( parts[0] )
				{
					case "v":
						if ( parts.Length >= 4 )
						{
							meshData.Vertices.Add( new Vector3(
								float.Parse( parts[1] ),
								float.Parse( parts[2] ),
								float.Parse( parts[3] )
							) );
						}

						break;

					case "vn":
						if ( parts.Length >= 4 )
						{
							meshData.Normals.Add( new Vector3(
								float.Parse( parts[1] ),
								float.Parse( parts[2] ),
								float.Parse( parts[3] )
							) );
						}

						break;

					case "vt":
						if ( parts.Length >= 3 )
						{
							meshData.UVs.Add( new Vector2(
								float.Parse( parts[1] ),
								float.Parse( parts[2] )
							) );
						}

						break;

					case "f":
						if ( parts.Length >= 4 )
						{
							var face = new List<int>();
							for ( int i = 1; i < parts.Length; i++ )
							{
								var indices = parts[i].Split( '/' );
								face.Add( int.Parse( indices[0] ) - 1 );
							}

							meshData.Faces.Add( face );
						}

						break;
				}
			}

			return meshData;
		}

		private static Vector3 ConvertToOBJCoordinates( Vector3 sboxPos )
		{
			return new Vector3( sboxPos.y, sboxPos.z, sboxPos.x );
		}

		private static Vector3 ConvertNormalToOBJCoordinates( Vector3 sboxNormal )
		{
			return new Vector3( -sboxNormal.y, sboxNormal.z, sboxNormal.x );
		}

		private static void WriteOBJFile( string path, List<MeshData> lodMeshes )
		{
			using var writer = new StreamWriter( path );

			writer.WriteLine( "# Generated aggressive LOD OBJ file" );
			writer.WriteLine( $"# Contains {lodMeshes.Count} LOD levels" );
			writer.WriteLine( "# Uses hybrid simplification approach" );
			writer.WriteLine();

			int vertexOffset = 0;
			int normalOffset = 0;
			int uvOffset = 0;

			for ( int lodIndex = 0; lodIndex < lodMeshes.Count; lodIndex++ )
			{
				var mesh = lodMeshes[lodIndex];

				writer.WriteLine(
					$"# LOD Level {lodIndex} - {mesh.Faces.Count} triangles, {mesh.Vertices.Count} vertices" );
				writer.WriteLine( $"g LOD_{lodIndex}" );

				foreach ( var vertex in mesh.Vertices )
				{
					var objVertex = ConvertToOBJCoordinates( vertex );
					writer.WriteLine( $"v {objVertex.x:F6} {objVertex.y:F6} {objVertex.z:F6}" );
				}

				foreach ( var normal in mesh.Normals )
				{
					var objNormal = ConvertNormalToOBJCoordinates( normal );
					writer.WriteLine( $"vn {objNormal.x:F6} {objNormal.y:F6} {objNormal.z:F6}" );
				}

				foreach ( var uv in mesh.UVs )
				{
					var objUV = new Vector2( uv.x, 1.0f - uv.y );
					writer.WriteLine( $"vt {objUV.x:F6} {objUV.y:F6}" );
				}

				foreach ( var face in mesh.Faces )
				{
					writer.Write( "f" );
					foreach ( var vertexIndex in face )
					{
						int v = vertexIndex + vertexOffset + 1;
						int vn = (vertexIndex < mesh.Normals.Count) ? vertexIndex + normalOffset + 1 : 0;
						int vt = (vertexIndex < mesh.UVs.Count) ? vertexIndex + uvOffset + 1 : 0;

						if ( vn > 0 && vt > 0 )
							writer.Write( $" {v}/{vt}/{vn}" );
						else if ( vt > 0 )
							writer.Write( $" {v}/{vt}" );
						else if ( vn > 0 )
							writer.Write( $" {v}//{vn}" );
						else
							writer.Write( $" {v}" );
					}

					writer.WriteLine();
				}

				vertexOffset += mesh.Vertices.Count;
				normalOffset += mesh.Normals.Count;
				uvOffset += mesh.UVs.Count;

				writer.WriteLine();
			}

			Log.Info( $"Aggressive LOD OBJ file written to: {path}" );
		}

		public class MeshData
		{
			public List<Vector3> Vertices { get; set; } = new List<Vector3>();
			public List<Vector3> Normals { get; set; } = new List<Vector3>();
			public List<Vector2> UVs { get; set; } = new List<Vector2>();
			public List<List<int>> Faces { get; set; } = new List<List<int>>();

			public MeshData Clone()
			{
				return new MeshData
				{
					Vertices = new List<Vector3>( Vertices ),
					Normals = new List<Vector3>( Normals ),
					UVs = new List<Vector2>( UVs ),
					Faces = Faces.Select( f => new List<int>( f ) ).ToList()
				};
			}
		}
	}
}
