using System;
using System.Collections.Generic;
using System.Linq;

namespace MeshOptimizer
{
    public static class MeshSimplifier
    {
        private const int MaxAttributes = 32;
        
        public enum SimplifyOptions
        {
            None = 0,
            LockBorder = 1 << 0,
            Sparse = 1 << 1,
            ErrorAbsolute = 1 << 2,
            Prune = 1 << 3
        }

        public enum VertexKind
        {
            Manifold = 0,
            Border = 1,
            Seam = 2,
            Complex = 3,
            Locked = 4
        }

        public struct Vector3
        {
            public float x, y, z;
            
            public Vector3(float x, float y, float z)
            {
                this.x = x;
                this.y = y;
                this.z = z;
            }
            
            public static Vector3 operator +(Vector3 a, Vector3 b) => new Vector3(a.x + b.x, a.y + b.y, a.z + b.z);
            public static Vector3 operator -(Vector3 a, Vector3 b) => new Vector3(a.x - b.x, a.y - b.y, a.z - b.z);
            public static Vector3 operator *(Vector3 a, float s) => new Vector3(a.x * s, a.y * s, a.z * s);
            public static float Dot(Vector3 a, Vector3 b) => a.x * b.x + a.y * b.y + a.z * b.z;
            public static Vector3 Cross(Vector3 a, Vector3 b) => new Vector3(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x);
            
            public float Length => (float)Math.Sqrt(x * x + y * y + z * z);
            public Vector3 Normalized
            {
                get
                {
                    float len = Length;
                    return len > 0 ? this * (1.0f / len) : new Vector3(0, 0, 0);
                }
            }
            
            public float Distance(Vector3 other)
            {
                var diff = this - other;
                return diff.Length;
            }
        }

        public struct Quadric
        {
            public float a00, a11, a22;
            public float a10, a20, a21;
            public float b0, b1, b2, c;
            public float w;
            
            public static Quadric operator +(Quadric a, Quadric b)
            {
                return new Quadric
                {
                    a00 = a.a00 + b.a00,
                    a11 = a.a11 + b.a11,
                    a22 = a.a22 + b.a22,
                    a10 = a.a10 + b.a10,
                    a20 = a.a20 + b.a20,
                    a21 = a.a21 + b.a21,
                    b0 = a.b0 + b.b0,
                    b1 = a.b1 + b.b1,
                    b2 = a.b2 + b.b2,
                    c = a.c + b.c,
                    w = a.w + b.w
                };
            }
        }

        public struct Collapse
        {
            public uint v0, v1;
            public float error;
            public bool bidi;
        }

        private struct EdgeAdjacency
        {
            public uint[] offsets;
            public Edge[] data;
            
            public struct Edge
            {
                public uint next, prev;
            }
        }

        private static readonly bool[,] CanCollapse = new bool[5, 5]
        {
            {true,  true,  true,  true,  true },
            {false, true,  false, false, true },
            {false, false, true,  false, true },
            {false, false, false, true,  true },
            {false, false, false, false, false}
        };

        private static readonly bool[,] HasOpposite = new bool[5, 5]
        {
            {true,  true,  true,  false, true },
            {true,  false, true,  false, false},
            {true,  true,  true,  false, true },
            {false, false, false, false, false},
            {true,  false, true,  false, false}
        };

        public static uint[] Simplify(
            uint[] indices,
            Vector3[] vertexPositions,
            uint targetIndexCount,
            float targetError = 0.01f,
            SimplifyOptions options = SimplifyOptions.None)
        {
            if (indices.Length % 3 != 0)
                throw new ArgumentException("Index count must be divisible by 3");
            
            var result = new uint[indices.Length];
            Array.Copy(indices, result, indices.Length);
            
            uint vertexCount = (uint)vertexPositions.Length;
            uint indexCount = (uint)indices.Length;
            
            // Build position remap
            var remap = BuildPositionRemap(vertexPositions);
            var wedge = new uint[vertexCount];
            BuildWedgeTable(wedge, remap);
            
            // Build adjacency
            var adjacency = BuildEdgeAdjacency(result, indexCount, vertexCount);
            
            // Classify vertices
            var vertexKind = new VertexKind[vertexCount];
            var loop = new uint[vertexCount];
            var loopback = new uint[vertexCount];
            ClassifyVertices(vertexKind, loop, loopback, vertexCount, adjacency, remap, wedge, options);
            
            // Rescale positions
            var positions = RescalePositions(vertexPositions);
            float vertexScale = 1.0f; // You might want to compute this properly
            
            // Build quadrics
            var vertexQuadrics = new Quadric[vertexCount];
            FillFaceQuadrics(vertexQuadrics, result, indexCount, positions, remap);
            FillEdgeQuadrics(vertexQuadrics, result, indexCount, positions, remap, vertexKind, loop, loopback);
            
            uint resultCount = indexCount;
            float resultError = 0;
            
            float errorScale = (options & SimplifyOptions.ErrorAbsolute) != 0 ? vertexScale : 1.0f;
            float errorLimit = (targetError * targetError) / (errorScale * errorScale);
            
            // Simplification passes
            while (resultCount > targetIndexCount)
            {
                UpdateEdgeAdjacency(adjacency, result, resultCount, vertexCount, remap);
                
                var collapses = PickEdgeCollapses(result, resultCount, remap, vertexKind, loop, loopback);
                if (collapses.Count == 0) break;
                
                RankEdgeCollapses(collapses, positions, vertexQuadrics, remap, wedge, vertexKind, loop, loopback);
                collapses.Sort((a, b) => a.error.CompareTo(b.error));
                
                uint triangleCollapseGoal = (resultCount - targetIndexCount) / 3;
                var collapseRemap = new uint[vertexCount];
                for (uint i = 0; i < vertexCount; i++) collapseRemap[i] = i;
                
                var collapseLocked = new bool[vertexCount];
                
                uint collapseCount = PerformEdgeCollapses(
                    collapseRemap, collapseLocked, collapses, remap, wedge, vertexKind,
                    loop, loopback, positions, adjacency, triangleCollapseGoal, errorLimit, ref resultError);
                
                if (collapseCount == 0) break;
                
                UpdateQuadrics(collapseRemap, vertexCount, vertexQuadrics, remap);
                RemapEdgeLoops(loop, vertexCount, collapseRemap);
                RemapEdgeLoops(loopback, vertexCount, collapseRemap);
                
                resultCount = RemapIndexBuffer(result, resultCount, collapseRemap);
            }
            
            // Filter degenerate triangles
            resultCount = FilterIndexBuffer(result, resultCount, remap);
            
            var finalResult = new uint[resultCount];
            Array.Copy(result, finalResult, resultCount);
            return finalResult;
        }

        private static uint[] BuildPositionRemap(Vector3[] positions)
        {
            var remap = new uint[positions.Length];
            var positionMap = new Dictionary<Vector3, uint>();
            
            for (uint i = 0; i < positions.Length; i++)
            {
                var pos = positions[i];
                if (positionMap.TryGetValue(pos, out uint existing))
                {
                    remap[i] = existing;
                }
                else
                {
                    remap[i] = i;
                    positionMap[pos] = i;
                }
            }
            
            return remap;
        }

        private static void BuildWedgeTable(uint[] wedge, uint[] remap)
        {
            for (uint i = 0; i < wedge.Length; i++)
                wedge[i] = i;
                
            for (uint i = 0; i < wedge.Length; i++)
            {
                if (remap[i] != i)
                {
                    uint r = remap[i];
                    wedge[i] = wedge[r];
                    wedge[r] = i;
                }
            }
        }

        private static EdgeAdjacency BuildEdgeAdjacency(uint[] indices, uint indexCount, uint vertexCount)
        {
            var adjacency = new EdgeAdjacency();
            adjacency.offsets = new uint[vertexCount + 1];
            adjacency.data = new EdgeAdjacency.Edge[indexCount];
            
            UpdateEdgeAdjacency(adjacency, indices, indexCount, vertexCount, null);
            return adjacency;
        }

        private static void UpdateEdgeAdjacency(EdgeAdjacency adjacency, uint[] indices, uint indexCount, uint vertexCount, uint[] remap)
        {
            uint faceCount = indexCount / 3;
            
            // Clear offsets
            for (uint i = 0; i < vertexCount; i++)
                adjacency.offsets[i + 1] = 0;
            
            // Count edges per vertex
            for (uint i = 0; i < indexCount; i++)
            {
                uint v = remap != null ? remap[indices[i]] : indices[i];
                adjacency.offsets[v + 1]++;
            }
            
            // Convert counts to offsets
            for (uint i = 0; i < vertexCount; i++)
            {
                adjacency.offsets[i + 1] += adjacency.offsets[i];
            }
            
            // Fill edge data
            var currentOffsets = new uint[vertexCount + 1];
            Array.Copy(adjacency.offsets, currentOffsets, vertexCount + 1);
            
            for (uint i = 0; i < faceCount; i++)
            {
                uint a = indices[i * 3 + 0];
                uint b = indices[i * 3 + 1];
                uint c = indices[i * 3 + 2];
                
                if (remap != null)
                {
                    a = remap[a];
                    b = remap[b];
                    c = remap[c];
                }
                
                adjacency.data[currentOffsets[a]++] = new EdgeAdjacency.Edge { next = b, prev = c };
                adjacency.data[currentOffsets[b]++] = new EdgeAdjacency.Edge { next = c, prev = a };
                adjacency.data[currentOffsets[c]++] = new EdgeAdjacency.Edge { next = a, prev = b };
            }
        }

        private static void ClassifyVertices(VertexKind[] vertexKind, uint[] loop, uint[] loopback, uint vertexCount,
            EdgeAdjacency adjacency, uint[] remap, uint[] wedge, SimplifyOptions options)
        {
            // Initialize arrays
            for (uint i = 0; i < vertexCount; i++)
            {
                loop[i] = uint.MaxValue;
                loopback[i] = uint.MaxValue;
            }
            
            // Find open edges
            var openIncoming = loopback;
            var openOutgoing = loop;
            
            for (uint i = 0; i < vertexCount; i++)
            {
                uint vertex = i;
                uint count = adjacency.offsets[vertex + 1] - adjacency.offsets[vertex];
                
                for (uint j = 0; j < count; j++)
                {
                    var edge = adjacency.data[adjacency.offsets[vertex] + j];
                    uint target = edge.next;
                    
                    if (target == vertex)
                    {
                        openIncoming[vertex] = vertex;
                        openOutgoing[vertex] = vertex;
                    }
                    else if (!HasEdge(adjacency, target, vertex))
                    {
                        openIncoming[target] = openIncoming[target] == uint.MaxValue ? vertex : target;
                        openOutgoing[vertex] = openOutgoing[vertex] == uint.MaxValue ? target : vertex;
                    }
                }
            }
            
            // Classify vertices
            for (uint i = 0; i < vertexCount; i++)
            {
                if (remap[i] == i)
                {
                    if (wedge[i] == i)
                    {
                        uint openI = openIncoming[i];
                        uint openO = openOutgoing[i];
                        
                        if (openI == uint.MaxValue && openO == uint.MaxValue)
                        {
                            vertexKind[i] = VertexKind.Manifold;
                        }
                        else if (openI != i && openO != i)
                        {
                            vertexKind[i] = VertexKind.Border;
                        }
                        else
                        {
                            vertexKind[i] = VertexKind.Locked;
                        }
                    }
                    else
                    {
                        vertexKind[i] = VertexKind.Locked;
                    }
                }
                else
                {
                    vertexKind[i] = vertexKind[remap[i]];
                }
            }
            
            if ((options & SimplifyOptions.LockBorder) != 0)
            {
                for (uint i = 0; i < vertexCount; i++)
                {
                    if (vertexKind[i] == VertexKind.Border)
                        vertexKind[i] = VertexKind.Locked;
                }
            }
        }

        private static bool HasEdge(EdgeAdjacency adjacency, uint a, uint b)
        {
            uint count = adjacency.offsets[a + 1] - adjacency.offsets[a];
            
            for (uint i = 0; i < count; i++)
            {
                if (adjacency.data[adjacency.offsets[a] + i].next == b)
                    return true;
            }
            
            return false;
        }

        private static Vector3[] RescalePositions(Vector3[] positions)
        {
            var result = new Vector3[positions.Length];
            var min = new Vector3(float.MaxValue, float.MaxValue, float.MaxValue);
            var max = new Vector3(float.MinValue, float.MinValue, float.MinValue);
            
            foreach (var pos in positions)
            {
                if (pos.x < min.x) min.x = pos.x;
                if (pos.y < min.y) min.y = pos.y;
                if (pos.z < min.z) min.z = pos.z;
                if (pos.x > max.x) max.x = pos.x;
                if (pos.y > max.y) max.y = pos.y;
                if (pos.z > max.z) max.z = pos.z;
            }
            
            var extent = Math.Max(Math.Max(max.x - min.x, max.y - min.y), max.z - min.z);
            float scale = extent == 0 ? 0 : 1.0f / extent;
            
            for (int i = 0; i < positions.Length; i++)
            {
                result[i] = new Vector3(
                    (positions[i].x - min.x) * scale,
                    (positions[i].y - min.y) * scale,
                    (positions[i].z - min.z) * scale);
            }
            
            return result;
        }

        private static void FillFaceQuadrics(Quadric[] vertexQuadrics, uint[] indices, uint indexCount, Vector3[] positions, uint[] remap)
        {
            for (uint i = 0; i < indexCount; i += 3)
            {
                uint i0 = indices[i + 0];
                uint i1 = indices[i + 1];
                uint i2 = indices[i + 2];
                
                var q = QuadricFromTriangle(positions[i0], positions[i1], positions[i2], 1.0f);
                
                vertexQuadrics[remap[i0]] = vertexQuadrics[remap[i0]] + q;
                vertexQuadrics[remap[i1]] = vertexQuadrics[remap[i1]] + q;
                vertexQuadrics[remap[i2]] = vertexQuadrics[remap[i2]] + q;
            }
        }

        private static void FillEdgeQuadrics(Quadric[] vertexQuadrics, uint[] indices, uint indexCount, Vector3[] positions,
            uint[] remap, VertexKind[] vertexKind, uint[] loop, uint[] loopback)
        {
            for (uint i = 0; i < indexCount; i += 3)
            {
                for (int e = 0; e < 3; e++)
                {
                    uint i0 = indices[i + e];
                    uint i1 = indices[i + (e + 1) % 3];
                    
                    var k0 = vertexKind[i0];
                    var k1 = vertexKind[i1];
                    
                    if (k0 != VertexKind.Border && k0 != VertexKind.Seam &&
                        k1 != VertexKind.Border && k1 != VertexKind.Seam)
                        continue;
                    
                    if ((k0 == VertexKind.Border || k0 == VertexKind.Seam) && loop[i0] != i1)
                        continue;
                    
                    if ((k1 == VertexKind.Border || k1 == VertexKind.Seam) && loopback[i1] != i0)
                        continue;
                    
                    if (HasOpposite[(int)k0, (int)k1] && remap[i1] > remap[i0])
                        continue;
                    
                    uint i2 = indices[i + (e + 2) % 3];
                    
                    float edgeWeight = (k0 == VertexKind.Border || k1 == VertexKind.Border) ? 10.0f : 1.0f;
                    
                    var q = QuadricFromTriangleEdge(positions[i0], positions[i1], positions[i2], edgeWeight);
                    
                    vertexQuadrics[remap[i0]] = vertexQuadrics[remap[i0]] + q;
                    vertexQuadrics[remap[i1]] = vertexQuadrics[remap[i1]] + q;
                }
            }
        }

        private static Quadric QuadricFromTriangle(Vector3 p0, Vector3 p1, Vector3 p2, float weight)
        {
            var p10 = p1 - p0;
            var p20 = p2 - p0;
            
            var normal = Vector3.Cross(p10, p20);
            float area = normal.Length;
            if (area > 0) normal = normal * (1.0f / area);
            
            float distance = Vector3.Dot(normal, p0);
            float w = (float)Math.Sqrt(area) * weight;
            
            return QuadricFromPlane(normal.x, normal.y, normal.z, -distance, w);
        }

        private static Quadric QuadricFromTriangleEdge(Vector3 p0, Vector3 p1, Vector3 p2, float weight)
        {
            var p10 = p1 - p0;
            var p20 = p2 - p0;
            
            float lengthSq = Vector3.Dot(p10, p10);
            float length = (float)Math.Sqrt(lengthSq);
            
            float p20p = Vector3.Dot(p20, p10);
            var perp = p20 * lengthSq - p10 * p20p;
            
            float perpLen = perp.Length;
            if (perpLen > 0) perp = perp * (1.0f / perpLen);
            
            float distance = Vector3.Dot(perp, p0);
            
            return QuadricFromPlane(perp.x, perp.y, perp.z, -distance, length * weight);
        }

        private static Quadric QuadricFromPlane(float a, float b, float c, float d, float w)
        {
            float aw = a * w;
            float bw = b * w;
            float cw = c * w;
            float dw = d * w;
            
            return new Quadric
            {
                a00 = a * aw,
                a11 = b * bw,
                a22 = c * cw,
                a10 = a * bw,
                a20 = a * cw,
                a21 = b * cw,
                b0 = a * dw,
                b1 = b * dw,
                b2 = c * dw,
                c = d * dw,
                w = w
            };
        }

        private static float QuadricError(Quadric q, Vector3 v)
        {
            float rx = q.b0 + q.a10 * v.y + q.a20 * v.z;
            float ry = q.b1 + q.a21 * v.z;
            float rz = q.b2;
            
            rx = 2 * rx + q.a00 * v.x;
            ry = 2 * ry + q.a11 * v.y;
            rz = 2 * rz + q.a22 * v.z;
            
            float r = q.c + rx * v.x + ry * v.y + rz * v.z;
            float s = q.w == 0 ? 0 : 1.0f / q.w;
            
            return Math.Abs(r) * s;
        }

        private static List<Collapse> PickEdgeCollapses(uint[] indices, uint indexCount, uint[] remap,
            VertexKind[] vertexKind, uint[] loop, uint[] loopback)
        {
            var collapses = new List<Collapse>();
            
            for (uint i = 0; i < indexCount; i += 3)
            {
                for (int e = 0; e < 3; e++)
                {
                    uint i0 = indices[i + e];
                    uint i1 = indices[i + (e + 1) % 3];
                    
                    if (remap[i0] == remap[i1]) continue;
                    
                    var k0 = vertexKind[i0];
                    var k1 = vertexKind[i1];
                    
                    if (!CanCollapse[(int)k0, (int)k1] && !CanCollapse[(int)k1, (int)k0])
                        continue;
                    
                    if (HasOpposite[(int)k0, (int)k1] && remap[i1] > remap[i0])
                        continue;
                    
                    if (k0 == k1 && (k0 == VertexKind.Border || k0 == VertexKind.Seam) && loop[i0] != i1)
                        continue;
                    
                    if (k0 == VertexKind.Locked || k1 == VertexKind.Locked)
                    {
                        if ((k0 == VertexKind.Border || k0 == VertexKind.Seam) && loop[i0] != i1)
                            continue;
                        if ((k1 == VertexKind.Border || k1 == VertexKind.Seam) && loopback[i1] != i0)
                            continue;
                    }
                    
                    bool bidi = CanCollapse[(int)k0, (int)k1] && CanCollapse[(int)k1, (int)k0];
                    
                    var collapse = new Collapse
                    {
                        v0 = bidi ? (i0 < i1 ? i0 : i1) : (CanCollapse[(int)k0, (int)k1] ? i0 : i1),
                        v1 = bidi ? (i0 < i1 ? i1 : i0) : (CanCollapse[(int)k0, (int)k1] ? i1 : i0),
                        bidi = bidi
                    };
                    
                    collapses.Add(collapse);
                }
            }
            
            return collapses;
        }

        private static void RankEdgeCollapses(List<Collapse> collapses, Vector3[] positions, Quadric[] vertexQuadrics,
            uint[] remap, uint[] wedge, VertexKind[] vertexKind, uint[] loop, uint[] loopback)
        {
            for (int i = 0; i < collapses.Count; i++)
            {
                var collapse = collapses[i];
                
                uint i0 = collapse.v0;
                uint i1 = collapse.v1;
                uint j0 = collapse.bidi ? i1 : i0;
                uint j1 = collapse.bidi ? i0 : i1;
                
                float ei = QuadricError(vertexQuadrics[remap[i0]], positions[i1]);
                float ej = collapse.bidi ? QuadricError(vertexQuadrics[remap[j0]], positions[j1]) : float.MaxValue;
                
                collapse.v0 = ei <= ej ? i0 : j0;
                collapse.v1 = ei <= ej ? i1 : j1;
                collapse.error = ei <= ej ? ei : ej;
                
                collapses[i] = collapse;
            }
        }

        private static uint PerformEdgeCollapses(uint[] collapseRemap, bool[] collapseLocked, List<Collapse> collapses,
            uint[] remap, uint[] wedge, VertexKind[] vertexKind, uint[] loop, uint[] loopback,
            Vector3[] positions, EdgeAdjacency adjacency, uint triangleCollapseGoal, float errorLimit, ref float resultError)
        {
            uint edgeCollapses = 0;
            uint triangleCollapses = 0;
            
            foreach (var collapse in collapses)
            {
                if (collapse.error > errorLimit) break;
                if (triangleCollapses >= triangleCollapseGoal) break;
                
                uint i0 = collapse.v0;
                uint i1 = collapse.v1;
                uint r0 = remap[i0];
                uint r1 = remap[i1];
                
                if (collapseLocked[r0] || collapseLocked[r1]) continue;
                
                // Simplified triangle flip check - you might want to implement the full version
                if (HasTriangleFlips(adjacency, positions, collapseRemap, r0, r1)) continue;
                
                var kind = vertexKind[i0];
                
                if (kind == VertexKind.Complex)
                {
                    uint v = i0;
                    do
                    {
                        collapseRemap[v] = i1;
                        v = wedge[v];
                    } while (v != i0);
                }
                else
                {
                    collapseRemap[i0] = i1;
                }
                
                collapseLocked[r0] = true;
                collapseLocked[r1] = true;
                
                triangleCollapses += kind == VertexKind.Border ? 1u : 2u;
                edgeCollapses++;
                
                if (collapse.error > resultError)
                    resultError = collapse.error;
            }
            
            return edgeCollapses;
        }

        private static bool HasTriangleFlips(EdgeAdjacency adjacency, Vector3[] positions, uint[] collapseRemap, uint i0, uint i1)
        {
            // Simplified triangle flip detection - implement full version for better quality
            return false;
        }

        private static void UpdateQuadrics(uint[] collapseRemap, uint vertexCount, Quadric[] vertexQuadrics, uint[] remap)
        {
            for (uint i = 0; i < vertexCount; i++)
            {
                if (collapseRemap[i] == i) continue;
                
                uint i0 = i;
                uint i1 = collapseRemap[i];
                uint r0 = remap[i0];
                uint r1 = remap[i1];
                
                if (i0 == r0)
                    vertexQuadrics[r1] = vertexQuadrics[r1] + vertexQuadrics[r0];
            }
        }

        private static void RemapEdgeLoops(uint[] loop, uint vertexCount, uint[] collapseRemap)
        {
            for (uint i = 0; i < vertexCount; i++)
            {
                if (loop[i] != uint.MaxValue)
                {
                    uint l = loop[i];
                    uint r = collapseRemap[l];
                    
                    if (i == r)
                        loop[i] = (loop[l] != uint.MaxValue) ? collapseRemap[loop[l]] : uint.MaxValue;
                    else
                        loop[i] = r;
                }
            }
        }

        private static uint RemapIndexBuffer(uint[] indices, uint indexCount, uint[] collapseRemap)
        {
            uint write = 0;
            
            for (uint i = 0; i < indexCount; i += 3)
            {
                uint v0 = collapseRemap[indices[i + 0]];
                uint v1 = collapseRemap[indices[i + 1]];
                uint v2 = collapseRemap[indices[i + 2]];
                
                if (v0 != v1 && v0 != v2 && v1 != v2)
                {
                    indices[write + 0] = v0;
                    indices[write + 1] = v1;
                    indices[write + 2] = v2;
                    write += 3;
                }
            }
            
            return write;
        }

        private static uint FilterIndexBuffer(uint[] indices, uint indexCount, uint[] remap)
        {
            uint write = 0;
            
            for (uint i = 0; i < indexCount; i += 3)
            {
                uint v0 = indices[i + 0];
                uint v1 = indices[i + 1];
                uint v2 = indices[i + 2];
                
                uint r0 = remap[v0];
                uint r1 = remap[v1];
                uint r2 = remap[v2];
                
                if (r0 != r1 && r0 != r2 && r1 != r2)
                {
                    indices[write + 0] = v0;
                    indices[write + 1] = v1;
                    indices[write + 2] = v2;
                    write += 3;
                }
            }
            
            return write;
        }
    }

    // Extension methods to convert from s&box types
    public static class MeshSimplifierExtensions
    {
        public static MeshSimplifier.Vector3 ToMeshOptVector3(this global::Vector3 v)
        {
            return new MeshSimplifier.Vector3(v.x, v.y, v.z);
        }

        public static global::Vector3 ToSBoxVector3(this MeshSimplifier.Vector3 v)
        {
            return new global::Vector3(v.x, v.y, v.z);
        }

        public static MeshSimplifier.Vector3[] ToMeshOptVector3Array(this global::Vector3[] vertices)
        {
            var result = new MeshSimplifier.Vector3[vertices.Length];
            for (int i = 0; i < vertices.Length; i++)
            {
                result[i] = vertices[i].ToMeshOptVector3();
            }
            return result;
        }

        public static global::Vector3[] ToSBoxVector3Array(this MeshSimplifier.Vector3[] vertices)
        {
            var result = new global::Vector3[vertices.Length];
            for (int i = 0; i < vertices.Length; i++)
            {
                result[i] = vertices[i].ToSBoxVector3();
            }
            return result;
        }
    }
}
