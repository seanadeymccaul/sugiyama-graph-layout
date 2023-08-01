namespace CAB402

// Each node in a graph is represented by an integer Id 
// Nodes are numbered from 0 to N-1, where N is the number of nodes in the graph
type NodeId = int

// Edges are "Directed" and represented by a pair 
// where the first is the source node and the second is the destination node
type Edge = NodeId * NodeId

// A Directed Graph is represented by a set of edges
// Every node is assumed to have at least one edge
type Edges = Edge Set

// To create a Sugiyama Layout of a Directed Acyclic Graph (DAG), we need to arrange nodes into layers
// A single layer is an ordered list of nodes
type Layer = NodeId list
    
// The entire graph is layed out based on a list of layers (where the first layer is the top layer)
type Layers = Layer list

// A hierarchy consists of both the edges of the graph and the layers that will be used for laying out the nodes
type Hierarchy = 
    { edges: Edges; layers: Layers }