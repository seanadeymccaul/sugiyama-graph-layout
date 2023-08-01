namespace CAB402
  
open Graph
open CreateLayers

module DummyNodes =

    // Add a new node to the start of the list of nodes in the layer with the specified level
    // Note: we don't actually alter the existing layers, but create a new list of layers with the require addition
    // For example AddNodeToLayer [[0;1];[2;3;4];[5]] (1,6) should return [[0;1];[6;2;3;4];[5]]
    let AddNodeToLayer (layers:Layers) ((level,nodeId):int*NodeId) : Layers =

        layers
        |> List.mapi (fun i x -> if (i = level) then nodeId::x else x)
            
    
    // Here we assume that the edge from src to dst is a "long" edge, meaning it spans more than 2 levels
    // We therefore need to replace that edge by introducing new dummy nodes in each of the intervening levels 
    // and creating a sequence of edges between these dummy nodes such that each new edge goes from one level 
    // to the next. The new dummy nodes need to be added to those layers.
    // The result is a new hierarch will a new set of edges and a new list of layers
    // For example, if there is a long edge from node 1 in level 0 to node 5 in level 2 then
    // we need to introduce a new dummy nodes (say node 6), add it to level 1 and 
    // create new edges from 1 to 6 and from 6 to 5 to replace the existing long edge from 1 to 5
    // The new dummy node Id should be the next available unused node Id.
    // So if the nodes in the graph are currently numbered 0, 1, 2, 3, 4, 5 and two new dummy nodes are required,
    // then their nodes ids should be 6 and 7.
    let ReplaceLongEdge (hierarchy:Hierarchy) ((src,dst): Edge) : Hierarchy =

        // A function to add new edges and nodes to the hierarchy recursively
        let AddNewEdges (newHierarchy: Hierarchy) : Hierarchy = 

            let rec Add (endLayer: int) (currentLayer: int) (currentNode: NodeId) (nextNode: NodeId) (newHierarchy: Hierarchy) : Hierarchy = 
                match currentLayer with
                | x when x = endLayer - 1 -> newHierarchy.edges
                                             |> Set.add (currentNode,dst)
                                             |> (fun x -> {edges=x;layers=newHierarchy.layers})
                | _ -> newHierarchy.edges
                       |> Set.add (currentNode,nextNode)
                       |> (fun x -> AddNodeToLayer (newHierarchy.layers) (currentLayer+1,nextNode)
                                    |> (fun y -> {edges=x;layers=y})) |> Add (endLayer) (currentLayer+1) (nextNode) (nextNode+1)

            newHierarchy
            |> Add (Level (dst) (hierarchy.layers)) (Level (src) (hierarchy.layers)) (src) (LastNodeId(hierarchy.edges)+1)

        hierarchy.edges
        |> Set.remove (src,dst)                                         // remove the edge and repack into hierarchy
        |> (fun x -> {edges=x;layers=hierarchy.layers})
        |> AddNewEdges                                                  // add the new edges by using recursion
       

    // Find all "long" edges and replace them by a sequence of shorter edges between successive layers.
    // An edge is long if the level of the source and destination nodes differ by more than one.
    // The result is a new hierarchy with all long edges removed and replaced by shorter edges (with corresponding new dummy nodes)
    let AddDummyNodes (hierarchy:Hierarchy): Hierarchy =

        // Function to search recursively for long edges and apply the dummy nodes function
        let SearchEdges (hierarchy: Hierarchy) : Hierarchy = 

            let rec Search (newH:Hierarchy) (newEdgeList:Edge list): Hierarchy = 
                match newEdgeList with 
                | [] -> newH
                | (x,y)::tail -> if (Level (x) (newH.layers) - Level (y) (newH.layers)) = 1 then Search (newH) (tail)
                                 else Search (ReplaceLongEdge (newH) (x,y)) (tail)
            
            hierarchy.edges
            |> Set.toList
            |> Search (hierarchy)

        hierarchy
        |> SearchEdges
