namespace CAB402
   
open Graph

module CreateLayers =
    
    // Return the list of all nodes that can be placed in the next layer given a list of nodes that 
    // have already been placed in previous layers and the list of nodes that remain to be placed in a layer.
    // A node can only be placed in the next layer if all of its predecessors have already been placed.
    let FindNextLayer (edges: Edges) (previous_layers: NodeId list) (remaining_nodes: NodeId list) : Layer =

        // Filter remaining nodes by those whose predecessors are contained within previous layers
        remaining_nodes
        |> List.filter (fun x -> Set.forall (fun y -> List.contains y previous_layers) (Predecessors (x) (edges)))
        

    // Take all nodes and place them into layers in a greedy manner such that each node is placed into the earliest possible layer
    // The algorithm starts by determining which nodes can be placed in the first/top layer, then determining which nodes can be 
    // placed in the second layer and so on until all nodes are place in a layer. The result is a list of layers. 
    let ComputeLayers (edges:Edges) : Layers =
        
        // A recursive function to get the layers
        let GetLayers (nodeList : NodeId list) : Layers = 

            let rec Get (newLayers: Layers) (prevNodes: NodeId list) (remNodes: NodeId list) : Layers = 
                match FindNextLayer (edges) (prevNodes) (remNodes) with
                | [] -> newLayers
                | newList -> List.filter (fun y -> if (List.contains y newList) then false else true) remNodes
                             |> Get (newLayers@[newList]) (newList@prevNodes)

            Get (list.Empty) (list.Empty) (nodeList)

        [0..LastNodeId(edges)]
        |> GetLayers
        

    // The level of a node is the number of layer that it belongs to.
    // If a node is in the first/top layer then it will have a level of 0
    let Level (nodeId:NodeId) (layers:Layers): int =

        // Find index of the nodeId level
        layers
        |> List.findIndex (fun y -> List.contains nodeId y)

    // The position of a node within a layer is a number representing where the node exists
    // If a node is the first node in the layer then it will have a position of 0
    let Position (nodeId:NodeId) (layer:Layer): int =

        // Find index of the nodeId position
        layer
        |> List.findIndex (fun y -> y = nodeId)


