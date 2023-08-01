namespace CAB402
 
open Graph
open CreateLayers

module BaryCentricOrder =

    // The computed BaryCentre for the specifed node will be the average 
    // position of all nodes in the next and previous layers that it has an edge to.
    // Here the position is not an actual pixel coordinate, 
    // but just the relative position of those nodes within their layer.
    // For example, if the node has edges to the first and third node in the 
    // previous layer (positions 0 and 2) and to the fourth node in the next layer (position 3)
    // then the BaryCentre will be the average of 0, 2 and 3 (approximately 1.66)
    let BaryCentre (hierarchy:Hierarchy) (nodeId:NodeId) : float =

        let GetEdges (edgeList: Edge list) : NodeId list = 

            let rec Get (newList: NodeId list) (edgeList: Edge list) : NodeId list = 
                match edgeList with
                | [] -> newList
                | (x,y)::tail -> if (x = nodeId) then (Get (y::newList) (tail))
                                 else if (y = nodeId) then (Get (x::newList) (tail))
                                      else Get (newList) (tail)
            
            edgeList
            |> Get (list.Empty) 

        let FindEdgePositions (nodeList: NodeId list) : float list = 
            List.map (fun y -> Position (y) (hierarchy.layers[Level (y) (hierarchy.layers)])) nodeList

        hierarchy.edges
        |> Set.toList
        |> GetEdges                         // Get a list of the edges of relevant nodes
        |> FindEdgePositions                // Map to the barry centric values (float)
        |> List.average


    // Change the order of the nodes in the variable level layer based on the computed BaryCentres of those nodes
    // The result will be a new hierarchy which will be the same as the original hierarchy except that the order
    // of the nodes in the variable level may be changed.
    let BaryCentricCrossingMinimization (hierarchy:Hierarchy) (variable_level:int) : Hierarchy =
        
        // Function to create a new hierarchy around the sorted bary centres
        let ReplaceHierarchy (layer: Layer) : Hierarchy = 
            {edges= hierarchy.edges; layers = List.map (fun y -> if y = hierarchy.layers[variable_level] then layer else y) hierarchy.layers}

        hierarchy.layers[variable_level]
        |> List.sortBy (fun y -> BaryCentre (hierarchy) (y))            // Sort by Bary Centres
        |> ReplaceHierarchy                                             // Creates new hierarchy
    
        