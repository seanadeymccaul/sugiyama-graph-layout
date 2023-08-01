namespace CAB402

module Graph =

    // Given a list of edges, we can determine the number of nodes in the graph by finding the node with the largest nodeId
    let LastNodeId (edges: Edges) : int =
        
        edges
        |> Set.toList                           
        |> List.map (fun (a,b) -> [a; b])       // map each tuple to a list
        |> List.concat                          // concat the created list
        |> List.max

    // Find all nodes that have an edge to the specified node
    let Predecessors (node:NodeId) (edges: Edges) : NodeId Set =

        edges 
        |> Set.map (fun (a,b) -> if b = node then a else node)      // find each edge with node as b and return a
        |> Set.remove node                                          // remove node from the results


    // Find all nodes that have an edge from the specified node
    let Successors (node:NodeId) (edges: Edges) : NodeId Set =

        edges 
        |> Set.map (fun (a,b) -> if a = node then b else node)      // find each edge with node as a and return b
        |> Set.remove node                                          // remove node from results
