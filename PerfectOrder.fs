namespace CAB402

module PerfectOrder =

    // Given a list of values, return a sequence of all possible permuations of those values
    // For example Permutations ["a";"b";"c"] would return a sequence of 6 possible permutations:
    // ["a";"b";"c"], ["a";"c";"b"], ["b";"a";"c"], ["b";"c";"a"], ["c";"a";"b"] and ["c";"b";"a"]
    // If there are n items in the list, then there will be n factorial (n!) possible permuations
    // This function is generic meaning that it should work for any list element type 'T

    let Permutations (list: 'T list) : 'T list seq = 
    
        // Source from https://stackoverflow.com/questions/286427/calculating-permutations-in-f
        let Permutations (list: 'T list) : 'T list seq =
            let rec permutations list taken = 
                seq { if Set.count taken = List.length list then yield [] else
                      for l in list do                                              // for each head in list if set does contain then
                        if not (Set.contains l taken) then 
                            for perm in permutations list (Set.add l taken)  do
                                yield l::perm }
            permutations list Set.empty
 
        list
        |> Permutations

    // Determine whether the edge from src1 to dst1 would cross the edge from src2 to dst2 if the 
    // nodes are ordered as specified in the previous and next layers (where the source 
    // nodes belong to the previous layer and the destination nodes belong to the next layer.
    // The edges will cross if either src1 is ordered before src2 in the previous layer but
    // dst1 is ordered after dst2 in the next layer, or vice versa
    let EdgesCross (prev_layer:Layer) (next_layer:Layer) ((src1,dst1):Edge) ((src2,dst2):Edge) : bool =
        
        if (List.contains src1 prev_layer && List.contains src2 prev_layer) && (List.contains dst1 next_layer && List.contains dst2 next_layer) 
        then let src1Index : int = List.findIndex (fun y -> if y = src1 then true else false) prev_layer
             let src2Index : int = List.findIndex (fun y -> if y = src2 then true else false) prev_layer
             let dst1Index : int = List.findIndex (fun y -> if y = dst1 then true else false) next_layer
             let dst2Index : int = List.findIndex (fun y -> if y = dst2 then true else false) next_layer

             let decideFun : bool = 
                 if ((src1Index < src2Index) && (dst2Index < dst1Index)) || ((src1Index > src2Index) && (dst2Index > dst1Index)) then true else false
        
             decideFun
        else false


    // Count the total number of pairs edges from the previous layer to the next layer that will 
    // cross each other, given the specified ordering of nodes in the previous layer and next layer.
    let EdgeCrossings (edges:Edges) (prev_layer:Layer) (next_layer:Layer): int =

        // A function to calculate the amount of crossings each edge has
        let rec individualEdgeCrossings (edge:Edge) (edgesRec:Edge list) (x:int): int = 

            match edgesRec with
            | [] -> x
            | head::tail -> if (EdgesCross (prev_layer) (next_layer) (edge) (head)) 
                            then (individualEdgeCrossings (edge) (tail) (x+1))
                            else (individualEdgeCrossings (edge) (tail) (x))

        edges
        |> Set.toList
        |> List.map (fun x -> (individualEdgeCrossings (x) (Set.toList edges) (0)))              // map each set element to its individual int results
        |> List.fold (fun acc elem -> acc + elem) 0                                              // fold the set
        |> (fun x -> x/2)                                                                        // divide by 2



    // Consider every possible permuation of the nodes in the variable layer and choose the permutation
    // that leads to the least edge crossings from the fixed_level to the variable level.
    // The fixed level will either be the level immediately above the variable level or the level immediately below.
    // The result will be a new hierarchy which will be the same as the original hierarchy except that the order
    // of the nodes in the variable level may be changed.

    // Forgot to do both ways
    let PerfectCrossingMinimization (hierarchy:Hierarchy) ((fixed_level, variable_level):int*int) : Hierarchy =

        // Function to create a hierarchy with new layers around the returned variable layer
        let CreateNewLayers (variableLayer: NodeId list) : Hierarchy = 

            let rec Create (layers: Layers) (newLayers: Layers) (variableLayer: NodeId list) : Hierarchy = 
                match layers with
                | [] -> {edges= hierarchy.edges; layers= newLayers}
                | head::tail -> if head = hierarchy.layers[variable_level] then Create (tail) (newLayers@[variableLayer]) (variableLayer)
                                else Create (tail) (newLayers@[head]) (variableLayer)
            
            variableLayer
            |> Create (hierarchy.layers) (List.empty: Layers)

        hierarchy.layers[variable_level]
        |> Permutations
        |> (fun x -> if (fixed_level < variable_level)              // depends which layer apply to which
                     then x |> Seq.minBy (fun y -> EdgeCrossings (hierarchy.edges) (hierarchy.layers[fixed_level]) (y))    
                            |> CreateNewLayers 
                     else x |> Seq.minBy (fun y -> EdgeCrossings (hierarchy.edges) (y) (hierarchy.layers[fixed_level]))    
                            |> CreateNewLayers)
