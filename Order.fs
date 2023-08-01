namespace CAB402
 
open PerfectOrder
open BaryCentricOrder

module Order =

    let THRESHOLD = 8

    // If the number of nodes in the variable level is less than THRESHOLD then
    // we will use the Perfect Crossing Minimization approach,
    // otherwise we will resort to the Bary Centric Crossing Minimization approach.
    let OneSidedCrossingMinimization (hierarchy:Hierarchy)  ((fixed_level, variable_level):int*int) : Hierarchy =

        if (List.length hierarchy.layers[variable_level] < THRESHOLD)
        then PerfectCrossingMinimization (hierarchy) ((fixed_level, variable_level))
        else BaryCentricCrossingMinimization (hierarchy) (variable_level)

    // Apply one iteration of the Down Up procedure.
    // We start with level 0 as our fixed layer and use it to compute a new ordering of the nodes in level 1,
    // we then use that new ordering of level 1 to compute a new ordering of the nodes at level 2,
    // and so on until we reach the bottom layer.
    // We then work our way back up, starting with the bottom layer as our fixed layer 
    // and the second bottom layer as the variable layer.
    // We continue up until we have computed a new  ordering of the top layer.
    // The result is a new hierarchy that incorporates all of the reorderings made to each layer throughout the process.
    let OneIterationDownAndUp (hierarchy:Hierarchy) : Hierarchy = 

        let rec oneUp (x: int) (hierarchy: Hierarchy) : Hierarchy = 
            match x + 1 with
            | y when (y = List.length hierarchy.layers) -> hierarchy
            | _ -> OneSidedCrossingMinimization (hierarchy) (x, x+1) |> oneUp (x+1)

        let rec oneDown (x: int) (hierarchy: Hierarchy) : Hierarchy = 
            match x with 
            | y when (y = 0) -> hierarchy
            | _ -> OneSidedCrossingMinimization (hierarchy) (x, x-1) |> oneDown (x-1)
            
        hierarchy
        |> oneUp (0)
        |> oneDown (List.length hierarchy.layers - 1)

       

    let MAX_ITERATIONS = 10

    // We repeat the Down Up procedure until either there is no change to the hierarchy 
    // or we have exceeded the specified maximum number of iterations
    let OrderLayers (hierarchy: Hierarchy) : Hierarchy =

        let GetOrderedLayers(hierarchy:Hierarchy) : Hierarchy = 

            let rec Order (counter: int) (hierarchy: Hierarchy) : Hierarchy = 
                if (counter <= MAX_ITERATIONS) 
                then hierarchy
                     |> OneIterationDownAndUp
                     |> (fun y -> if (y = hierarchy) then hierarchy else Order (counter + 1) (y))                
                else hierarchy
                   
            hierarchy
            |> Order (0)

        hierarchy
        |> GetOrderedLayers