namespace CAB402
  
open Graph
open CreateLayers

module Coordinates =
    open Flips
    open Flips.Types

    // These solver settings are provided for you (don't change)
    let settings : SolverSettings = 
        { 
            SolverType = SolverType.CBC; 
            MaxDuration = 10_000L; 
            WriteLPFile= Some "test.lp"; // this setting will cause the generated Linear programming model to be exported to a 
                                         // text file which can be helpful for debugging construction of your model
            WriteMPSFile=None 
        }

    // This function is not used for the Sugiyama Layout, 
    // it is provided merely to give you an example of how to construct and solve a linear programming model using Flips
    // See also:
    //      https://flipslibrary.com/#/#intro-problem
   
    let Flips_Example () =
        // Create Flips variables
        let a = Decision.createContinuous "a" 0 infinity
        let b = Decision.createContinuous "b" 0 infinity

        // Objective is a + 2 b
        let objective_expression = a + 2.0 * b
        
        // Create some constraints 
        // note that >== is used for >= and <== is use for <=
        let constraints = 
            [
                Constraint.create "rule1" (a >== b + 1.0);
                Constraint.create "rule2" (a + 2.0 * b <== 100.0)
            ]

        // We want to minimize the objective expression
        let objective = Objective.create "objective" Minimize objective_expression
        let model =
            Model.create objective
            |> Model.addConstraints constraints
        
        // Try to solve the problem optimally (this step make take some time if the problem is large or complex)
        let result = Solver.solve settings model

        // Check to see if the model could be solved optimally
        match result with
        | Optimal solution ->
            // Extract the optimal values of the decision variables ...    
            printfn ""
            printfn $"Optimal objective is {Objective.evaluate solution objective}"
            printfn $"  value of variable a is {solution.DecisionResults[a]}"
            printfn $"  value of variable b is {solution.DecisionResults[b]}"
        | Infeasible msg -> failwith "Error: Constraints are not feasible {msg}"
        | Unbounded  msg -> failwith "Error: Objective is not bounded {msg}"
        | Unknown    msg -> failwith "Error: Failed {msg}"
        

    // Create a new Decision variable to represent the x coordinate of each of the nodes in the graph.
    // Each of  these variables will be Continuous (i.e real valued variable) 
    // with a lower bound of 0 and an upper bound of infinity
    // The name of the variable for node 3 will be "x3"
    let CreateXCoordinateVariables (hierarchy: Hierarchy) : Decision list =

        let rec createDecisionList (newList: Decision List) (nodeList: NodeId list) : Decision List = 
            match nodeList with
            | [] -> newList
            | head::tail -> let theString : string = "x" + string head
                            let decVar = Decision.createContinuous theString 0 infinity
                            createDecisionList (decVar::newList) (tail)
            
        // Main
        let createCoordinateList (hierarchy: Hierarchy) : Decision List = 
            hierarchy.layers
            |> List.concat
            |> List.sort
            |> createDecisionList (List.empty: Decision list)

        createCoordinateList (hierarchy)

    // For the optimization we wish to minimize the sum of the absolute value of difference in x coordinate of each each.
    // Unfortunately, expressions involving absolute value calculations are not "Linear" expressions.
    // We can however use a commonly used "trick" to convert an absolute value expression into a linear expression:
    // To do so, we need to introduce a new variable representing the absolute value expression.
    // Let's call this new variable "a". If we wish to represent |x - y| then we simply add two constraints:
    // x - y <= a and y - x <= a. Regardless of whether x < y or y <=x, we know |x - y| <= a,
    // so when we apply optimization and try to find the minimum value of a, it will be |x - y|

    // Create a new Decision variable to represent |x_src - x_dst| for each edge from src to dst in the graph.
    // Each of  these variables will be Continuous (i.e real valued variable) 
    // with a lower bound of 0 and an upper bound of infinity
    // The name of the variable for edge (src,dst) will be "|xsrc-xdst|"
    // We store these variables in a map, indexed by (src,dst) so we can conviently loopup the variable associated with any edge
    let CreateAbsoluteValueVariables (hierarchy: Hierarchy) : Map<NodeId*NodeId, Decision> =
        
        let rec CreateMap (theMap: Map<NodeId*NodeId, Decision>) (edgeList: Edge List) : Map<NodeId*NodeId, Decision> =
            match edgeList with
            | [] -> theMap
            | head::tail -> let theString : string = "|x" + string (fst(head)) + "-x" + string (snd(head)) + "|"
                            let decVar = Decision.createContinuous theString 0 infinity
                            CreateMap (theMap.Add (head, decVar)) (tail)

        let createAbsoluteValues (hierarchy: Hierarchy) : Map<NodeId*NodeId, Decision> =
            hierarchy.edges
            |> Set.toList
            |> CreateMap (Map.empty: Map<NodeId*NodeId, Decision>)
            
        createAbsoluteValues (hierarchy)

    // The Objective expression will be the sum of the absolute values for all edges
    let CreateObjective (absolute_value_variables:Map<NodeId*NodeId, Decision>) : LinearExpression =

        let rec sumDecisionValues (objectiveExpression :LinearExpression) (decisionList: Decision list) : LinearExpression = 
            
            match decisionList with 
            | [] -> objectiveExpression
            | head::tail -> sumDecisionValues (objectiveExpression + head) (tail)

        absolute_value_variables
        |> Map.values // returns a sequence of teh values (Decisions)
        |> Seq.toList
        |> sumDecisionValues (LinearExpression.Zero)


    // Create two absolute value constraints for the edge from src to dst:
    //     x_src - x_dst <= |x_src - x_dst|
    //     x_dst - x_src <= |x_src - x_dst|
    let CreateAbsoluteValueContraints ((src,dst):NodeId*NodeId) (x_coordinate_variables: Decision list) (absolute_value_variables:Map<NodeId*NodeId, Decision>) : Constraint seq =

        let name1 : string = "rule" + string src + "-" + string dst
        let name2 : string = "rule" + string dst + "-" + string src
        let absValue = Map.find (src,dst) absolute_value_variables
        let srcVal = x_coordinate_variables[src]
        let dstVal = x_coordinate_variables[dst]
        let constraints = 
            [
                Constraint.create name1 (srcVal - dstVal <== absValue)
                Constraint.create name2 (dstVal - srcVal <== absValue)
            ]
        constraints
        |> List.toSeq
               

    // Create absolute value constraints for all edges in the hierarchy
    let CreateAllAbsoluteValueContraints (hierarchy: Hierarchy) (x_coordinate_variables: Decision list) (absolute_value_variables:Map<NodeId*NodeId, Decision>) : Constraint seq =

        hierarchy.edges
        |> Set.toSeq
        |> Seq.map (fun elem -> CreateAbsoluteValueContraints (elem) (x_coordinate_variables) (absolute_value_variables) )
        |> Seq.concat


    // Here we assume that second node appears immediately after first node in the same layer
    // In order to ensure that the second node is positioned to the right of the first node we add a constraint on their x coordinates:
    //    x_second - x_first >= 1
    let CreateOrderWithinLayerConstraint (first:NodeId) (second:NodeId) (x_coordinate_variables: Decision list) : Constraint =

        let constraintName : string = "ruleTwo-" + string (first) + string (second)
        Constraint.create constraintName (x_coordinate_variables[second] - x_coordinate_variables[first] >== 1.0)


    // Create Ordering constraints between all successive nodes in each layer
    let CreateOrderWithinLayerConstraints (hierarchy: Hierarchy) (x_coordinate_variables: Decision list) : Constraint seq =

        let rec mapper (newList: Constraint list) (nodeList: NodeId list) : Constraint seq = 
            match nodeList with
            | [] | [_] -> newList |> List.toSeq
            | head::tail -> let newCon : Constraint = CreateOrderWithinLayerConstraint (head) (tail[0]) (x_coordinate_variables)
                            mapper (newCon::newList) (tail)
            
        hierarchy.layers
        |> List.toSeq
        |> Seq.map (fun y -> mapper (List.empty: Constraint list) (y))
        |> Seq.concat


    // Use the Flips Linear programming system to build a model to compute the optimal x coordinates for each node in the graph.
    // We need to first create variables to represent the x_coordinates of each node and variables to represent the absolute
    // difference in x coordinates for each edge. We then use those variable to create ordering constraints, absolute value
    // constraints and an optimization objective. All of these are combined to create a Flips Model.
    // We don't actually try to solve that model in this function, just construct it.
    // See example above for how to build and solve a Flips linear programming model
    let CreateLinearProgrammingModel (x_coordinate_variables: Decision list) (hierarchy: Hierarchy) : Model.Model =

        let absValueVariables :Map<NodeId*NodeId, Decision> = CreateAbsoluteValueVariables (hierarchy)

        let objective = Objective.create "Minimization" Minimize (CreateObjective (absValueVariables))

        let absValueConstraints = CreateAllAbsoluteValueContraints (hierarchy) (x_coordinate_variables) (absValueVariables)

        let orderWithinConstraints = CreateOrderWithinLayerConstraints (hierarchy) (x_coordinate_variables)

        let model = Model.create objective
                    |> Model.addConstraints absValueConstraints
                    |> Model.addConstraints orderWithinConstraints
        
        model
        


    // Given a solution to the Linear programming model created above, 
    // extract the optimal floating point value for each x coordinate variable
    // The above Linear programming model should always be feasible, 
    // but if an Optimal solution cannot be found then this function would raise an exception
    let ExtractXCoordinates (x_coordinate_variables:Decision list) (optimization_result: SolveResult) : float list =

        match optimization_result with
        | Optimal solution -> x_coordinate_variables
                              |> List.map (fun y -> solution.DecisionResults[y])
        | Infeasible msg -> failwith "Error: Constraints are not feasible {msg}"
        | Unbounded  msg -> failwith "Error: Objective is not bounded {msg}"
        | Unknown    msg -> failwith "Error: Failed {msg}"
        

    // The x coordinate for each node is computed using the Flips linear programming system (see above)
    // The y coordinate of each node is simply the level that it belongs to (converted to a floating point number)
    // The result is a list of (x, y) coordinates for each of the nodes in the graph
    let DetermineCoordinates (hierarchy: Hierarchy) : (float*float) list =

        // create the xCoordinate variables
        let xCoordVariables = CreateXCoordinateVariables (hierarchy)

        // this will return the layer of the nodeId
        let checkLayer (node: NodeId) : float = 
            Level (node) (hierarchy.layers)

        let yList : float list = hierarchy.layers
                                 |> List.concat
                                 |> List.sort
                                 |> List.map (fun y -> checkLayer (y)) 


        let xList : float list = CreateLinearProgrammingModel (xCoordVariables) (hierarchy)
                                 |> Solver.solve settings
                                 |> ExtractXCoordinates (xCoordVariables) 
        

        List.zip (xList) (yList)

