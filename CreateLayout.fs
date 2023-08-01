namespace CAB402

open CreateLayers
open DummyNodes
open Order
open PerfectOrder
open Coordinates
open SVG

module CreateLayout = 
    
    // Here we apply the entire sequence of operations: 
    // 1) Compute the layers to create a hierarchy,
    // 2) Add dummy nodes to replace long edges 
    // 3) Order the nodes within each layer to try to minimize edge crossings,
    // 4) Compute (x,y) coordinates for each node using Linear Programming,
    // 5) Generate the SVG image based on the computed coordinates
    let Layout (edges:Edges) (node_labels: string seq) : string =

        let hierarchy : Hierarchy = {edges = edges; layers = ComputeLayers(edges)}
                                    |> AddDummyNodes
                                    |> OrderLayers

        let coordinatesList : (float*float) list = hierarchy
                                                   |> DetermineCoordinates

        let svgString : string = GenerateSVG (hierarchy) (coordinatesList) (node_labels)

        svgString
