namespace CAB402

module SVG =
    let PAGE_MARGIN = 10.0
    let X_SEP = 10.0
    let Y_SEP = 40.0
         
    // This function has been provided for you (do not modify)
    // Creates a Scalable Vector Graphics (SVG) drawing of the graph using the calculated coordinates and node labels
    // The return result is a string which can be saved to a .svg file and viewed in an SVG viewer (which includes most web browsers)
    let GenerateSVG (hierarchy:Hierarchy) (coordinates: (float*float) list) (node_labels:string seq) : string =
        let widest_label = 
            node_labels 
            |> Seq.map String.length 
            |> Seq.max 

        // the diameter of each node is based on the widest node label
        let NODE_DIAMETER = 
            20.0 * float(widest_label)

        // convert logical (x,y) coordinates to SVG coordinates
        let pixel_coordinates ((x, y):float*float): float*float = 
            (PAGE_MARGIN + x * (X_SEP + NODE_DIAMETER) + NODE_DIAMETER/2.0, 
                PAGE_MARGIN + y * (Y_SEP + NODE_DIAMETER) + NODE_DIAMETER/2.0)

        // draw one a line for the edge from src to dst
        let SVGLine (src:NodeId) (dst:NodeId) (coordinates: (float*float) list) : string =
            let (x1,y1) = pixel_coordinates coordinates[src]
            let (x2,y2) = pixel_coordinates coordinates[dst]
            $"""    <line x1="{x1}" y1="{y1}" x2="{x2}" y2="{y2}" stroke="black"/>"""

        // draw the node which consists of the node label as the text element, with a circle around it
        let SVGNode (nodeId:NodeId) (node_label:string) (coordinates: (float*float) list): string =
            let (x,y) = pixel_coordinates coordinates[nodeId]
            let circle = $"""    <circle cx="{x}" cy="{y}" r="{NODE_DIAMETER/2.0}" fill="white" stroke="black"/>"""
            let text_label = $"""    <text x="{x}" y="{y}" dominant-baseline="middle" text-anchor="middle">{node_label}</text>"""
            circle + "\r\n" + text_label

        // draw all of the lines
        let lines =
            hierarchy.edges
            |> Set.map (fun (src,dst) -> SVGLine src dst coordinates)
            |> String.concat "\r\n"

        // draw all of the nodes
        let nodes = 
            node_labels 
            |> Seq.mapi (fun nodeId node_label -> SVGNode nodeId node_label coordinates)
            |> String.concat "\r\n"

        // scale the viewBox based in the largest x and y coordinates using for the nodes
        let viewBox =
            let pixels = coordinates |> List.map pixel_coordinates
            let max_x = pixels |> List.map fst |> List.max
            let max_y = pixels |> List.map snd |> List.max
            let width  = max_x  + NODE_DIAMETER/2.0 + PAGE_MARGIN
            let height = max_y  + NODE_DIAMETER/2.0 + PAGE_MARGIN
            $"""viewBox="0 0 {width} {height}"""

        // create the standard SVG header and footer nodes
        let header = 
            $"""<svg xmlns="http://www.w3.org/2000/svg" {viewBox}">"""    

        let footer = 
            "</svg>"    

        // combine it all together into one big string
        String.concat "\r\n" [header; lines; nodes; footer]
