namespace CAB402

open System.Xml.Linq  

module FileInput =

    // Helper function to convert a string into a System.Xml.Linq.XName
    let xn s = XName.Get(s)
    
    // This function has been provided for you (do not modify)
    // Read a GraphML file (http://graphml.graphdrawing.org/) and convert it to a list of node labels and edges
    let Load (filename:string) : string list * Edges =
        let document = 
            XDocument.Load filename

        // Extract the node labels from the XML document and store in Map from node label to node id
        let node_map : Map<string, NodeId> = 
            document.Descendants() 
            |> Seq.where (fun element -> element.Name.LocalName = "node")
            |> Seq.mapi (fun i element -> element.Attribute("id").Value, i)
            |> Map.ofSeq

        // Get just the node_labels from the map
        let node_labels: string list = 
            node_map.Keys 
            |> Seq.toList

        // Function to convert a node label into a node Id
        let nodeId (element:XElement) (attribute_name:string) : int =
            node_map[element.Attribute(attribute_name).Value]

        // Extract the edge elements from the XML document and convert to set of edges
        let edges: Set<NodeId * NodeId> =
            document.Descendants()
            |> Seq.where (fun element -> element.Name.LocalName = "edge")
            |> Seq.map (fun element -> (nodeId element "source"), (nodeId element "target"))
            |> Set.ofSeq

        // Return both the node labels and the edges as a tuple
        (node_labels, edges)