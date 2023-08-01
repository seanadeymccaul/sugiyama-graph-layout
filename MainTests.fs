namespace CAB402

open System.IO
open FileInput 
open CreateLayout
open Coordinates

module MainTests =

    Flips_Example()

    // Path to project folder relative to bin folder containing the executable
    let ProjectFolder = "../../.."

    // A simple test to start with ...
    let edges = Set [(0,2);(0,4);(0,5);(1,3);(1,4);(1,5);(2,5)]
    let node_labels = ["a";"b";"c";"d";"e";"f"]
    let svg = Layout edges node_labels
    printfn "%s" svg
    System.IO.File.WriteAllText($"{ProjectFolder}/TestResults/test.svg", svg)

    // Read a graph from an GraphML file, lay it out then dump to an .svg file
    let RomeTest filename =
        printfn "Layout %A ..." filename
        let (node_labels, edges) = Load $"{ProjectFolder}/RomeTests/{filename}.graphml"
        let svg_text = Layout edges node_labels
        System.IO.File.WriteAllText($"{ProjectFolder}/TestResults/{filename}.svg", svg_text)

    // Create a stopwatch
    let stopWatch = System.Diagnostics.Stopwatch.StartNew();

    // Some slightly more complex graphs ...
    RomeTest "g.10.2"
    RomeTest "g.10.7"
    RomeTest "g.10.13"
    RomeTest "g.10.14"
    RomeTest "g.10.22"
    RomeTest "g.10.28"
    RomeTest "g.10.37"
    RomeTest "g.10.42"
    RomeTest "g.10.45"
    RomeTest "g.10.56"
    RomeTest "g.55.0"
    RomeTest "g.55.26"
    RomeTest "g.56.11"

    stopWatch.Stop();
    printfn "\nPartA Time = %f milliseconds" stopWatch.Elapsed.TotalMilliseconds;
    
    
    // Test on all 1277 graphml files in the Rome benchmark set
    // for filename in Directory.GetFiles("../../../RomeTests/", "*.graphml")  do
    //    RomeTest (Path.GetFileNameWithoutExtension filename)
        