module Entry

open System.IO
open System.Text.Json
open Json2Fs.Core

match System.Environment.GetCommandLineArgs() with
| [| _; typename; json; fs |] ->
    File.ReadAllText json
    |> JsonSerializer.Deserialize<JsonElement>
    |> generateFSharpDTOsByJson typename
    |> fsRecordsToString
    |> (fun contents -> File.WriteAllText(fs, contents))
| _ -> raise (System.Exception ("Expected three arguments: typename of the root record, path to the json file and destination path"))

printfn $"Ok"