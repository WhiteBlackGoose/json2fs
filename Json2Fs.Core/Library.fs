module Json2Fs.Core

open System
open System.Text.Json
open StringBuilder

let rec private mergeListsOrdered a b eq merge =
    let rec untilCommon (a : 'a list) (b : 'a list) : ('a * 'a list * 'a list * 'a list * 'a list * int) Option =
        match a, b with
        | h1 :: t1, h2 :: t2 when eq h1 h2 -> Some (merge h1 h2, [ ], [ ], t1, t2, 0)
        | [], [] -> None
        | [], _ | _, [] -> None
        | h1 :: t1, h2 :: t2 ->
            let value1 = untilCommon a t2
            let value2 = untilCommon t1 b
            match value1, value2 with
            | None, None -> None
            | Some (h, dh1, dh2, dt1, dt2, k), None -> Some (h, dh1, h2 :: dh2, dt1, dt2, k + 1)
            | None, Some (h, dh1, dh2, dt1, dt2, k) -> Some (h, h1 :: dh1, dh2, dt1, dt2, k + 1)
            | Some (h_1, dh1_1, dh2_1, dt1_1, dt2_1, k_1), Some (h_2, dh1_2, dh2_2, dt1_2, dt2_2, k_2) ->
                if k_1 < k_2 then
                    Some (h_1, dh1_1, h2 :: dh2_1, dt1_1, dt2_1, k_1 + 1)
                else
                    Some (h_2, h1 :: dh1_2, dh2_2, dt1_2, dt2_2, k_2 + 1)
    match untilCommon a b with
    | None -> List.append a b
    | Some (h, dh1, dh2, dt1, dt2, _) ->
        List.concat [ 
            dh1
            dh2
            [ h ]
            mergeListsOrdered dt1 dt2 eq merge
        ]

type TypeToGenerate = {
    typename : string
    props : Map<string, string list>
}

type Types = TypeToGenerate list

let mergeTwoTypes (props1 : Map<string, string list>) (props2 : Map<string, string list>) : Map<string, string list> =
    let keys = Set.union (Set<_>(props1.Keys)) (Set<_>(props2.Keys))

    let rec mergeValues (a : string list) (b : string list) : string list =
        match a, b with
        | ["obj"], other | other, ["obj"] -> other
        | "array" :: other1, "array" :: other2 -> "array" :: mergeValues other1 other2
        | any, _ -> any
    
    let propsToString v =
        v
        |> List.rev
        |> String.concat " "

    keys
    |> Seq.map (fun key ->
        if props1.ContainsKey key |> not then
            key, props2[key]
        else if props2.ContainsKey key |> not then
            key, props1[key]
        else
            let value1 = props1[key]
            let value2 = props2[key]
            key, mergeValues value1 value2
        )
    |> Map.ofSeq

let mergeTwoTypeLists (types1 : Types) (types2 : Types) =
    mergeListsOrdered types1 types2 (fun a b -> a.typename = b.typename) (fun a b -> { typename = a.typename; props = mergeTwoTypes a.props b.props })

let mergeSequenceOfTypeLists typess =
    Seq.fold mergeTwoTypeLists [] typess


let rec generateFSharpDTOsByJson (typename : string) (json : JsonElement) : Types =
    let capitalize (s : string) =
        System.Char.ToUpper(s[0]).ToString() + s[1..]

    let rec getType (name : string) (value : JsonElement) : (string list * Types) =
        match value.ValueKind with
        | JsonValueKind.Array ->
            match value.EnumerateArray() |> Seq.tryHead with
            | Some _ ->
                let gened = value.EnumerateArray() |> Seq.map (getType name)
                let mergedTypes = gened |> Seq.map (fun (_, s) -> s) |> mergeSequenceOfTypeLists
                let (proptype, _) = gened |> Seq.head
                ("array" :: proptype, mergedTypes)
            | None -> (["array"; "obj"], [])
        | JsonValueKind.False | JsonValueKind.True -> ["bool"], []
        | JsonValueKind.Number -> ["decimal"], []
        | JsonValueKind.Object ->
            let generated = generateFSharpDTOsByJson name value
            ([capitalize name], generated)
        | JsonValueKind.String -> ["string"], []
        | JsonValueKind.Null | JsonValueKind.Undefined -> ["obj"], []
        | _ -> raise (NotImplementedException ($"Unmatched kind: {value.ValueKind}"))

    if json.ValueKind <> JsonValueKind.Object then
        raise (Exception ("Ohno!"))

    let (_, addedTypes, props) = 
        json.EnumerateObject()
        |> Seq.fold (fun (alreadyAddedProps : Set<string>, addedTypes : TypeToGenerate list, props : Map<string, string list>) prop ->
            if alreadyAddedProps |> Seq.contains (prop.Name.ToLower()) then
                (alreadyAddedProps, addedTypes, props)
            else
                let alreadyAddedProps = alreadyAddedProps.Add (prop.Name.ToLower())
                let (proptype, additionalTypes) = getType prop.Name prop.Value
                let addedTypes = List.append additionalTypes addedTypes
                (alreadyAddedProps, addedTypes, props.Add (prop.Name, proptype))

            ) (Set.empty, [], Map.empty)
    
    { props = props; typename = capitalize typename } :: addedTypes

let fsRecordsToString (types : TypeToGenerate list) =
    let escape s =
        match s with
        | "abstract" | "type" -> $"``{s}``"
        | _ -> s

    str {
        for { props = props; typename = typename } in types |> List.rev do
            $"type {typename} = {{\n"
            for pair in props do
                let typename = pair.Value |> List.rev |> String.concat " "
                $"    {escape pair.Key} : {typename}\n"
            "}\n"
    }
