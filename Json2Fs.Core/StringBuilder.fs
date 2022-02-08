module StringBuilder

type StringBuilder () =
    member _.Yield el = el

    member _.Delay f = f()

    member _.Combine (a : string, b : string) = a + b

    member _.For (seq : seq<'a>, f : 'a -> string) =
        seq |> Seq.map f |> String.concat ""

let str = StringBuilder()
