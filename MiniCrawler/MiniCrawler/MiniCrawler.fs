open System
open System.Net
open System.IO
open System.Text.RegularExpressions

/// finds all referenced pages which have following form: <a href="http://...">
let findReferencedUrls html =
    let pattern = "<a\s+href\s*=\s*\"(https?://[^\"]+)\">" 
    Regex.Matches(html, pattern) |> Seq.map (fun x -> x.Groups.[1].Value) |> Seq.filter (fun url -> url <> "") 

/// downloads page using url
let downloadPage (url: string) =
    async {
        try
            let request = WebRequest.Create(url)
            use! response = request.AsyncGetResponse()
            use stream = response.GetResponseStream()
            use reader = new StreamReader(stream)
            let html = reader.ReadToEnd()
            return Some html
        with
        | _ -> return None
    }

/// downloads all pages referenced by given page and returns their addresses and size
let downloadAllReferencedPages (url: string) = 
    match downloadPage url |> Async.RunSynchronously with
    | Some html -> let referencedUrls = findReferencedUrls html
                   let htmls = Seq.map downloadPage referencedUrls 
                               |> Async.Parallel 
                               |> Async.RunSynchronously
                   htmls |> Seq.map (fun x -> if x.IsSome then x.Value.Length else -1) |> Seq.zip referencedUrls |> Some
    | _ -> None               

/// prints pair url * length
let print = Seq.iter (fun (url, length) -> printf "%s --- %d\n" url length)

[<EntryPoint>]
let main _ =
    printf "Please, enter URL: "
    let url = Console.ReadLine()
    match url |> downloadAllReferencedPages with 
    | Some result when Seq.isEmpty result -> printfn "No referenced pages"
    | Some result -> print result
    | None -> printfn "Failed to download: %s" url
    0 
