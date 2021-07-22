open System
open System.Net
open System.IO
open System.Text.RegularExpressions

let findReferencedUrls html =
    let pattern = "<a\s+href\s*=\s*\"(https?://[^\"]+)\">" // <a href="http://...">
    Regex.Matches(html, pattern) |> Seq.map (fun x -> x.Groups.[1].Value) |> Seq.filter (fun url -> url <> "") 

let downloadPage (url: string) =
    async {
        let request = WebRequest.Create(url)
        use! response = request.AsyncGetResponse()
        use stream = response.GetResponseStream()
        use reader = new StreamReader(stream)
        let html = reader.ReadToEnd()
        return html
    }

let downloadAllReferencedPages (url: string) = 
    let referencedUrls = downloadPage url |> Async.RunSynchronously |> findReferencedUrls
    let htmls = Seq.map downloadPage referencedUrls 
                |> Async.Parallel 
                |> Async.RunSynchronously
                |> Array.toSeq
                
    htmls |> Seq.map (fun x -> x.Length) |> Seq.zip referencedUrls

let print = Seq.iter (fun (url, length) -> printf "%s --- %d\n" url length)

[<EntryPoint>]
let main argv =
    let url = Console.ReadLine()
    let result = url |> downloadAllReferencedPages
    print result |> ignore
    0 