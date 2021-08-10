/// contains tests for web crawler
module MiniCrawler.Tests

    open NUnit.Framework
    open FsUnit

    [<Test>]
    let ``Shouldn't fail on incorrect address`` () =
        "g7fgh" |> downloadAllReferencedPages |> should equal None

    [<Test>]
    let ``Should return references correctly`` () =
        let expected = ["https://github.com/swlaschin/fsharpforfunandprofit.com"; "https://goo.gl/a1j5CS"]
        match "https://fsharpforfunandprofit.com/site-contents/" |> downloadAllReferencedPages with
        | Some actual -> Seq.map fst actual |> should equivalent expected
        | _ -> Assert.Fail("Expected value, but was None")

        let expected = ["https://www.mediawiki.org/wiki/Special:MyLanguage/How_to_contribute"; 
        "https://stats.wikimedia.org/#/en.wikipedia.org"; "https://foundation.wikimedia.org/wiki/Cookie_statement"; 
        "https://wikimediafoundation.org/"; "https://www.mediawiki.org/"]
        match "https://en.wikipedia.org/wiki/Main_Page" |> downloadAllReferencedPages with
        | Some actual -> Seq.map fst actual |> should equivalent expected
        | _ -> Assert.Fail("Expected value, but was None")

    [<Test>]
    let ``Should return references correctly on page without references`` () =
        match "https://fsharpforfunandprofit.com/about/license/" |> downloadAllReferencedPages with
        | Some actual -> Seq.map fst actual |> should equivalent []
        | _ -> Assert.Fail("Expected value, but was None")
