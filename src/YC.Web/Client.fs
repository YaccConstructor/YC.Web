namespace YC.Web

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.Html.Client
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html
open WebSharper.UI.Next.Server

[<JavaScript>]
module Client =

    let Start input k =
        async {
            let! data = Server.DoSomething input
            return k data
        }
        |> Async.Start

    let Main () =
        let input = Input [Attr.Value ""] -< []
        let output = H1 []
        Div [
            input
            Button [Text "Send"]
            |>! OnClick (fun _ _ ->
                async {
                    let! data = Server.DoSomething input.Value
                    output.Text <- data
                }
                |> Async.Start
            )
            HR []
            H4 [Attr.Class "text-muted"] -< [Text "The server responded:"]
            Div [Attr.Class "jumbotron"] -< [output]
        ]

    let HelloWorld () =
        let Welcome = Input [Attr.Value ""]
        let Output = H2[]
        Div [
            Welcome
            Button [Text "Click me"]
            |>! OnClick (fun _ _ ->
                async {
                    Output.Text <- "Hello, world!"
                }
                |> Async.Start
            )
            
        ]
//        |> Doc.RunById "main"