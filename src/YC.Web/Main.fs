namespace YC.Web

open WebSharper
open WebSharper.Sitelets

type EndPoint =
    | [<EndPoint "/">] Home
    | [<EndPoint "/BioGraph">] BioGraph
    | [<EndPoint "/GraphParsingDemo">] GraphParsingDemo
    | [<EndPoint "/RecursiveAutomata">] RecursiveAutomata

module Templating =
    open WebSharper.Html.Server

    type Page =
        {
            Title : string
            MenuBar : list<Element>
            Body : list<Element>
        }

    let MainTemplate =
        Content.Template<Page>("~/Main.html")
            .With("title", fun x -> x.Title)
            .With("menubar", fun x -> x.MenuBar)
            .With("body", fun x -> x.Body)

    // Compute a menubar where the menu item for the given endpoint is active
    let MenuBar (ctx: Context<EndPoint>) =
        let ( => ) txt act =
             LI [
                A [Attr.HRef (ctx.Link act)] -< [Text txt]
             ]
        [
            LI ["Home" => EndPoint.Home]
//            LI ["About" => EndPoint.BioGraph]
//            LI ["Test" => EndPoint.GraphParsingDemo]
        ]

    let Main ctx endpoint title body : Async<Content<EndPoint>> =
        Content.WithTemplate MainTemplate
            {
                Title = title
                MenuBar = MenuBar ctx 
                Body = body
            }

module Site =
    open WebSharper.Html.Server

    let HomePage ctx =
        Templating.Main ctx EndPoint.Home "Home"  [
            Div [
                Div [
                    H1 [Text "Welcome to YC.Web!"] -< [Attr.Align "center"]
                    P [Text ""] -< [Attr.Align "center"]
                    P [Text "Blablablalbalblbalablbabalballabbalablballablababllbaballbabllablablablablbaabl"] -< [Attr.Align "center"]
                    P [Text "Blablablalbalblbalablbabalballabbalablballablababllbabal"] -< [Attr.Align "center"]
                    P [Text "Blablablalbalblbalablbabalballabbalablball"] -< [Attr.Align "center"]
                    ] -< [Attr.Class "container"]
                ] -< [Attr.Class "jumbotron"]
            Div [ 
                Div [
                    Div [
                        H2 [Text "Biograph"]
                        P [Text "Donec id elit non mi porta gravida at eget metus. Fusce dapibus, tellus ac cursus commodo, tortor mauris condimentum nibh, ut fermentum massa justo sit amet risus. Etiam porta sem malesuada magna mollis euismod. Donec sed odio dui."]
                        P [ A [Text "View details"] -< [Attr.HRef (ctx.Link EndPoint.BioGraph)] -< [Attr.Class "btn btn-default"] ]
                        ] -< [Attr.Class "col-md-4"]
                    Div [
                        H2 [Text "GraphParsingDemo"] 
                        P [Text "Donec id elit non mi porta gravida at eget metus. Fusce dapibus, tellus ac cursus commodo, tortor mauris condimentum nibh, ut fermentum massa justo sit amet risus. Etiam porta sem malesuada magna mollis euismod. Donec sed odio dui."]
                        P [ A [Text "View details"] -< [Attr.HRef (ctx.Link EndPoint.GraphParsingDemo)] -< [Attr.Class "btn btn-default"]]
                        ] -< [Attr.Class "col-md-4"]
                    Div [
                        H2 [Text "Recursive automata algorithm"] 
                        P [Text "Donec id elit non mi porta gravida at eget metus. Fusce dapibus, tellus ac cursus commodo, tortor mauris condimentum nibh, ut fermentum massa justo sit amet risus. Etiam porta sem malesuada magna mollis euismod. Donec sed odio dui."]
                        P [ A [Text "View details"] -< [Attr.HRef (ctx.Link EndPoint.RecursiveAutomata)] -< [Attr.Class "btn btn-default"]]
                        ] -< [Attr.Class "col-md-4"]
                    ] -< [Attr.Class "row"]
                ] -< [Attr.Class "container"]
            
        ]

    let BioGraphPage ctx =
        Templating.Main ctx EndPoint.BioGraph "BioGraph" [
            H1 [Text "BioGraph"]
            P [Text "This is a template WebSharper client-server application."]
            Label [Text "WTF"]
            WebSharper.Html.Server.Tags.TextArea [Text ""] -< [Cols "20"] -< [Rows "14"] -< [Disabled ""]
        ]

    let GraphParsingDemoPage ctx =
       Templating.Main ctx EndPoint.GraphParsingDemo "GraphParsingDemo" [
           H1 [Text "GraphParsingDemoPage"]
           P [Text "This is test string"]
           Div [ClientSide <@ Client.HelloWorld() @>]
       ]

    let RecursiveAutomataPage ctx =
       Templating.Main ctx EndPoint.RecursiveAutomata "RecursiveAutomata" [
           H1 [Text "RecursiveAutomataPage"]
       ]
 
    [<Website>]
    let Main =
        Application.MultiPage (fun ctx endpoint ->
            match endpoint with
            | EndPoint.Home -> HomePage ctx
            | EndPoint.BioGraph -> BioGraphPage ctx
            | EndPoint.GraphParsingDemo -> GraphParsingDemoPage ctx
            | EndPoint.RecursiveAutomata -> GraphParsingDemoPage ctx
        )
