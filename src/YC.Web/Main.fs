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
            LI [A [Attr.HRef "https://github.com/YaccConstructor/YC.Web"] -< [Text "Documentation"] ]
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
                    P [Text "YC.Web is a web application for YaccConstructor."] -< [Attr.Align "center"]
                    P [Text "YaccConstructor is a platform for parser generators and other grammarware research and development."] -< [Attr.Align "center"]
                    P [Text "Click "] -<
                        [
                        A [Text "here"] -< [Attr.HRef "http://yaccconstructor.github.io/YaccConstructor/index.html"]  -< [Target "_blank"]
                        ] -< [Text " to learn more about YaccConstructor."] -< [Attr.Align "center"]
                    ] -< [Attr.Class "container"]
                ] -< [Attr.Class "jumbotron"]
            Div [ 
                Div [
                    Div [
                        H2 [Text "Biograph"]
                        P [Text "Web application for searching subpaths in the metagenomic sequences. This app also visualizes the obtained sequences on input graph."]
                        P [ A [Text "Try app"] -< [Attr.HRef (ctx.Link EndPoint.BioGraph)] -< [Attr.Class "btn btn-default"] ]
                        ] -< [Attr.Class "col-md-4"]
                    Div [
                        H2 [Text "GraphParsingDemo"] 
                        P [Text "Web application for graph parsing and visualization. This app also can extract the minimal length path between two specified verteces."]
                        P [ A [Text "Try app"] -< [Attr.HRef (ctx.Link EndPoint.GraphParsingDemo)] -< [Attr.Class "btn btn-default"]]
                        ] -< [Attr.Class "col-md-4"]
                    Div [
                        H2 [Text "Recursive automata"] 
                        P [Text "Donec id elit non mi porta gravida at eget metus. Fusce dapibus, tellus ac cursus commodo, tortor mauris condimentum nibh, ut fermentum massa justo sit amet risus."]
                        P [ A [Text "Try app"] -< [Attr.HRef (ctx.Link EndPoint.RecursiveAutomata)] -< [Attr.Class "btn btn-default"]]
                        ] -< [Attr.Class "col-md-4"]
                    ] -< [Attr.Class "row"]
                ] -< [Attr.Class "container"]
            
        ]

    let BioGraphPage ctx =
        Templating.Main ctx EndPoint.BioGraph "BioGraph" [
            Div [
                H1 [Text "BioGraph page"] -< [Attr.Align "center"]
                ] -< [Attr.Class "jumbotron"]
        ]

    let GraphParsingDemoPage ctx =
       Templating.Main ctx EndPoint.GraphParsingDemo "GraphParsingDemo" [
            Div [
                H1 [Text "GraphParsing page"] -< [Attr.Align "center"]
                ] -< [Attr.Class "jumbotron"]
       ]

    let RecursiveAutomataPage ctx =
       Templating.Main ctx EndPoint.RecursiveAutomata "RecursiveAutomata" [
            Div [
                H1 [Text "Recursive Automata page"] -< [Attr.Align "center"]
                ] -< [Attr.Class "jumbotron"]
       ]
 
    [<Website>]
    let Main =
        Application.MultiPage (fun ctx endpoint ->
            match endpoint with
            | EndPoint.Home -> HomePage ctx
            | EndPoint.BioGraph -> BioGraphPage ctx
            | EndPoint.GraphParsingDemo -> GraphParsingDemoPage ctx
            | EndPoint.RecursiveAutomata -> RecursiveAutomataPage ctx
        )
