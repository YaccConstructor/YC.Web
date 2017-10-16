namespace YC.Web

open WebSharper
open WebSharper.Sitelets
open WebComponents.MainPageComponents


//Add here reference for new algorithm
type EndPoint =
    | [<EndPoint "/">] Home
    | [<EndPoint "/BioGraph">] BioGraph
    | [<EndPoint "/GraphParsingDemo">] GraphParsingDemo
    | [<EndPoint "/graph"; Wildcard>] Graph of countOfVertex:int * edges: array<int * int * string * int>


module Templating =
    open WebSharper.Html.Server

    type Page =
        {
            Title : string
            MenuBar : list<Element>
            Body : list<Element>
        }

    type GraphPage =
        {
            Title : string
            Body : list<Element>
        }
     
    //Structure of all pages in application
    let MainTemplate =
        Content.Template<Page>("~/Main.html")
            .With("title", fun x -> x.Title)
            .With("menubar", fun x -> x.MenuBar)
            .With("body", fun x -> x.Body)
 
    let GraphTemplate =
        Content.Template<GraphPage>("~/Graph.html")
            .With("title", fun x -> x.Title)
            .With("body", fun x -> x.Body)

    // Compute a menubar where the menu item for the given endpoint is active
    let MenuBar (ctx: Context<EndPoint>) =
        let ( => ) txt act =
             LI [
                A [Attr.HRef (ctx.Link act)] -< [Text txt]
             ]
        [
            LI ["Home" => EndPoint.Home]
            LI [A [Attr.HRef "http://yaccconstructor.github.io/YC.Web/"] -< [Text "Documentation"] ]
        ]

    let Main ctx endpoint title body : Async<Content<EndPoint>> =
        Content.WithTemplate MainTemplate
            {
                Title = title
                MenuBar = MenuBar ctx 
                Body = body
            }

    let Graph title body =
        Content.WithTemplate GraphTemplate
            {
                Title = title
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
                    //Creates algorithm form in main page                    
                    let BioGraphForm = {
                              Name = "Biograph"; 
                              Description =  "Web application for searching subpaths in the metagenomic sequences. This app also visualizes the obtained sequences on input graph."; 
                              Link = (ctx.Link EndPoint.BioGraph)
                            }
                    yield (BioGraphForm.CreateForm())

                    let GraphParsingForm = {
                              Name = "GraphParsingDemo"; 
                              Description = "Web application for graph parsing and visualization. This app also can extract the minimal length path between two specified verteces."; 
                              Link = (ctx.Link EndPoint.GraphParsingDemo)
                            }
                    yield (GraphParsingForm.CreateForm())
                    ] -< [Attr.Class "row"]
                ] -< [Attr.Class "container"] -< [Attr.Align "center"]          
        ]
    
    let BioGraphPage ctx =
        Templating.Main ctx EndPoint.BioGraph "BioGraph" [
            Div [
                H1 [Text "BioGraph page"]
                P [Text "Web application for searching subpaths in the metagenomic sequences."]
                P [Text  "This app also visualizes the obtained sequences on input graph."] 
                ] -< [Attr.Class "jumbotron"] -< [Attr.Align "center"]
            Div [
                ClientSide <@ BioGraphClient.MainFormRun () @>
             ] -< [Attr.Align "center"]
        ]

    let GraphParsingDemoPage ctx =
       Templating.Main ctx EndPoint.GraphParsingDemo "GraphParsingDemo" [
            Div [
                 H1 [Text "GraphParsing Application"]
                 P [Text "Web application for graph parsing and visualization."] 
                 P [Text "This app also can extract the minimal length path between two specified verteces."]  
                 ] -< [Attr.Class "jumbotron"] -< [Attr.Align "center"]
            Div [
                ClientSide <@ GraphParsingClient.MainFormRun () @>
             ]   -< [Attr.Align "center"]
              
       ]

    let GraphPage g i =
        Templating.Graph "Graph" [
            Div [Attr.Id "canvas"; Attr.Height "height"; Attr.Width "width"]
        ]

    [<Website>]
    let Main =
        Application.MultiPage (fun ctx endpoint ->
            match endpoint with
            | EndPoint.Home -> HomePage ctx
            | EndPoint.BioGraph -> BioGraphPage ctx
            | EndPoint.GraphParsingDemo -> GraphParsingDemoPage ctx
            | EndPoint.Graph (i, g) -> GraphPage g i
        )
