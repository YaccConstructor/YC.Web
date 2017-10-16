namespace YC.Web

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.Html.Client
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html
open WebSharper.UI.Next.Server
open WebSharper.Formlets
open WebComponents.AlgorithmsComponents
open GraphParsingServer

module wsfc = WebSharper.Formlets.Controls
module wsfe = WebSharper.Formlets.Enhance
module wsfd = WebSharper.Formlets.Data
module wsff = WebSharper.Formlets.Formlet
module wsfl = WebSharper.Formlets.Layout

[<JavaScript>]
module GraphParsingClient = 

//************GraphParsing Page client-side block************//

    //Visualize SPPF using JavaScript library
    let SPPF lbl (g: array<int*string*int*string>, c: int)  (canvas: Element) =
        let hw = "height: " + formH + "; width: " + formW
        let button = Button [Text lbl; Attr.Style hw]
        canvas.OnClick (fun _ _ -> 
            canvas.Clear()
            JS.Window?draw(JS.Window?createTree g c canvas.Id) canvas.Id graphSize) |> ignore
        button.OnClick (fun _ _ -> 
            JS.Window?draw(JS.Window?createTree g c canvas.Id) canvas.Id graphSize
            button.Remove()) 
        Div [
            canvas
            button
            ]
    //Create SPPF visualizing area    
    let ShowTreeImageControl lbl  (tree: GraphParsingFunctions.ParsedSppf) id  = 
        wsff.OfElement(fun () -> SPPF lbl (tree.edges, tree.countOfVertex)  (Div [Attr.Id id]))
        |> wsfe.WithTextLabel lbl        
        |> wsfe.WithLabelAbove 
        |> wsfe.WithFormContainer           
        
    let MainForm = 
        let InputForm = 
            let GrammarInputForm =         
                wsff.Do {
                    let! grammar = InputAreaControl "Grammar" "Type in grammar here. Use \"ChooseDefault\" to see example. You can also upload files from your device. "
                                                                    (GraphParsingRemote.LoadDefaultFileNames GraphParsingRemote.FileType.Grammar
                                                                    |> List.map (fun grmName -> grmName, GraphParsingRemote.LoadDefaultFile GraphParsingRemote.FileType.Grammar grmName))
                    return (grammar) 
                    }
                |> wsff.Vertical
                |> wsfe.WithCustomFormContainer({wsfe.FormContainerConfiguration.Default with CssClass=Some"tomiddle"})
            let GraphInputForm  = 
                wsff.Do {
                    let! graph = InputAreaControl "Graph" "Type in graph here. Use \"ChooseDefault\" to see example. If you want to show formal subgraph of input graph and/or remove redundant nodes from SPPF, use checkboxes under this form." 
                                                                    (GraphParsingRemote.LoadDefaultFileNames GraphParsingRemote.FileType.Graph 
                                                                    |> List.map (fun grmName -> grmName, GraphParsingRemote.LoadDefaultFile GraphParsingRemote.FileType.Graph grmName))
                    let! subgraphCheckbox = wsfc.Checkbox false |> wsfe.WithTextLabel "Show formal subgraph" |> wsfe.WithLabelLeft
                    let! removeCheckbox = wsfc.Checkbox false |> wsfe.WithTextLabel "Remove redundant nodes" |> wsfe.WithLabelLeft 
                    return(graph,subgraphCheckbox,removeCheckbox)
                    }        
                |> wsff.Vertical
                |> wsfe.WithCustomFormContainer({wsfe.FormContainerConfiguration.Default with CssClass=Some"tomiddle"})
            (wsff.Yield (fun (grmInput: string) (grphInput: string*bool*bool) -> (grmInput,  grphInput))
            <*> (GrammarInputForm)
            <*> (GraphInputForm))
            |> wsff.Horizontal  
            |> wsfe.WithCustomFormContainer({wsfe.FormContainerConfiguration.Default with CssClass=Some"totop"})

        let OutputForm ((grammar: string), ((graph: string), (subgraphCheckbox: bool),  (removeCheckbox: bool))) =  
            let VisualizationWithRangeForm = 
                let VisualizationForm  =                            
                    wsff.Do {
                        match GraphParsingRemote.draw grammar graph subgraphCheckbox removeCheckbox with
                        | GraphParsingRemote.Result.Error msg ->
                            let! graphImg = OutputAreaControl ("Error:" + msg) "Graph Visualization"
                            let! sppfImg = OutputAreaControl ("Error:" + msg) "SPPF"
                            return (graphImg, sppfImg)
                        | GraphParsingRemote.Result.SucTreeGraph (tree, graph) ->
                            let! graphImg = ShowGraphImageControl "Graph Visualization" graph "canvas1"
                            let! sppfImg = ShowTreeImageControl  "SPPF" tree "canvas2"
                            return (graphImg, sppfImg) 
                            }          
                        |> wsfe.WithFormContainer 
                        |> wsff.Horizontal  

                let RangeAndButtonForm  =
                    wsff.Do {
                        let! rng = RangeControl "Vertices" "To extract minimal length path between two specific vertices of the input graph write their numbers in special form and press \"Find Path\". Remember that the range input should be correct." "Initial" "Final"
                        return rng 
                        }                   
                    |> wsfe.WithCustomSubmitButton ({ wsfe.FormButtonConfiguration.Default with 
                                                                                                Label = Some "Find Path" 
                                                                                                Style = Some buttonStyle })  
                    |> wsff.Horizontal    
                    |> wsfe.WithCustomFormContainer({wsfe.FormContainerConfiguration.Default with CssClass=Some"todown"})
                wsff.Do {
                    let! x = VisualizationForm 
                    let! y = RangeAndButtonForm
                    return (x, y)
                        }
                |> wsff.Vertical
         
            let PathsVisualizationForm rng = 
                        wsff.Do {
                            if fst rng < snd rng && fst rng >= 0 && snd rng >= 0
                            then
                                match GraphParsingRemote.findMinLen grammar graph removeCheckbox (fst rng) (snd rng) with
                                | GraphParsingRemote.Result.Error msg ->
                                    let! pathImg = OutputAreaControl ("Error:" + msg) "Path"
                                    let! sppfPathImg = OutputAreaControl ("Error:" + msg) "SPPF Path"
                                    return (pathImg, sppfPathImg)
                                | GraphParsingRemote.Result.SucTreeGraph (tree, graph) ->
                                    let! pathImg = ShowGraphImageControl "Path" graph "canvas3"
                                    let! sppfPathImg = ShowTreeImageControl "SPPF Path" tree "canvas4"
                                    return (pathImg, sppfPathImg)
                            else
                                let! pathImg = OutputAreaControl "Error: Incorrect range" "Path"
                                let! sppfPathImg = OutputAreaControl "Error: Incorrect range" "SPPF Path"
                                return (pathImg, sppfPathImg) } 
                        |> wsfe.WithFormContainer 
                        |> wsff.Horizontal 
           
            wsff.Do {
                let! x = VisualizationWithRangeForm 
                let! y = PathsVisualizationForm (snd x)
                return (x, y) }
                |> wsff.Vertical  
                  
        wsff.Do {
            let! x = InputForm 
            let! y = OutputForm x
            return (y) }
            |> wsff.Vertical 

    let MainFormRun () =
        let Form = MainForm.Run(fun _ -> ())    
                 
        Div [
            Div [
                Form
                ] -< [Attr.Align "center"]
        ] 



 