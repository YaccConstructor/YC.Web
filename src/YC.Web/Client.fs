namespace YC.Web

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.Html.Client
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html
open WebSharper.UI.Next.Server
open WebSharper.Formlets

module wsfc = WebSharper.Formlets.Controls
module wsfe = WebSharper.Formlets.Enhance
module wsfd = WebSharper.Formlets.Data
module wsff = WebSharper.Formlets.Formlet
module wsfl = WebSharper.Formlets.Layout

//************General components and functions for visualization************//
[<JavaScript>]
module Client =

//************Funtions for scalability************//
    let screenWidth = JQuery.JQuery.Of("html").Width()
    let screenHeight = JQuery.JQuery.Of("html").Height()

    let getFormSize (height: int) (width: int) = 
        ((height * screenHeight / 638).ToString() + "px", (width * screenWidth / 1366).ToString() + "px")

    let setFormSize ((height: string), (width: string)) (formletType: string) (formlet: Formlets.Data.Formlet<'c>) =
        formlet |> wsff.MapElement (fun e ->
            JQuery.JQuery.Of(e.Dom.QuerySelector(formletType))
                .Css("height", height) 
                .Css("width", width)
                .Ignore
            e)

    let graphSize = 540 * screenWidth / 1366

    let formH = fst(getFormSize 90 540)
    let formW = snd(getFormSize 90 540)

    let buttonStyle = "padding-top: 0px;
                 background-color: #FF69B4; 
                 border-width: 3px; 
                 font-weight: bold;
                 border-color: #000000; 
                 border-radius: 10px; 
                 color: #000000; 
                 height: " + fst(getFormSize 40 150) + "; 
                 width: " + snd(getFormSize 40 150) + "; 
                 font-size:" + fst(getFormSize 15 15); 
  
//************Drop down menu with default values************// 

    let ChooseDefaultControl (defaultData: List<string * string>) = 
        wsff.Do {
            let! dataSelect = 
                wsfc.Select 1 (("", "") :: defaultData)
                |> wsfe.WithTextLabel "ChooseDefault"
                |> setFormSize (getFormSize 27 150) "select" 
                |> wsfe.WithFormContainer 
            return dataSelect
                }

//************Choose from file button************// 
 
    let FileControl = 
        let readFile (elFrom: Element) (stateChanged: Event<_>) =
            let file = (WebSharper.JavaScript.FileList.OfElement elFrom.Dom).Item 0
            let reader = new WebSharper.JavaScript.TextFileReader()            
            reader.ReadAsText file
            reader.AddEventListener("load", (fun () -> stateChanged.Trigger(Result.Success reader.Result)), true)

        Formlet.BuildFormlet <| fun() ->
            let stateChanged = new Event<Result<string>>()
            let input =
                Input [Attr.Type "file"; Attr.Accept "text/*"]
                |>! OnChange (fun e -> readFile e stateChanged)                        
            let reset () =
                input.Value <- ""
                stateChanged.Trigger(Result.Success "")
            input, reset, stateChanged.Publish
        |> Formlet.InitWith ""

//************InputArea component************// 
     
    //Text input area with ChooseFromFile and ChooseDefault buttons          
    let InputAreaControl signature defaultData  = 
        wsff.Do {
            let! (defaultValue, fileInput) = 
                wsff.Do {  
                    let! defaultValue = ChooseDefaultControl defaultData
                    let! fileInput = FileControl
                    return  (defaultValue, fileInput) 
                    }   
                |> wsff.Horizontal                                
            let txt = 
                match fileInput with
                | "" -> defaultValue
                | _ -> fileInput
            let! textInput =
                wsfc.TextArea txt            
                |> wsfe.WithTextLabel signature
                |> wsfe.WithLabelAbove
                |> setFormSize (formH, formW) "textarea"          
            return (textInput)
             }
            |> wsff.FlipBody
            |> wsff.Vertical
            |> wsfe.WithFormContainer


//************OutputArea component************// 

    //Text output area
    let OutputAreaControl outputText signature = 
        wsff.Do {
        let! output =
            wsff.OfElement (fun () -> TextArea [Attr.ReadOnly "readonly"; Text (outputText)])
            |> wsfe.WithTextLabel signature
            |> wsfe.WithLabelAbove
            |> setFormSize (formH, formW) "textarea" 
        return output
         }
        |> wsfe.WithFormContainer 

//************GraphVisualization components************//
    
    //Visualize graph using JavaScript library
    let Graph lbl (g: array<int*int*string*bool>, c: int) (canvas: Element) =
        let hw = "height: " + formH + "; width: " + formW
        let button = Button [Text lbl; Attr.Style hw]
        canvas.OnClick (fun _ _ -> 
            canvas.Clear()
            JS.Window?draw(JS.Window?createGraph g c canvas.Id) canvas.Id graphSize) 
        button.OnClick (fun _ _ -> 
            JS.Window?draw(JS.Window?createGraph g c canvas.Id) canvas.Id graphSize
            button.Remove())     
        Div [
            canvas
            button
            ]

    //Create graph visualizing area
    let ShowGraphImageControl lbl (graph: GraphParsingFunctions.InputGraph) id = 
        wsff.OfElement(fun () -> Graph lbl (graph.edges, graph.countOfVertex) (Div [Attr.Id id]))
        |> wsfe.WithTextLabel lbl        
        |> wsfe.WithLabelAbove 
        |> wsfe.WithFormContainer  

//************Range component with integer values************//

    let RangeControl signature initLabel finLabel =
        wsff.Yield (fun min max -> (int min, int max))
        <*> wsff.Do {
                let! initField = 
                    wsfc.Input ""
                    |> wsfe.WithTextLabel initLabel 
                    |> setFormSize (getFormSize 20 50) "input"
                    |>  wsfd.Validator.IsInt "Enter numeric value"
                    |> wsfe.WithValidationIcon
                return initField }
        <*> wsff.Do {                
                let! finField = 
                    wsfc.Input ""
                    |> wsfe.WithTextLabel finLabel
                    |> setFormSize (getFormSize 20 50) "input"   
                    |>  wsfd.Validator.IsInt "Enter numeric value"
                    |> wsfe.WithValidationIcon   
                return finField }
        |> wsff.Horizontal 
        |> wsfe.WithTextLabel signature
        |> wsfe.WithLabelAbove
        |> wsfe.WithFormContainer

//************GraphParsing Page client-side block************//
    [<JavaScript>]
    module GraphParsingApp =                                                                                              
        
        //Visualize SPPF using JavaScript library
        let SPPF lbl (g: array<int*string*int*string>, c: int)  (canvas: Element) =
            let hw = "height: " + formH + "; width: " + formW
            let button = Button [Text lbl; Attr.Style hw]
            canvas.OnClick (fun _ _ -> 
                canvas.Clear()
                JS.Window?draw(JS.Window?createTree g c canvas.Id) canvas.Id graphSize) 
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
                        let! grammar = InputAreaControl "Grammar" (GraphParsingRemote.LoadDefaultFileNames GraphParsingRemote.FileType.Grammar |> List.map (fun grmName -> grmName, GraphParsingRemote.LoadDefaultFile GraphParsingRemote.FileType.Grammar grmName))
                        return (grammar) 
                        }
                    |> wsff.Vertical
                    |> wsfe.WithCustomFormContainer({wsfe.FormContainerConfiguration.Default with CssClass=Some"tomiddle"})
                let GraphInputForm  = 
                    wsff.Do {
                        let! graph = InputAreaControl "Graph" (GraphParsingRemote.LoadDefaultFileNames GraphParsingRemote.FileType.Graph |> List.map (fun grmName -> grmName, GraphParsingRemote.LoadDefaultFile GraphParsingRemote.FileType.Graph grmName))
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
    //            |> wsfe.WithCustomSubmitButton ({ wsfe.FormButtonConfiguration.Default with 
    //                                                                    Label = Some "SHOW GRAPH" 
    //                                                                    Style = Some buttonStyle })

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
                            let! rng = RangeControl "Vertices" "Initial" "Final"
                            return rng 
                            }                   
                        |> wsfe.WithCustomSubmitButton ({ wsfe.FormButtonConfiguration.Default with 
                                                                                                    Label = Some "FIND PATH"
                                                                                                    Style = Some buttonStyle })  
                        |> wsff.Horizontal    
               
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
 
 //************BioGraph Page client-side block************//

    [<JavaScript>]
    module BioGraphApp =
    
        let Graph (height, width, g: array<int * int * string * bool>, c: int) =
            let button = Button [Text "Draw!"; Attr.Style "width: 350px; height: 350px"]
            button.OnClick (fun _ _ -> 
                JS.Window?draw g c
                button.Remove()) 
            Div [
                Div [Attr.Id "canvas"]
                button
                ]
   
//        let ShowImageControl grOption drawGr = 
//            let src =
//                match (grOption, drawGr) with
//                | (None, true) -> wsff.OfElement (fun () ->
//                let hw = "height: " + fst(getFormSize 355 355) + "; width: " + fst(getFormSize 355 355)
//                Img [Attr.Style hw; Attr.Src "defaultImg.svg"])
//                | (None, false) -> wsff.OfElement (fun () ->
//                let hw = "height: " + fst(getFormSize 355 355) + "; width: " + fst(getFormSize 355 355)
//                Img [Attr.Style hw; Attr.Src "defaultImg.svg"])
//                | (Some(graphOption), true) -> //to do
//                        let arr: array<int*int*string*bool> = Array.zeroCreate (Array.length graphOption.edges) 
//                        for indx = 0 to Array.length graphOption.edges-1 do
//                            let f1 nuc =
//                                match nuc with
//                                    |A -> "A"
//                                    |U -> "U"
//                                    |C -> "C"
//                                    |G -> "G"  
//                       
//                            arr.[indx] <-
//                                match graphOption.edges.[indx] with
//                                    | a, b, c, d -> a,b,f1 c, d
//                        wsff.OfElement(fun () ->Graph ((fst(getFormSize 355 355)),(fst(getFormSize 355 355)),arr, graphOption.countOfVertex))
//
//                | (Some(graphOption), false) -> wsff.OfElement (fun () ->
//                    let hw = "height: " + fst(getFormSize 355 355) + "; width: " + fst(getFormSize 355 355)
//                    Img [Attr.Style hw; Attr.Src "defaultImg.svg"])
//            src
//                |> wsfe.WithTextLabel "Graph visualisation"
//                |> wsfe.WithLabelAbove 
//                |> wsfe.WithFormContainer
   
//        let MainForm =   

        let InputForm = 
            let InputGrammarForm = 
                wsff.Do {
                    let! grammar= InputAreaControl "Grammar" (BioGraphRemote.LoadDefaultFileNames BioGraphRemote.FileType.Grammar |> List.map (fun grmName -> grmName, BioGraphRemote.LoadDefaultFile BioGraphRemote.FileType.Grammar grmName))
                    let! strRange = RangeControl "String Range" "Min" "Max" 
                    return (grammar,strRange)
                    }
                    |> wsff.Vertical
                    |> wsfe.WithCustomFormContainer({wsfe.FormContainerConfiguration.Default with CssClass=Some"tomiddle"})

//                        |> wsfe.WithCustomSubmitButton ({ wsfe.FormButtonConfiguration.Default with 
//                                                                                            Label = Some "GO" 
//                                                                                            Style = Some buttonStyle })      
            let InputGraphForm = 
                wsff.Do {
                    let! graph = InputAreaControl "Graph" (BioGraphRemote.LoadDefaultFileNames BioGraphRemote.FileType.Graph |> List.map (fun grmName -> grmName, BioGraphRemote.LoadDefaultFile BioGraphRemote.FileType.Graph grmName))
                    let! drawGr = wsfc.Checkbox false |> wsfe.WithTextLabel "Draw Graph" |> wsfe.WithLabelLeft |> wsfe.WithFormContainer
                    return (graph, drawGr)
                        } 
                    |> wsff.Vertical
                    |> wsfe.WithCustomFormContainer({wsfe.FormContainerConfiguration.Default with CssClass=Some"tomiddle"})
                                                                                  
            (wsff.Yield (fun (grmInput: string*(int*int)) (grphInput: string*bool) -> (grmInput,  grphInput))
            <*> (InputGrammarForm)
            <*> (InputGraphForm))
                |> wsff.Horizontal                              
                |> wsfe.WithCustomFormContainer({wsfe.FormContainerConfiguration.Default with CssClass=Some"totop"}) 
//            let OutputForm ((grm: string, rng: int * int), (graph: string, drawGr: bool)) =
//                wsff.Do {
//                    let (grOption, seqs) =
//                        match BioGraphRemote.Parse grm graph rng drawGr with
//                        | BioGraphRemote.Result.Error txt -> (None, txt)
//                        | BioGraphRemote.Result.Success (grOption, seqs) -> 
//                                                                      match grOption with
//                                                                      | None -> (None, System.String.Join("\n",seqs))
//                                                                      | Some(graphOption) -> (Some(graphOption), System.String.Join("\n",seqs))
//                                                                                                                                                                                                                                                                                              
//                    let! picture = ShowImageControl grOption drawGr
//                    let! output = OutputAreaControl seqs "Output"             
//                    return (output) }
//                |> wsff.Vertical 
//                                                               
//            wsff.Do {
//                let! x = InputForm  
//                let! y = OutputForm x
//                return (x, y) }
//              |> wsff.Horizontal
                                                                     
        let MainFormRun () =
            let Form = InputForm.Run(fun _ -> ())

            Div [
                Form 
            ] 