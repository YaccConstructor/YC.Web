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


[<JavaScript>]
module Client =

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

    let style = "padding-top: 0px; background-color: #FF69B4; border-width: 3px; font-weight: bold; border-color: #000000; border-radius: 10px; color: #000000; height: " + fst(getFormSize 40 150) + "; width: " + snd(getFormSize 40 150) + "; font-size:" + fst(getFormSize 15 15);
    

//=========GraphParsing client-side block=========//

    module GraphParsingApp =                                                                                              

        let ChooseDefaultControl (defaultData: List<string * string>) = 
            wsff.Do {
                let! dataSelect = 
                    wsfc.Select 0 (("", "") :: defaultData)
                    |> wsfe.WithTextLabel "ChooseDefault"
                    |> setFormSize (getFormSize 30 150) "select" 
                    |> wsfe.WithFormContainer     
                return dataSelect }
 
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
    
        let RangeControl =
            wsff.Yield (fun min max -> (int min, int max))
            <*> wsff.Do {
                    let! initVert = 
                        wsfc.Input ""
                        |> wsfe.WithTextLabel "Initial" 
                        |> setFormSize (getFormSize 20 50) "input"
                    return initVert }
            <*> wsff.Do {                
                    let! finVert = 
                        wsfc.Input ""
                        |> wsfe.WithTextLabel "Final" 
                        |> setFormSize (getFormSize 20 50) "input"      
                    return finVert }
            |> wsff.Horizontal 
            |> wsfe.WithTextLabel "Vertices"
            |> wsfe.WithLabelAbove
            |> wsfe.WithFormContainer
    
        let ErrorControl errortxt lbl = 
            wsff.Do {
            let! output =
                wsff.OfElement (fun () -> TextArea [Attr.ReadOnly "readonly"; Text ("Error: " + errortxt)])
                |> wsfe.WithTextLabel lbl
                |> wsfe.WithLabelAbove
                |> setFormSize (formH, formW) "textarea" 
            return output }
            |> wsfe.WithFormContainer 

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
        let ShowGraphImageControl lbl (graph: GraphParsingFunctions.InputGraph) id = 
            wsff.OfElement(fun () -> Graph lbl (graph.edges, graph.countOfVertex) (Div [Attr.Id id]))
            |> wsfe.WithTextLabel lbl        
            |> wsfe.WithLabelAbove 
            |> wsfe.WithFormContainer  

        let ShowTreeImageControl lbl  (tree: GraphParsingFunctions.ParsedSppf) id  = 
            wsff.OfElement(fun () -> SPPF lbl (tree.edges, tree.countOfVertex)  (Div [Attr.Id id]))
            |> wsfe.WithTextLabel lbl        
            |> wsfe.WithLabelAbove 
            |> wsfe.WithFormContainer           
    
        let VeryImportantForm = 
            let hw = "height: " + snd(getFormSize 540 47) + "; width: " + fst(getFormSize 540 47)
            wsff.OfElement(fun () -> Form [Attr.Style( "border-color: white; " + hw)])

        let FileControlWithVeryImportantForm defaultData = 
            wsff.Do {
                let! (defaultValue, fileInput) = 
                    wsff.Do { 
                        let! defaultValue = ChooseDefaultControl defaultData
                        let! fileInput = FileControl
                        return (fileInput, defaultValue) } 
                    |> wsff.Horizontal 
                let! f = VeryImportantForm
                return (defaultValue, fileInput) } 
                |> wsfe.WithFormContainer                                      
   
        let InputGrammarControl lbl defaultData = 
           wsff.Do {
                let! (defaultValue, fileInput) = FileControlWithVeryImportantForm defaultData              
                let txt = 
                    match fileInput with
                    | "" -> defaultValue
                    | _ -> fileInput
                let! textInput =
                    wsfc.TextArea txt            
                    |> wsfe.WithTextLabel lbl
                    |> wsfe.WithLabelAbove
                    |> setFormSize (formH, formW) "textarea"     
                return (textInput) }
             |> wsff.FlipBody
             |> wsff.Vertical
             |> wsfe.WithFormContainer

        let InputGraphControl lbl defaultData = 
           wsff.Do {
                let! (defaultValue, fileInput) = 
                    wsff.Do {  
                        let! defaultValue = ChooseDefaultControl defaultData
                        let! fileInput = FileControl
                        return  (defaultValue, fileInput) }   
                    |> wsff.Horizontal                      
                    |> wsfe.WithFormContainer              
                let txt = 
                    match fileInput with
                    | "" -> defaultValue
                    | _ -> fileInput
                let! textInput =
                    wsfc.TextArea txt            
                    |> wsfe.WithTextLabel lbl
                    |> wsfe.WithLabelAbove
                    |> setFormSize (formH, formW) "textarea"          
                return (textInput) }
             |> wsff.FlipBody
             |> wsff.Vertical
             |> wsfe.WithFormContainer
        let Form = 
            let InputForm =   
                let LeftInputForm =         
                    wsff.Do {
                        let! grammar = InputGrammarControl "Grammar" (GraphParsingRemote.LoadDefaultFileNames GraphParsingRemote.FileType.Grammar |> List.map (fun grmName -> grmName, GraphParsingRemote.LoadDefaultFile GraphParsingRemote.FileType.Grammar grmName))
                        return (grammar) }
                    |> wsff.Vertical
                let RightInputForm  = 
                    wsff.Do {
                        let! graph = InputGraphControl "Graph" (GraphParsingRemote.LoadDefaultFileNames GraphParsingRemote.FileType.Graph |> List.map (fun grmName -> grmName, GraphParsingRemote.LoadDefaultFile GraphParsingRemote.FileType.Graph grmName))
                        let! subgraphCheckbox = wsfc.Checkbox false |> wsfe.WithTextLabel "Show formal subgraph" |> wsfe.WithLabelLeft
                        let! removeCheckbox = wsfc.Checkbox false |> wsfe.WithTextLabel "Remove redundant nodes" |> wsfe.WithLabelLeft
                        return(graph,subgraphCheckbox,removeCheckbox)}        
                    |> wsff.Vertical
                    |> wsfe.WithFormContainer
            
                (wsff.Yield (fun (leftInput: string) (rightInput: string*bool*bool) -> (leftInput,  rightInput))
                <*> (LeftInputForm)
                <*> (RightInputForm))
                |> wsff.Horizontal
                |> wsfe.WithCustomSubmitButton ({ wsfe.FormButtonConfiguration.Default with 
                                                                                                Label = Some "SHOW GRAPH" 
                                                                                                Style = Some style })
                
            let OutputForm ((grammar: string), ((graph: string), (subgraphCheckbox: bool),  (removeCheckbox: bool))) =  
                let VisualizationWithRangeForm = 
                    let VisualizationForm  =                               
                            wsff.Do {
                                match GraphParsingRemote.draw grammar graph subgraphCheckbox removeCheckbox with
                                | GraphParsingRemote.Result.Error msg ->
                                    let! graphImg = ErrorControl msg "Graph Visualization"
                                    let! sppfImg = ErrorControl msg "SPPF"
                                    return (graphImg, sppfImg)
                                | GraphParsingRemote.Result.SucTreeGraph (tree, graph) ->
                                    let! graphImg = ShowGraphImageControl "Graph Visualization" graph "canvas1"
                                    let! sppfImg = ShowTreeImageControl  "SPPF" tree "canvas2"
                                    return (graphImg, sppfImg) }          
                              |> wsfe.WithFormContainer 
                              |> wsff.Horizontal  

                    let RangeAndButtonForm  =
                            wsff.Do {
                                let! rng = RangeControl
                                return rng }                   
                            |> wsfe.WithCustomSubmitButton ({ wsfe.FormButtonConfiguration.Default with 
                                                                                                        Label = Some "FIND PATH"
                                                                                                        Style = Some style })   
                            |> wsff.Horizontal    
               
                    wsff.Do {
                        let! x = VisualizationForm 
                        let! y = RangeAndButtonForm
                        return (x, y) }
                    |> wsff.Vertical
            
                let PathsVisualizationForm rng = 
                            wsff.Do {
                                if fst rng < snd rng && fst rng >= 0 && snd rng >= 0
                                then
                                    match GraphParsingRemote.findMinLen grammar graph removeCheckbox (fst rng) (snd rng) with
                                    | GraphParsingRemote.Result.Error msg ->
                                        let! pathImg = ErrorControl msg "Path"
                                        let! sppfPathImg = ErrorControl msg "SPPF Path" 
                                        return (pathImg, sppfPathImg)
                                    | GraphParsingRemote.Result.SucTreeGraph (tree, graph) ->
                                        let! pathImg = ShowGraphImageControl "Path" graph "canvas3"
                                        let! sppfPathImg = ShowTreeImageControl "SPPF Path" tree "canvas4"
                                        return (pathImg, sppfPathImg)
                                else
                                    let! pathImg = ErrorControl "Incorrect range" "Path"
                                    let! sppfPathImg = ErrorControl "Incorrect range" "SPPF Path"
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
                return (x, y) }
                |> wsff.Vertical 
    
        let FormRun () =
            let MainForm =  Form.Run(fun _ -> ())

            Div [
                MainForm
            ]
 
 //=========BioGraph client-side block=========//

     [<JavaScript>]
    module BioGraphApp =

        let ChooseDefaultControl (defaultData: List<string * string>) = 
            wsff.Do {
                let! dataSelect = 
                    wsfc.Select 1 (("", "") :: defaultData)
                    |> wsfe.WithTextLabel "Choose default"
                    |> setFormSize (getFormSize 30 210) "select" 
                    |> wsfe.WithFormContainer     
                return dataSelect }

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

        let InputControl lbl defaultData =
            wsff.Do {
                let! (defaultValue, fileInput) = 
                    wsff.Do { 
                        let! defaultValue = ChooseDefaultControl defaultData
                        let! fileInput = FileControl
                        return (defaultValue, fileInput) } 
                    |> wsff.FlipBody
                let txt = 
                    match fileInput with
                    | "" -> defaultValue
                    | _ -> fileInput
                let! textInput =
                        wsfc.TextArea txt              
                        |> wsfe.WithTextLabel lbl
                        |> wsfe.WithLabelAbove
                        |> setFormSize (getFormSize 85 500) "textarea"          
                return (textInput) }
            |> wsff.FlipBody
            |> wsff.Vertical
            |> wsfe.WithFormContainer

        let RangeControl =
            wsff.Yield (fun min max -> (int min, int max))
            <*> wsff.Do {
                    let! min = 
                        wsfc.Input ""
                        |> wsfe.WithTextLabel "from" 
                        |> setFormSize (getFormSize 30 210) "input"
                    return min }
            <*> wsff.Do {                
                    let! max = 
                        wsfc.Input ""
                        |> wsfe.WithTextLabel "to" 
                        |> setFormSize (getFormSize 30 210) "input"      
                    return max }
            |> wsff.Horizontal 
            |> wsfe.WithTextLabel "String range"
            |> wsfe.WithLabelAbove 
            |> wsfe.WithFormContainer 

        let OutputControl outputText  =
            wsff.Do {
                let! wrapCheckbox = wsfc.Checkbox false |> wsfe.WithTextLabel "wrap" |> wsfe.WithLabelLeft 
                let wrapValue =
                    match wrapCheckbox with
                    | false -> "off"
                    | true -> "soft"             
                let! output =
                    wsff.OfElement (fun () -> TextArea [Attr.ReadOnly "readonly"; Attr.Wrap wrapValue; Text outputText] )
                    |> wsfe.WithTextLabel "Output"
                    |> wsfe.WithLabelAbove
                    |> setFormSize (getFormSize 85 500) "textarea"               
                return output
                    }
        
            
            |> wsfe.WithFormContainer
            |> wsff.FlipBody
              
        let Graph (height, width, g: array<int * int * string * bool>, c: int) =
            let button = Button [Text "Draw!"; Attr.Style "width: 350px; height: 350px"]
            button.OnClick (fun _ _ -> 
                JS.Window?draw g c
                button.Remove()) 
            Div [
                Div [Attr.Id "canvas"]
                button
                ]
      
        let ShowImageControl grOption drawGr = 
            let src =
                match (grOption, drawGr) with
                | (None, true) -> wsff.OfElement (fun () ->
                let hw = "height: " + fst(getFormSize 355 355) + "; width: " + fst(getFormSize 355 355)
                Img [Attr.Style hw; Attr.Src "defaultImg.svg"])
                | (None, false) -> wsff.OfElement (fun () ->
                let hw = "height: " + fst(getFormSize 355 355) + "; width: " + fst(getFormSize 355 355)
                Img [Attr.Style hw; Attr.Src "defaultImg.svg"])
                | (Some(graphOption), true) -> //to do
                        let arr: array<int*int*string*bool> = Array.zeroCreate (Array.length graphOption.edges) 
                        for indx = 0 to Array.length graphOption.edges-1 do
                            let f1 nuc =
                                match nuc with
                                    |A -> "A"
                                    |U -> "U"
                                    |C -> "C"
                                    |G -> "G"  
                       
                            arr.[indx] <-
                                match graphOption.edges.[indx] with
                                    | a, b, c, d -> a,b,f1 c, d
                        wsff.OfElement(fun () ->Graph ((fst(getFormSize 355 355)),(fst(getFormSize 355 355)),arr, graphOption.countOfVertex))

                | (Some(graphOption), false) -> wsff.OfElement (fun () ->
                    let hw = "height: " + fst(getFormSize 355 355) + "; width: " + fst(getFormSize 355 355)
                    Img [Attr.Style hw; Attr.Src "defaultImg.svg"])
            src
                |> wsfe.WithTextLabel "Graph visualisation"
                |> wsfe.WithLabelAbove 
                |> wsfe.WithFormContainer
   
        let frm =   
            let InputForm  =
                let style = "padding-top: 0px; background-color: #FF69B4; border-width: 3px; border-color: #000000; color: #000000; height: " + fst(getFormSize 40 80) + "; width: " + snd(getFormSize 40 80) + "; font-size:" + fst(getFormSize 26 80); 

                (wsff.Yield (fun (grm: string) (graph: string) (rng: int * int) (drawGr: bool) -> (grm, graph, rng, drawGr))
                <*> (InputControl "Grammar" (BioGraphRemote.LoadDefaultFileNames BioGraphRemote.FileType.Grammar |> List.map (fun grmName -> grmName, BioGraphRemote.LoadDefaultFile BioGraphRemote.FileType.Grammar grmName)))
                <*> (InputControl "Graph" (BioGraphRemote.LoadDefaultFileNames BioGraphRemote.FileType.Graph |> List.map (fun grmName -> grmName, BioGraphRemote.LoadDefaultFile BioGraphRemote.FileType.Graph grmName)))
                <*> RangeControl
                <*> (wsfc.Checkbox false |> wsfe.WithTextLabel "DRAW GRAPH" |> wsfe.WithLabelLeft |> wsfe.WithFormContainer))
                |> wsfe.WithCustomSubmitButton ({ wsfe.FormButtonConfiguration.Default with 
                                                                                           Label = Some "GO" 
                                                                                           Style = Some style })
                |> wsff.Vertical
 
            let OutputForm (grm: string, graph: string, rng: int * int, drawGr: bool) =
                wsff.Do {
                    let (grOption, seqs) =
                        match BioGraphRemote.Parse grm graph rng drawGr with
                        | BioGraphRemote.Result.Error txt -> (None, txt)
                        | BioGraphRemote.Result.Success (grOption, seqs) -> 
                                                                      match grOption with
                                                                      | None -> (None, System.String.Join("\n",seqs))
                                                                      | Some(graphOption) -> (Some(graphOption), System.String.Join("\n",seqs))
                                                                                                                                                                                                                                                                                              
                    let! picture = ShowImageControl grOption drawGr
                    let! output = OutputControl seqs              
                    return (output) }
                |> wsff.Vertical 
                                                               
            wsff.Do {
                let! x = InputForm  
                let! y =  OutputForm x
                return (x, y) }
              |> wsff.Horizontal
                                                                     
        let FormRun () =
            let MainForm =
                frm.Run(fun _ -> ())
        
            Div [      
               MainForm
            ]