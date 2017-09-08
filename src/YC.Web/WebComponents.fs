namespace YC.Web

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.Html.Client
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html
open WebSharper.UI.Next.Server
open WebSharper.Formlets
open GraphParsingServer

module wsfc = WebSharper.Formlets.Controls
module wsfe = WebSharper.Formlets.Enhance
module wsfd = WebSharper.Formlets.Data
module wsff = WebSharper.Formlets.Formlet
module wsfl = WebSharper.Formlets.Layout

//************General components and functions for visualization************//
[<JavaScript>]
module WebComponents =

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
    let ShowGraphImageControl lbl (graph: GraphParsingServer.GraphParsingFunctions.InputGraph) id = 
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
                    |> setFormSize (getFormSize 20 100) "input"

                return initField }
        <*> wsff.Do {                
                let! finField = 
                    wsfc.Input ""
                    |> wsfe.WithTextLabel finLabel
                    |> setFormSize (getFormSize 20 100) "input"      
                return finField }

        |> wsff.Horizontal 
        |> wsfe.WithTextLabel signature
        |> wsfe.WithLabelAbove
        |> wsfe.WithFormContainer
