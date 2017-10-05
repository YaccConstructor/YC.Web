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
open BioGraphServer

module wsfc = WebSharper.Formlets.Controls
module wsfe = WebSharper.Formlets.Enhance
module wsfd = WebSharper.Formlets.Data
module wsff = WebSharper.Formlets.Formlet
module wsfl = WebSharper.Formlets.Layout

[<JavaScript>]
module BioGraphClient = 

//************BioGraph Page client-side block************//

    //Shows graph or not 
    let ShowImageControl grOption drawGr id = 
        let src =
            match (grOption, drawGr) with
            | (None, true) -> wsff.OfElement (fun () -> Img [Attr.Hidden "true"])                 
            | (None, false) -> wsff.OfElement (fun () -> Img [Attr.Hidden "true"]) 
            | (Some(graphOption), true) -> 
                    let arr: array<int*int*string*bool> = Array.zeroCreate (Array.length graphOption.edges) 
                    for indx = 0 to Array.length graphOption.edges-1 do
                        let f1 nuc =
                            match nuc with
                                |A -> "A"
                                |U -> "U"
                                |C -> "C"
                                |G -> "G"  
                       
                        arr.[indx] <- match graphOption.edges.[indx] with
                                      | a, b, c, d -> a,b,f1 c, d

                    wsff.OfElement(fun () -> Graph "Graph Visualization" (arr, graphOption.countOfVertex) (Div [Attr.Id id]))

            | (Some(graphOption), false) -> wsff.OfElement (fun () -> Img [Attr.Hidden "true"]) 

        src |> wsfe.WithLabelAbove |> wsfe.WithFormContainer

    let MainForm =   

        let InputForm = 
            let InputGrammarForm = 
                wsff.Do {
                    let! grammar= InputAreaControl "Grammar" (BioGraphRemote.LoadDefaultFileNames BioGraphRemote.FileType.Grammar
                        |> List.map (fun grmName -> grmName, BioGraphRemote.LoadDefaultFile BioGraphRemote.FileType.Grammar grmName))
                    let! strRange = RangeControl "String Range" "Min" "Max" 
                    return (grammar,strRange)
                    }
                    |> wsff.Vertical
                    |> wsfe.WithCustomFormContainer({wsfe.FormContainerConfiguration.Default with CssClass=Some"tomiddle"})
     
            let InputGraphForm = 
                wsff.Do {
                    let! graph = InputAreaControl "Graph" (BioGraphRemote.LoadDefaultFileNames BioGraphRemote.FileType.Graph
                        |> List.map (fun grmName -> grmName, BioGraphRemote.LoadDefaultFile BioGraphRemote.FileType.Graph grmName))
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

        let OutputForm ((grm: string, rng: int * int), (graph: string, drawGr: bool)) =
            wsff.Do {
                let (grOption, seqs) =
                    match BioGraphRemote.Parse grm graph rng drawGr with
                    | BioGraphRemote.Result.Error txt -> (None, txt)
                    | BioGraphRemote.Result.Success (grOption, seqs) -> 
                                                                    match grOption with
                                                                    | None -> (None, System.String.Join("\n",seqs))
                                                                    | Some(graphOption) -> (Some(graphOption), System.String.Join("\n",seqs))                  
                                                                                                                                                                                                                                                                                         

                let! output = OutputAreaControl seqs "Output"        
                let! picture = ShowImageControl grOption drawGr "canvas"  

                return (output,picture)
                }
            |> wsff.Horizontal 
            |> wsfe.WithCustomFormContainer({wsfe.FormContainerConfiguration.Default with CssClass=Some"totop"})
                                                               
        wsff.Do {
            let! x = InputForm  
            let! y = OutputForm x
            return (x, y) }
            |> wsff.Vertical
                                                                     
    let MainFormRun () =
        let Form = MainForm.Run(fun _ -> ())
        Div [
            Form 
        ] 
