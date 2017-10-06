(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Adding new algorithm
========================

To create new algorithm page add to project two files: `AlgorithmNameClient.fs` and `AlgorithmNameServer.fs` 

Then edit next type adding link for new page: 

*)
type EndPoint =
    | [<EndPoint "/">] Home
    | [<EndPoint "/BioGraph">] BioGraph
    | [<EndPoint "/NewPageLink">] NewPageLink
(**

Next, create function that will generate empty page:

*)
let NewPageLink ctx =
    Templating.Main ctx EndPoint.NewPageLink "NewPageLink" [
        Div [
                H1 [Text "Your app"] -< [Attr.Align "center"]
                ] -< [Attr.Class "jumbotron"]
        Div [
            ClientSide <@ NewPageLinkClient.MainFormRun () @>
            ]   -< [Attr.Align "center"]

(**
Where `NewPage.MainFormRun ()` is a form that you have to run from `NewPageClient.fs` file using `WebComponents` module.

<img src="img/img2.PNG" alt="2"/>

To add descriptional form for you algorithm on main page add next code:
*)
let NewPageForm = {
            Name = "NewPage"; 
            Description =  "Small algorithm description"; 
            Link = (ctx.Link EndPoint.NewPageLink)
        }
yield (NewPage.CreateForm())

(**
<img src="files/img/img1.PNG" alt="1"/>

<img src="docsr/files/img/img1.PNG" alt="1"/>

<img src="docs/img/img1.PNG" alt="1"/>
*)

    
