namespace YC.Web

open WebSharper
open Yard.Frontends.YardFrontend.Main
open Yard.Generators.GLL.AbstractParser
open AbstractAnalysis.Common
open Yard.Generators.GLL
open Yard.Generators.Common.FinalGrammar
open Yard.Generators.Common.InitialConvert
open Yard.Generators.Common.ASTGLL
open Yard.Generators.GLL.ParserCommon
open System.Collections.Generic
open Yard.Generators.GLL
open Yard.Generators.Common

//=========GraphParsing remoting block=========//
module GraphParsingRemote = 

    type Result =
        | SucTreeGraph of GraphParsingFunctions.ParsedSppf * GraphParsingFunctions.InputGraph
        | Error of string

    type FileType =
        | Graph
        | Grammar

    [<Rpc>]
    let LoadDefaultFileNames (fileType: FileType) =
        match fileType with
        | Grammar ->
            [
                "Math"
                "Bio"
            ]
        | Graph ->
            [
                "Math"
                "Bio"
            ]
    [<Rpc>]
    let LoadDefaultFile (fileType: FileType) name =
        match fileType with
        | Grammar ->
            match name with
            | "Math" -> @"[<Start>]
s: s P n | n
n: n M y | y
y: L s R | INT"
            | "Bio" -> @"[<Start>]
s: a b | b c | d
a: A
b: C
c: G
d: U"
            |  _  -> ""
        | Graph ->
            match name with
            | "Math" -> @"digraph {
    0 -> 1 [label = L]
    1 -> 2 [label = INT]
    2 -> 3 [label = P]
    3 -> 4 [label = INT]
    1 -> 5 [label = INT]
    5 -> 6 [label = M]
    6 -> 7 [label = INT]
    7 -> 8 [label = P]
    8 -> 4 [label = INT]
    4 -> 9 [label = R]
    9 -> 10 [label = M]
    10 -> 11 [label = INT]
    11 -> 12 [label = P]
    12 -> 13 [label = INT]            
}"
            | "Bio" -> @"digraph {
    0 -> 1 [label = A]
    1 -> 2 [label = C]            
}"
            |  _  -> ""

    [<Rpc>]
    let draw (grammar'text : string) (graph'text : string) (isFormal : bool) (isMinimised : bool)=
        try
            if grammar'text = "" && graph'text = "" then Error "Empty input"
            elif graph'text = "" then Error "Empty graph input"
            elif grammar'text = "" then Error "Empty grammar input"
            else
                let grammar, graph = GraphParsingFunctions.grmParse grammar'text, GraphParsingFunctions.graphParse graph'text
                match GraphParsingFunctions.parse grammar graph with
                | Yard.Generators.GLL.ParserCommon.ParseResult.Error msg -> Error msg
                | Yard.Generators.GLL.ParserCommon.ParseResult.Success tree ->
                    if isMinimised
                    then
                        if isFormal
                        then
                            let minimisedTree = GraphParsingFunctions.minimiseSppf tree
                            let formalSubgraph = GraphParsingFunctions.getFormalSubgraph minimisedTree (GraphParsingFunctions.graphToMap graph)
                            if formalSubgraph.countOfVertex <> 0
                            then
                                SucTreeGraph(GraphParsingFunctions.treeToParsed minimisedTree minimisedTree.Root (fun x -> true), formalSubgraph)
                            else
                                Error "There is no verticles in subgraph"
                        else
                            let minimisedTree = GraphParsingFunctions.minimiseSppf tree
                            SucTreeGraph (GraphParsingFunctions.treeToParsed minimisedTree minimisedTree.Root (fun x -> true), GraphParsingFunctions.toInputGraph graph)
                    else
                        if isFormal
                        then
                            let formalSubgraph = GraphParsingFunctions.getFormalSubgraph tree (GraphParsingFunctions.graphToMap graph)
                            if formalSubgraph.countOfVertex <> 0
                            then
                                SucTreeGraph(GraphParsingFunctions.treeToParsed tree tree.Root (fun x -> true), formalSubgraph)
                            else
                                Error "There is no verticles in subgraph"
                        else
                            SucTreeGraph (GraphParsingFunctions.treeToParsed tree tree.Root (fun x -> true), GraphParsingFunctions.toInputGraph graph)
        with
        | e -> Error e.Message

    [<Rpc>]
    let findMinLen (grammar'text : string) (graph'text : string) (isMinimised : bool) (first : int) (second : int) =
        try
            if grammar'text = "" && graph'text = "" then Error "Empty input"
            elif graph'text = "" then Error "Empty graph input"
            elif grammar'text = "" then Error "Empty grammar input"
            else
                let grammar, graph = GraphParsingFunctions.grmParse grammar'text, GraphParsingFunctions.graphParse graph'text
                match GraphParsingFunctions.parse grammar graph with
                | Yard.Generators.GLL.ParserCommon.ParseResult.Error msg -> Error msg
                | Yard.Generators.GLL.ParserCommon.ParseResult.Success tree ->
                    let mtree =
                        if isMinimised
                        then
                            GraphParsingFunctions.minimiseSppf tree
                        else
                            tree
                    let nTNode = GraphParsingFunctions.getNonTermNode mtree (packExtension first second)
                    match nTNode with
                    | GraphParsingFunctions.ResNode.Suc(node) ->
                        let edges, nodes = GraphParsingFunctions.getEdgesOfMinLen node
                        let tree, graph = GraphParsingFunctions.getTreeOfMnLn mtree edges nodes node (GraphParsingFunctions.toInputGraph graph)
                        SucTreeGraph(tree, graph)
                    |  GraphParsingFunctions.ResNode.None -> 
                        Error "No such nodes found"
                    |  GraphParsingFunctions.ResNode.Error msg -> 
                        Error msg
        with
        |e -> Error e.Message


//=========BioGraph remoting block=========//

module BioGraphRemote =
    type FileType =
        | Graph
        | Grammar

    type Result =
        | Error of string
        | Success of option<Graph> * string[]
        
    [<Rpc>]
    let LoadDefaultFileNames (fileType: FileType) =
        match fileType with
        | Grammar ->
            [
                "lite"
                "cycle"
                "brackets"
            ]
        | Graph ->
            [
                "lite"
                "cycle"
                "brackets"
            ]

    [<Rpc>]
    let LoadDefaultFile (fileType: FileType) name =
        match fileType with
        | Grammar ->
            match name with
            | "lite" -> @"[<Start>]
s: a b | b c | d
a: A
b: C
c: G
d: U"
            | "cycle" -> @"[<Start>]
s: a | d s
a: A
d: U"
            | "brackets" -> @"[<Start>]
s: d s | d
d: f | e
f: A s U
e: C"
            |  _  -> ""
        | Graph ->
            match name with
            | "lite" -> @"digraph {
    0 -> 1 [label = U]
    1 -> 2 [label = C]
}"
            | "cycle" -> @"digraph {
    0 -> 1 [label = U]
    1 -> 0 [label = U]
    1 -> 2 [label = A]
}"
            | "brackets" -> @"digraph {
    0 -> 0 [label = U]
    1 -> 1 [label = U]
    2 -> 2 [label = U]
    0 -> 1 [label = A]
    1 -> 2 [label = A]
    2 -> 0 [label = A]
    0 -> 2 [label = C]
    1 -> 0 [label = C]
    2 -> 1 [label = C]
}"
            |  _  -> ""

    [<Rpc>]
    let Parse (grammar'text: string) (graph'text: string) (range: int * int) (isOutputGraph: bool) =
        try
            if grammar'text = "" && graph'text = "" then Error "Empty input"
            elif graph'text = "" then Error "Empty graph input"
            elif grammar'text = "" then Error "Empty grammar input"
            else
                let grammar, graph = BioGraphFunctions.grmParse grammar'text, BioGraphFunctions.graphParse graph'text
                match BioGraphFunctions.parse grammar graph with
                | Yard.Generators.GLL.ParserCommon.ParseResult.Error msg -> Error msg
                | Yard.Generators.GLL.ParserCommon.ParseResult.Success tree ->
                    if fst range >= 0 && snd range >= fst range then
                        let extGraph = BioGraphFunctions.tree2extGraph tree
                        let mapInput = BioGraphFunctions.inputGraph2Map graph
                        let graphOpt = if isOutputGraph then Some (BioGraphFunctions.markGraph (graph.VertexCount - 1) mapInput (BioGraphFunctions.extGraph2edges extGraph)) else None
                        let graphSeqs = BioGraphFunctions.seqFilter << (BioGraphFunctions.lazyTree2guardedSeqs range) << BioGraphFunctions.iLazyTree2lazyTree mapInput << BioGraphFunctions.extGraph2iLazyTree <| extGraph
                        Success (graphOpt, graphSeqs)
                    elif range = (-1, -1) then
                        let extGraph = BioGraphFunctions.tree2extGraph tree
                        let mapInput = BioGraphFunctions.inputGraph2Map graph
                        let graphOpt = if isOutputGraph then Some (BioGraphFunctions.markGraph (graph.VertexCount - 1) mapInput (BioGraphFunctions.extGraph2edges extGraph)) else None
                        let graphSeqs = BioGraphFunctions.seqFilter << BioGraphFunctions.lazyTree2seqs << BioGraphFunctions.iLazyTree2lazyTree mapInput << BioGraphFunctions.extGraph2iLazyTree <| extGraph
                        Success (graphOpt, graphSeqs)
                    else Error "Unexcepted input range.\nRange must be (-1, -1) for output of all length seqs\nRange.from must be more or equal then 0 and Range.to must be more or equal than Range.from"
        with
        | e -> Error e.Message