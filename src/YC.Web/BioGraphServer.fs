namespace YC.Web

type Nuc = | A | C | G | U

type Edge = int * int * Nuc * bool

type Graph = {
    countOfVertex: int;
    edges: Edge[];
}

open WebSharper
open WebSharper.JavaScript
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

module BioGraphServer = 


//************BioGraph server-side block************//
    module BioGraphFunctions =

        type Token =
            | A
            | C
            | G
            | U
            | EOF

        let graphParse graph'text =
            let gd = DotParser.parse graph'text
            let n = gd.Nodes.Count
            let g = new ParserInputGraph<Token>([|0..n|], [|n|])

            for i in 0..n do
                ignore <| g.AddVertex i

            for i in 0..n - 1 do
                ignore <| g.AddEdge(new ParserEdge<Token>(i, n, EOF))
    
            for edge in gd.Edges do
                let (x, y) = edge.Key
                let (a, b) = ref 0, ref 0
                if System.Int32.TryParse(x, a) && System.Int32.TryParse(y, b) then
                    match edge.Value.Head.["label"] with
                    | "A" -> ignore <| g.AddEdge(new ParserEdge<Token>(!a, !b, A))
                    | "C" -> ignore <| g.AddEdge(new ParserEdge<Token>(!a, !b, C))
                    | "G" -> ignore <| g.AddEdge(new ParserEdge<Token>(!a, !b, G))
                    | "U" -> ignore <| g.AddEdge(new ParserEdge<Token>(!a, !b, U))
                    | _ -> ()
            g
    
        let mutable indToString = fun i -> ""
        let mutable tokenToNumber = fun t -> 0
        let tokenData (t:Token): obj = null

        let grmParse parser_text = 
            let text = parser_text
            let grm = ParseText text "file.yrd"
            let icg = initialConvert grm
            let fg = FinalGrammar(icg.grammar.[0].rules, true)

            tokenToNumber <- function
                | A -> fg.indexator.termToIndex "A"
                | C -> fg.indexator.termToIndex "C"
                | G -> fg.indexator.termToIndex "G"
                | U -> fg.indexator.termToIndex "U"
                | EOF -> fg.indexator.eofIndex

            let genLiteral (s:string) (i:int): Token option = None
    
            let isLiteral (t:Token): bool = false
            let isTerminal (t:Token): bool = true
            let getLiteralNames = []

            let td = (Table fg).result
            let table = new System.Collections.Generic.Dictionary<int, int[]>()
            for k in td.Keys do
                table.Add(k, td.[k].ToArray())

            let rulesArr = Array.zeroCreate fg.rules.rulesCount
            for i = 0 to fg.rules.rulesCount-1 do
                rulesArr.[i] <- fg.rules.rightSide i

            let totalRulesLength = rulesArr |> Array.sumBy (fun x -> x.Length)
            let rules = Array.zeroCreate totalRulesLength
            let rulesStart = Array.zeroCreate <| fg.rules.rulesCount + 1
            let mutable cur = 0
            for i = 0 to fg.rules.rulesCount-1 do
                rulesStart.[i] <- cur
                for j = 0 to rulesArr.[i].Length-1 do
                    rules.[cur] <- rulesArr.[i].[j]
                    cur <- cur + 1
            rulesStart.[fg.rules.rulesCount] <- cur

            let acceptEmptyInput = true
            let numIsTerminal (i:int): bool = fg.indexator.termsStart <= i && i <= fg.indexator.termsEnd
            let numIsNonTerminal (i:int): bool = fg.indexator.isNonTerm i
            let numIsLiteral (i:int): bool = fg.indexator.literalsStart <= i && i <= fg.indexator.literalsEnd

            let numToString (n:int):string =
                if numIsTerminal n then
                    fg.indexator.indexToTerm n
                elif numIsNonTerminal n then
                    fg.indexator.indexToNonTerm n
                elif numIsLiteral n then
                    fg.indexator.indexToLiteral n
                else string n
    
            indToString <- numToString

            let inline packRulePosition rule position = (int rule <<< 16) ||| int position

            let slots = new List<_>()
            slots.Add(packRulePosition -1 -1, 0)
            for i = 0 to fg.rules.rulesCount - 1 do
                let currentRightSide = fg.rules.rightSide i
                for j = 0 to currentRightSide.Length - 1 do
                    if fg.indexator.isNonTerm currentRightSide.[j] then
                        let key = packRulePosition i (j + 1)
                        slots.Add(key, slots.Count)

            let parserSource = new ParserSourceGLL<Token>(Token.EOF, tokenToNumber, genLiteral, numToString, tokenData, isLiteral, isTerminal, getLiteralNames, table, rules, rulesStart, fg.rules.leftSideArr, fg.startRule, fg.indexator.literalsEnd, fg.indexator.literalsStart, fg.indexator.termsEnd, fg.indexator.termsStart, fg.indexator.termCount, fg.indexator.nonTermCount, fg.indexator.literalsCount, fg.indexator.eofIndex, fg.rules.rulesCount, fg.indexator.fullCount, acceptEmptyInput, numIsTerminal, numIsNonTerminal, numIsLiteral, fg.canInferEpsilon, slots |> dict)
            parserSource

        let parse grammar graph = buildAbstractAst<Token> grammar graph

        let unpackExtension(ext: int64<extension>) =
            let ``2^32`` = 256L * 256L * 256L * 256L
            (int <| ext / ``2^32``, int <| ext % (``2^32`` * 1L<extension>))
    
        type ExtensionEdge = int * int64<extension>
        type ExtensionGraph =
            {
                start: ExtensionEdge
                terminals: list<int64<extension>>
                ands: Map<ExtensionEdge, ExtensionEdge * ExtensionEdge>
                ors: Map<ExtensionEdge, list<ExtensionEdge>>
            }
    
        let internal genDummy = -1, -1L<extension>
        let internal genTermEdge (i: int64<extension>) = 0, i
        let mutable private j = 0
        let internal genNodeEdge (i: int64<extension>) =
            j <- 1 + j
            j, i
        let tree2extGraph (tree: Yard.Generators.Common.ASTGLL.Tree<Token>): ExtensionGraph =
            j <- 0
            let mutable terminals = []
            let mutable ands = Map.empty
            let mutable ors = Map.empty

            let rec f (linkStack: list<ExtensionEdge * obj>) (current: obj): ExtensionEdge =
                if current <> null then
                    let xs = List.filter (snd >> ((=) current)) linkStack
                    if List.length xs > 0 then
                        fst xs.Head
                    else
                        match current with
                        | :? TerminalNode as node ->
                            if not <| List.contains node.Extension terminals then
                                terminals <- node.Extension :: terminals
                            genTermEdge node.Extension
                        | :? PackedNode as node ->
                            let res = genNodeEdge ((node :> INode).getExtension())
                            let l = f ((res, current) :: linkStack) node.Left
                            let r = f ((res, current) :: linkStack) node.Right
                            ands <- Map.add res (l, r) ands
                            res
                        | :? NonTerminalNode as node ->
                            let res = genNodeEdge node.Extension
                            let xs =
                                (f ((res, current) :: linkStack) node.First)
                                :: (if node.Others <> null then [for it in node.Others -> f ((res, current) :: linkStack) it]
                                    else [])
                            ors <- Map.add res xs ors
                            res
                        | :? IntermidiateNode as node ->
                            let res = genNodeEdge node.Extension
                            let xs =
                                (f ((res, current) :: linkStack) node.First)
                                :: (if node.Others <> null then [for it in node.Others -> f ((res, current) :: linkStack) it]
                                    else [])
                            ors <- Map.add res xs ors
                            res
                        | _ -> genDummy
                else genDummy

            {
                start = f [] tree.Root
                terminals = terminals
                ands = ands
                ors = ors
            }
    
        let extGraph2edges (g: ExtensionGraph): list<int * int> =
            List.map unpackExtension g.terminals
    
        type LazyRegTree =
            | Or of seq<LazyRegTree>
            | And of Nuc * (unit -> LazyRegTree)
            | Empty
    
        let inputGraph2Map (inputG: ParserInputGraph<Token>) : Map<int * int, Nuc> =
            inputG.Edges
            |> Seq.filter (fun edge -> edge.Tag <> EOF)
            |> Seq.map (fun edge ->
                        (edge.Source, edge.Target),
                        (match edge.Tag with | A -> Nuc.A | C -> Nuc.C | G -> Nuc.G | U -> Nuc.U))
            |> Map.ofSeq
        
        type ILazyRegTree =
            | IOr of seq<ILazyRegTree>
            | IAnd of int64<extension> * (unit -> ILazyRegTree)
            | IEmpty
        
        let rec iDisOr (x: ILazyRegTree): seq<ILazyRegTree> =
            match x with
            | IOr xt -> seq { for xi in xt do for it in iDisOr xi -> it }
            | o -> seq { yield o }

        let rec iAndComb (f: unit -> ILazyRegTree) (s: unit -> ILazyRegTree): unit -> ILazyRegTree =
            fun () ->
                match f () with
                | IEmpty -> s ()
                | IAnd (t, o) -> IAnd (t, iAndComb o s)
                | IOr its -> IOr (seq { for it in iDisOr (IOr its) -> iAndComb (fun () -> it) s () })

        let extGraph2iLazyTree (g: ExtensionGraph) : ILazyRegTree =
            let rec gen current =
                match current with
                | (0, -1L<extension>) -> IEmpty
                | (0, tind) -> IAnd (tind, (fun () -> IEmpty))
                | x ->
                    if Map.containsKey x g.ands then
                        let l, r = g.ands.[x]
                        iAndComb (fun () -> gen l) (fun () -> gen r) ()
                    elif Map.containsKey x g.ors then
                        IOr << iDisOr << IOr <| (seq { for it in g.ors.[x] -> gen it })
                    else IOr Seq.empty
            gen (g.start)
    
        let iLazyTree2lazyTree (inputG: Map<int * int, Nuc>) (g: ILazyRegTree): LazyRegTree =
            let rec il2l (gr: ILazyRegTree) =
                match gr with
                | IEmpty -> Empty
                | IAnd(e, o) ->
                    let ext = unpackExtension e
                    if Map.containsKey ext inputG then
                        And(inputG.[ext], fun () -> il2l <| o ())
                    else il2l <| o ()
                | IOr s -> Or (seq { for it in s -> il2l it })
            il2l g

        let rec lazyTree2seqs (lt: LazyRegTree) : list<string> =
            match lt with
            | Empty -> ["<eos>"]
            | And(Nuc.A, o) -> List.map ((+) "A") (lazyTree2seqs (o ()))
            | And(Nuc.C, o) -> List.map ((+) "C") (lazyTree2seqs (o ()))
            | And(Nuc.G, o) -> List.map ((+) "G") (lazyTree2seqs (o ()))
            | And(Nuc.U, o) -> List.map ((+) "U") (lazyTree2seqs (o ()))
            | Or xs -> Seq.fold List.append [] (Seq.map lazyTree2seqs xs)

        let rec lazyTree2guardedSeqs (range: int * int) (lt: LazyRegTree) : list<string> =
            let next =
                match range with
                | 0, 0 -> 0, 0
                | 0, x -> 0, x - 1
                | x, y -> x - 1, y - 1
            match lt with
            | Empty when fst range = 0 && snd range >= 0 -> ["<eos>"]
            | Or xs when snd range >= 0 -> Seq.fold List.append [] (Seq.map (lazyTree2guardedSeqs range) xs)
            | And(Nuc.A, o) when snd range > 0 -> List.map ((+) "A") (lazyTree2guardedSeqs next (o ()))
            | And(Nuc.C, o) when snd range > 0 -> List.map ((+) "C") (lazyTree2guardedSeqs next (o ()))
            | And(Nuc.G, o) when snd range > 0 -> List.map ((+) "G") (lazyTree2guardedSeqs next (o ()))
            | And(Nuc.U, o) when snd range > 0 -> List.map ((+) "U") (lazyTree2guardedSeqs next (o ()))
            | _ -> []
    
        let seqFilter: list<string> -> string[] = Set >> Set.toArray
    
        let markGraph (vc: int) (mapInput: Map<int * int, Nuc>) (markers: list<int * int>): Graph =
            {
                countOfVertex = vc
                edges =
                    mapInput
                    |> Map.toArray
                    |> Array.map (fun ((s, t), tok) -> (s, t, tok, List.contains (s, t) markers))
            }

//************BioGraph remoting block************//

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
