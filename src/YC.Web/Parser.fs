namespace YC.Web

type Nuc = | A | C | G | U

type Edge = int * int * Nuc * bool

type Graph = {
    countOfVertex: int;
    edges: Edge[];
}


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

//=========GraphParsing server-side block=========//
module GraphParsingFunctions =

    type InputEdge = int * int * string * bool
    type InputGraph =
        {
            countOfVertex : int;
            edges : InputEdge[]
        }

    type SppfEdge = int * string * int * string
    type ParsedSppf =
        {
            countOfVertex : int;
            edges : SppfEdge[]
        }

    type Token =
        | Term of string
        | EOF

    let graphParse graph_text =
        let gd = DotParser.parse graph_text
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
                | str -> ignore <| g.AddEdge(new ParserEdge<Token>(!a, !b, Term(str)))
        g
    
    let mutable indToString = fun i -> ""
    let mutable tokenToNumber = fun t -> 0
    let tokenData (t : Token) : obj = null

    let grmParse parser_text = 
        let text = parser_text
        let grm = ParseText text "file.yrd"
        let icg = initialConvert grm
        let fg = FinalGrammar(icg.grammar.[0].rules, true)

        tokenToNumber <- function
            | Term(str) -> fg.indexator.termToIndex str
            | EOF -> fg.indexator.eofIndex

        let genLiteral (s :string) (i : int) : Token option = None
    
        let isLiteral (t : Token) : bool = false
        let isTerminal (t : Token) : bool = true
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
        let numIsTerminal (i : int) : bool = fg.indexator.termsStart <= i && i <= fg.indexator.termsEnd
        let numIsNonTerminal (i : int): bool = fg.indexator.isNonTerm i
        let numIsLiteral (i : int) : bool = fg.indexator.literalsStart <= i && i <= fg.indexator.literalsEnd

        let numToString (n : int) : string =
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

    let unpackExtension(ext : int64<extension>) =
        let ``2^32`` = 256L * 256L * 256L * 256L
        (int <| ext / ``2^32``, int <| ext % (``2^32`` * 1L<extension>))

    
    let graphToMap (inputG: ParserInputGraph<Token>) : Map<(int * int), (string * bool)> =
        inputG.Edges
        |> Seq.filter (fun edge -> edge.Tag <> EOF)
        |> Seq.map (fun edge ->
                    (edge.Source, edge.Target),
                    ((match edge.Tag with Term(str) -> str), false))
        |> Map.ofSeq

    let mapToGraph (graph : Map<(int * int), (string * bool)>) : InputGraph =
        let count = graph.Count
        let edges = graph
                    |> Map.toArray
                    |> Array.map (fun ((s, t), (tok, is)) -> (s, t, tok, is))
        {
            countOfVertex = count
            edges = edges
        }

    let treeToParsed (tree : Tree<Token>) (node : obj) (filter : obj -> bool) : ParsedSppf =
        let mutable edges = []
        let mutable count = 0
        let mutable been = []
        let mutable verts : Map<string, int> = Map.empty
        let rec f (curr : obj) (prev : int * string) : unit =
            if curr <> null then
                match curr with
                | :? TerminalNode as node ->
                    if (filter curr)
                    then
                        let fsti, scnd = unpackExtension(node.Extension)
                        let mutable str = ""
                        if node.Name = -1
                        then
                            str <- "epsilon"
                        else
                            if fsti = -1 && scnd = -1
                            then
                                str <- "dummy"
                            else
                                let getToken =
                                    match tree.tokens.[node.Name] with
                                    | Term(tk) -> tk
                                    | EOF -> "EOF"
                                str <- "Term" + ('\n').ToString() + getToken + " " + (fsti.ToString()) + " " + (scnd.ToString())
                        if List.contains curr been
                        then
                            let tcount = Map.find str verts
                            edges <- List.append edges [(fst prev, snd prev, tcount, str)]
                        else
                            verts <- Map.add str count verts
                            if prev <> (-1, "")
                            then
                                edges <- List.append edges [(fst prev, snd prev, count, str)]
                            count <- count + 1
                            been <- List.append been [node]
                | :? PackedNode as node ->
                    if (filter curr)
                    then
                        let str = "Packed" + " " + (node.Production).ToString()
                        if List.contains curr been
                        then
                            let tcount = Map.find str verts
                            edges <- List.append edges [(fst prev, snd prev, tcount, str)]
                        else
                            let vert = (count, str)
                            verts <- Map.add str count verts
                            if prev <> (-1, "")
                            then
                                edges <- List.append edges [(fst prev, snd prev, fst vert, snd vert)]
                            count <- count + 1
                            been <- List.append been [node]
                            f node.Left vert
                            f node.Right vert
                | :? NonTerminalNode as node ->
                    if (filter curr)
                    then
                        let fsti, scnd = unpackExtension(node.Extension)
                        let str = "NonTerm" + ('\n').ToString() + (indToString node.Name) + " " + (fsti.ToString()) + " " + (scnd.ToString())
                        if List.contains curr been
                        then
                            let tcount = Map.find str verts
                            edges <- List.append edges [(fst prev, snd prev, tcount, str)]                            
                        else
                            let vert = (count, str)
                            verts <- Map.add str count verts
                            if prev <> (-1, "")
                            then
                                edges <- List.append edges [(fst prev, snd prev, fst vert, snd vert)]
                            count <- count + 1
                            been <- List.append been [node]
                            f node.First vert
                            if node.Others <> null
                            then
                                for t in node.Others do
                                    f t vert               
                | :? IntermidiateNode as node ->
                    if (filter curr)
                    then
                        let fsti, scnd = unpackExtension(node.Extension)
                        let str = "Inter" + ('\n').ToString() + (node.Slot).ToString() + " " + (fsti.ToString()) + " " + (scnd.ToString())
                        if List.contains curr been
                        then
                            let tcount = Map.find str verts
                            edges <- List.append edges [(fst prev, snd prev, tcount, str)]                            
                        else
                            let vert = (count, str)
                            verts <- Map.add str count verts
                            if prev <> (-1, "")
                            then
                                edges <- List.append edges [(fst prev, snd prev, fst vert, snd vert)]
                            count <- count + 1
                            been <- List.append been [node]
                            f node.First vert
                            if node.Others <> null
                            then
                                for t in node.Others do
                                    f t vert
        f node (-1, "")
        {
            countOfVertex = count
            edges = List.toArray edges
        }

    let toInputGraph (graph : ParserInputGraph<Token>) : InputGraph =
        mapToGraph (graphToMap graph)

    let getFormalSubgraph (tree : Tree<Token>) (graph : Map<(int * int), (string * bool)>): InputGraph =
        let mutable been : list<obj>= []
        let rec f (graph : Map<(int * int), (string * bool)>) (toks : array<Token> ) (current : obj) =
            if current <> null 
            then
                match current with
                | :? TerminalNode as node ->
                    if List.contains current been
                    then
                        graph
                    else
                        been <- List.append been [current]
                        if node.Name <> -1
                        then
                            if node.Extension <> packExtension -1 -1
                            then
                                let fst, scnd = unpackExtension((node :> INode).getExtension())
                                let tok = toks.[node.Name]
                                match tok with
                                | Term(st) -> 
                                    let str = st
                                    graph.Add ((fst, scnd), (str, true))
                                | EOF -> 
                                    graph
                            else
                                graph
                        else
                            graph
                | :? PackedNode as node ->
                    if List.contains current been
                    then
                        graph
                    else
                        been <- List.append been [current]
                        let l = f graph toks node.Left
                        let r = f l toks node.Right
                        r
                | :? NonTerminalNode as node ->
                    if List.contains current been
                    then
                        graph
                    else
                        been <- List.append been [current]
                        let fst = f graph toks node.First
                        let mutable last = fst
                        let mutable res = fst
                        if node.Others <> null
                        then
                            for t in node.Others do
                                res <- f last toks t
                                last <- res
                        res
                | :? IntermidiateNode as node ->
                    if List.contains current been
                    then
                        graph
                    else
                        been <- List.append been [current]
                        let fst = f graph toks node.First
                        let mutable last = fst
                        let mutable res = fst
                        if node.Others <> null
                        then
                            for t in node.Others do
                                res <- f last toks t
                                last <- res
                        res
                | _ -> graph
            else
                graph
        let edges = f graph tree.tokens tree.Root
        let res = mapToGraph edges
        res

    type ResNode =
    | Suc of NonTerminalNode
    | None
    | Error of string

    
    let getNonTermNode (tree : Tree<Token>) (ext : int64<extension>) : ResNode =
        let mutable been : list<obj> = []
        let rec f (current : obj) =
            if current <> null 
            then
                match current with
                | :? TerminalNode as node ->
                    if not (List.contains current been)
                    then
                        been <- List.append been [node]
                    None
                | :? PackedNode as node ->
                    if List.contains current been
                    then
                        None
                    else
                        been <- List.append been [node]
                        let l = f node.Left
                        if l <> None
                        then
                            l
                        else
                            let r = f node.Right
                            r
                | :? NonTerminalNode as node ->
                    if List.contains current been
                    then
                        None
                    else
                        been <- List.append been [node]
                        if node.Extension = ext
                        then
                            Suc(node)
                        else
                            let fst = f node.First
                            if fst <> None
                            then
                                fst
                            else
                                let mutable is = false
                                let mutable nd = node
                                if node.Others <> null
                                then
                                    for t in node.Others do
                                        let cu = f t
                                        if cu <> None
                                        then
                                            is <- true
                                            match cu with Suc(x) -> nd <- x
                                if is
                                then
                                    Suc(nd)
                                else
                                    None
                | :? IntermidiateNode as node ->
                    if List.contains current been
                    then
                        None
                    else
                        been <- List.append been [node]
                        let fst = f node.First
                        if fst <> None
                            then
                                fst
                            else
                                let mutable is = false
                                let mutable nd = new NonTerminalNode(1, ext)
                                if node.Others <> null
                                then
                                    for t in node.Others do
                                        let cu = f t
                                        if cu <> None
                                        then
                                            is <- true
                                            match cu with Suc(x) -> nd <- x
                                if is
                                then
                                    Suc(nd)
                                else
                                    None
                | _ -> None
            else
                Error "There is no nodes in tree"
        f tree.Root

    let getEdgesOfMinLen (node : NonTerminalNode) =
        let mutable been : list<obj> = []
        let rec f (curr : obj) (map : list<int * int>) (len : int) (nodes : list<obj>) =
            match curr with
                | :? TerminalNode as node ->
                    if not (List.contains curr been)
                    then
                        been <- List.append been [curr]    
                        if node.Name <> -1
                        then
                            if node.Extension <> packExtension -1 -1
                            then
                                (List.append map [(unpackExtension node.Extension)] , len + 1, List.append nodes [node] )
                            else
                                (map, len, nodes)
                        else
                            (map, len, nodes)
                    else
                        (map, len, nodes)
                | :? PackedNode as node ->
                    if not (List.contains curr been)
                    then
                        been <- List.append been [curr]
                        let mapl, lenl, ndsl = f node.Left map len nodes
                        let mapr, lenr, ndsr = f node.Right map len nodes
                        (List.append mapl mapr, lenr + lenl, List.append (List.append ndsl ndsr) [node] )
                    else
                        (map, len, nodes)
                | :? NonTerminalNode as node ->
                    if not (List.contains curr been)
                    then
                        been <- List.append been [curr]
                        let mp, ln, nods = f node.First map len nodes
                        let mutable min = ln
                        let mutable res = mp
                        let mutable ndes = nods
                        if node.Others <> null
                        then
                            for t in node.Others do
                                let mpn, lnn, nds = f t map len nodes
                                if lnn < min
                                then
                                    min <- lnn
                                    res <- mpn
                                    ndes <- nds
                                else do()
                        else do()
                        (res, min, List.append ndes [node])
                    else
                        (map, len, nodes)
                | :? IntermidiateNode as node ->
                    if not (List.contains curr been)
                    then
                        been <- List.append been [curr]
                        let mp, ln, nods = f node.First map len nodes
                        let mutable min = ln
                        let mutable res = mp
                        let mutable ndes = nods
                        if node.Others <> null
                        then
                            for t in node.Others do
                                let mpn, lnn, nds = f t map len nodes
                                if lnn < min
                                then
                                    min <- lnn
                                    res <- mpn
                                    ndes <- nds
                                else do()
                        else do()
                        (res, min, List.append ndes [node])
                    else
                        (map, len, nodes)
        let edges, _, nodes = f node [] 0 []
        (edges, nodes)

    let getTreeOfMnLn (oldTree : Tree<Token>) (edges : list<int * int>) (nodes : list<obj>) (node) (graph : InputGraph) =
        let tree : ParsedSppf =
            (treeToParsed oldTree node (fun x -> List.contains x nodes))
        let markedGraph : InputGraph =
            {
                countOfVertex = graph.countOfVertex
                edges =
                    graph.edges
                    |> Array.map (fun (a, b, c, _) -> (a, b, c, List.contains (a, b) edges) )
            }
        (tree, markedGraph)

    type NotPacked =
        | NonTerminal  of NonTerminalNode
        | Terminal of TerminalNode
        | Intermidiate of IntermidiateNode
    type ResVert =
        | Packed of PackedNode * bool
        | Others of NotPacked * bool * (int * int64<extension>)
        | Error of string

    let getNxtNd (node : NotPacked) : INode =
        match node with
        | NonTerminal(trt) -> trt :> INode
        | Terminal(trt) -> trt :> INode
        | Intermidiate(trt) -> trt :> INode

    let minimiseSppf (tree : Tree<Token>) =
        let rec f (current : obj) (prevAble : bool) (been : list<obj>): ResVert =
            match current with
            | :? TerminalNode as cur ->
                Others(Terminal(cur), true, (cur.Name, cur.Extension))
            | :? PackedNode as cur ->
                if List.contains current been
                then
                    Packed(cur, false)
                else
                    let l = f cur.Left true (List.append been [cur])
                    let r = f cur.Right true (List.append been [cur])
                    match l, r with
                    | Others(vrtl, isl, extl), Others(vrtr, isr, extr) ->
                        if snd extl = packExtension -1 -1 || fst extl = -1 then
                            if prevAble
                            then
                                Others(vrtr, isr, extr)
                            else
                                let trt = getNxtNd vrtr
                                let vrt = new PackedNode(cur.Production, cur.Left, trt)
                                Packed(vrt, true)
                        elif snd extr = packExtension -1 -1 || fst extr = -1 then
                            if prevAble
                            then
                                Others(vrtl, isl, extl)
                            else
                                let trt = getNxtNd vrtl
                                let vrt = new PackedNode(cur.Production, trt, cur.Right)
                                Packed(vrt, true)
                        else
                            if isl || isr
                            then
                                let vrt = new PackedNode(cur.Production, (if isl then getNxtNd vrtl else cur.Left), (if isr then getNxtNd vrtr else cur.Right) )
                                Packed (vrt, true)
                            else
                                Packed (cur, false)
                    | Packed(vrtl, isl), Others(vrtr, isr, extr) ->
                        if snd extr = packExtension -1 -1 || fst extr = -1
                        then
                            l
                        else
                            let vrt = new PackedNode(cur.Production, vrtl, (if isr then getNxtNd vrtr else cur.Right) )
                            Packed (vrt, true)
                    | Others(vrtl, isl, extl), Packed(vrtr, isr) ->
                        if snd extl = packExtension -1 -1 || fst extl = -1
                        then
                            r
                        else
                            let vrt = new PackedNode(cur.Production, (if isl then getNxtNd vrtl else cur.Left), vrtr )
                            Packed (vrt, true)
                    | Packed(vrtl, isl), Packed(vrtr, isr) ->
                        let vrt = new PackedNode(cur.Production, vrtl, vrtr)
                        Packed (vrt, true)
            | :? NonTerminalNode as cur ->
                if List.contains current been
                then
                    Others(NonTerminal(cur), true, (cur.Name, cur.Extension))
                else
                    let fs = f cur.First false (List.append been [cur])
                    match fs with
                    | Packed(pckd, is) ->
                        if is
                        then
                            cur.First <- pckd
                    if cur.Others <> Unchecked.defaultof<_>
                    then
                        for i in 0..cur.Others.Count - 1 do
                            let rs = f (cur.Others.Item i) false (List.append been [cur])
                            match rs with
                            | Packed(pckd, is) ->
                                if is
                                then
                                    cur.Others.RemoveAt i
                                    cur.Others.Insert(i,pckd)
                    Others(NonTerminal(cur), true, (cur.Name, cur.Extension))
            | :? IntermidiateNode as cur ->
                if List.contains current been
                then
                    Others(Intermidiate(cur), true, (cur.Slot, cur.Extension))
                else
                    if cur.Others = Unchecked.defaultof<_>
                    then
                        let res = f cur.First true (List.append been [cur])
                        match res with
                        | Others(_, _, _) ->
                            res
                        | Packed(vrt, is) ->
                            if is
                            then res
                            else Packed(vrt, true)                        
                    else
                        let fs = f cur.First false (List.append been [cur])
                        match fs with
                        | Packed(pckd, is) ->
                            if is
                            then
                                cur.First <- pckd
                        if cur.Others <> Unchecked.defaultof<_>
                        then
                            for i in 0..cur.Others.Count - 1 do
                                let rs = f (cur.Others.Item i) false (List.append been [cur])
                                match rs with
                                | Packed(pckd, is) ->
                                    if is
                                    then
                                        cur.Others.RemoveAt i
                                        cur.Others.Insert(i,pckd)
                        Others (Intermidiate(cur), true, (cur.Slot, cur.Extension))
        f tree.Root false [] |> ignore
        tree

//=========BioGraph server-side block=========//
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
