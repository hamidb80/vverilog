import std/[sequtils, options, strutils, strformat]
import ./lexer, ./conventions

type
  VerilogDeclareKinds* = enum
    vdkInput
    vdkOutput
    vdkInOut
    vdkReg
    vdkWire
    vdkInteger

  VerilogGroupKinds* = enum
    vskPar
    vskBracket
    vskCurly

  VerilogNumberKinds* = enum
    vnInt
    vnFloat
    vnBinary
    vnHex

  ScopeKinds* = enum
    skAlways
    skForever
    skInitial
    skDelay

  VerilogNodeKinds* = enum
    vnkEmpty

    vnkNumber, vnkString, vnkRange
    vnkSymbol, vnkGroup

    vnkDeclare, vnkDefine, vnkAssign
    vnkCall, vnkInstanciate

    vnkModule, vnkScope
    vnkCase, vnkOf
    vnkElif, vnkElifBranch

    vnkPrefix, vnkInfix, vnkTriplefix
    vnkBracketExpr

    vnkComment
    vnkStmtList

  VerilogNode* {.acyclic.} = ref object
    children*: seq[VerilogNode]
    inline*: bool

    case kind*: VerilogNodeKinds
    of vnkNumber:
      numberKind*: VerilogNumberKinds
      digits*: string

    of vnkString:
      str*: string

    of vnkRange:
      head*, tail*: VerilogNode

    of vnkSymbol:
      symbol*: string

    of vnkCall:
      caller*: VerilogNode

    of vnkDeclare:
      dkind*: VerilogDeclareKinds
      bus*: Option[VerilogNode]
      idents*: seq[VerilogNode]

    of vnkDefine:
      ident*, value*: VerilogNode

    of vnkAssign:
      discard

    of vnkModule:
      name*: VerilogNode
      params*: seq[VerilogNode]

    of vnkScope:
      scope*: ScopeKinds
      input*: Option[VerilogNode]

    of vnkInstanciate:
      module*, instanceIdent*: VerilogNode

    of vnkCase:
      select*: VerilogNode

    of vnkOf:
      comparator*: VerilogNode

    of vnkElif:
      discard

    of vnkElifBranch:
      discard

    of vnkPrefix, vnkInfix, vnkTriplefix:
      operator*: string

    of vnkGroup:
      groupKind: VerilogGroupKinds

    of vnkBracketExpr:
      lookup*: VerilogNode
      index*: VerilogNode

    of vnkComment:
      comment*: string

    of vnkEmpty, vnkStmtList:
      discard

  VNode* = VerilogNode

  VerilogAST* = tuple
    header: string
    nodes: seq[VNode]


  ParserState = enum
    psTopLevel
    psModuldeIdent, psModuldeParams, psModuleApplyParams, psModuleBody, psModuleAddBody

    psDeclareStart, psDeclareBus, psDeclareApplyBus, psDeclareIdentDo
    psDeclareIdentCheck, psDeclareAddIdent

    psAssignStart, psAssignEnd

    psDefineStart, psDefineIdent, psDefineValue

    psParStart, psParBody, psParAdd
    psBracketStart, psBracketBody
    psCurlyStart, psCurlyBody

    psBracketExprFinalize

    psPrefixStart, psPrefixEnd
    psInfixStart, psInfixEnd
    psTriplefixStart, psTriplefixEnd

    psExprStart, psExprBody
    psApplyCallArgs

    psInstanciateStart, psInstanciateInstanceIdent
    psInstanciateArgs, psInstanciateEnd

    psIfStart, psElIfBranch, psElifCond, psElIfBody, psElseBody

    psCaseStart, psCaseAddSelect, psCaseMatchExpr, psCaseMatchExprWrapper
    psCaseMatchSep, psCaseMatchBodyWrapper

    psScopeStart, psScopeApplyInput, psScopeInput
    psScopeBodyWrapper, psScopeBodyAdd

    psForStart, psForParOpen, psForInit, psForCond
    psForSteppsForParClose, psForAddBody # TODO

    psBlockStart, psAddSingleStmt, psAddToBlockWrapper, psAddToBlock, psBlockEnd

    # TODO remove unused


func toVSymbol(name: string): VNode =
  VNode(kind: vnkSymbol, symbol: name)

func toVNumber(digits: string): VNode =
  VNode(kind: vnkNumber, digits: digits)

func toVString(s: string): VNode =
  VNode(kind: vnkString, str: s)

func toVNode(token: VToken): VNode =
  case token.kind:
  of vtkKeyword: toVSymbol token.keyword
  of vtkNumber: toVNumber token.digits
  of vtkString: toVString token.content
  of vtkComment:
    VNode(kind: vnkComment, inline: token.inline, comment: token.comment)
  else:
    err "this kind of converting is invalid"


func `$`*(k: VerilogDeclareKinds): string =
  case k:
  of vdkInput: "input"
  of vdkOutput: "output"
  of vdkInOut: "inout"
  of vdkReg: "reg"
  of vdkWire: "wire"
  of vdkInteger: "integer"

func `$`*(k: ScopeKinds): string =
  case k:
  of skAlways: "always"
  of skForever: "forever"
  of skInitial: "initial"
  of skDelay: "#"


const indentSize = 4

func needsSemiColon(vn: VNode): bool =
  vn.kind notin {vnkScope, vnkCase, vnkOf, vnkStmtList, vnkElif, vnkDefine, vnkElifBranch}

template toValidNodeStyleStr(vn, depth): untyped =
  let t =
    if vn.needsSemiColon: toString(vn) & ';'
    else: toString vn

  t.indent(depth * indentSize)


func toString(s: string, depth: int): string =
  indent s, depth * indentSize

func toString(vns: seq[VNode], depth: int): string

func toString(vn: VNode, depth: int = 0): string =
  let t =
    case vn.kind:

    of vnkNumber: vn.digits
    of vnkString: '"' & vn.str & '"'
    of vnkSymbol: vn.symbol

    of vnkRange:
      toString(vn.head) & ':' & toString(vn.tail)

    of vnkGroup:
      let openClose =
        case vn.groupKind:
        of vskPar: ['(', ')']
        of vskBracket: ['[', ']']
        of vskCurly: ['{', '}']

      openClose[0] & vn.children.mapIt(it.toString).join(", ") & openClose[1]

    of vnkCall:
      toString(vn.caller) & '(' &
      vn.children.mapIt(it.toString).join(", ") & ')'

    of vnkBracketExpr:
      toString(vn.lookup) & '[' & toString(vn.index) & ']'

    of vnkDeclare:
      let b =
        if issome vn.bus: '[' & toString(vn.bus.get) & "] "
          else: ""

      $vn.dkind & ' ' & b & vn.idents.mapIt(toString it).join(", ")

    of vnkDefine:
      "`define " & toString(vn.ident) & ' ' & toString(vn.value)

    of vnkAssign:
      "assign " & toString(vn.children[0])

    of vnkModule:
      "module " & toString(vn.name) &
      '(' & vn.params.mapIt(it.toString).join(", ") & ");\n" &
      vn.children[0].children.toString(depth+1) &
      "\nendmodule"

    of vnkPrefix:
      vn.operator & toString(vn.children[0])

    of vnkInfix:
      toString(vn.children[0]) & ' ' &
      vn.operator & ' ' &
      toString(vn.children[1])

    of vnkTriplefix:
      toString(vn.children[0]) & ' ' &
      vn.operator & ' ' &
      toString(vn.children[1])

    of vnkInstanciate:
      toString(vn.module) & ' ' & toString(vn.instanceIdent) &
      '(' & vn.children.mapIt(it.toString).join(", ") & ")"

    of vnkElif:
      var acc: seq[string]

      for i, br in vn.children:
        let prefix =
          if i == 0: indent("if", depth)
          elif br.children.len == 1: indent("else ", depth)
          else: indent("else if", depth)

        acc.add prefix & toString(br, depth)

      acc.join '\n' & indent("", depth)

    of vnkElifBranch:
      if vn.children.len == 2:
        toString(vn.children[0], depth) & ' ' & toString(vn.children[1], depth)
      else:
        toString(vn.children[0], depth)

    of vnkStmtList:
      if vn.children.len == 1 and vn.children[0].kind != vnkElif:
        toValidNodeStyleStr(vn.children[0], 0)

      else:
        "begin\n" & vn.children.toString(depth+1) &
        '\n' & toString("end", depth)

    of vnkCase:
      toString("case", depth) & ' ' & toString(vn.select) & '\n' &
      vn.children.toString(depth+1) &
      '\n' & toString("endcase", depth)

    of vnkOf:
      toString(vn.comparator) & ": " & toString(vn.children, depth)

    of vnkScope:
      let
        inp =
          if issome vn.input:
            toString(vn.input.get)
          else:
            ""

      $vn.scope & inp & ' ' & toString(vn.children[0], depth)

    of vnkComment:
      if vn.inline:
        "//" & vn.comment
      else:
        "/*" & vn.comment & "*/"

    of vnkEmpty: ""

  indent t, depth * indentSize

func toString(vns: seq[VNode], depth: int): string =
  vns.mapIt(toValidNodeStyleStr(it, depth)).join("\n")

func `$`*(vn: VNode): string =
  toString vn


func getAST(vn: VNode): VerilogAST =
  case vn.kind:
  of vnkDeclare:
    let bs =
      if issome vn.bus: @[vn.bus.get]
      else: @[]

    (fmt"Declare {vn.dkind}", bs & vn.idents)

  of vnkDefine:
    ("Define", @[vn.ident, vn.value])

  of vnkAssign:
    ("Assign", vn.children)

  of vnkInstanciate:
    (fmt"Instanciate", @[vn.module, vn.instanceIdent] & vn.children)

  of vnkModule:
    (fmt"Module", @[vn.name] & vn.params & vn.children)

  of vnkEmpty: ("Empty", @[])
  of vnkNumber: (fmt"Number {vn.digits}", @[])
  of vnkString: (fmt"String {vn}", @[])
  of vnkSymbol: (fmt"Symbol {vn}", @[])

  of vnkRange:
    ("Range", @[vn.head, vn.tail])

  of vnkBracketExpr:
    ("BracketExpr", @[vn.lookup, vn.index])

  of vnkCall:
    ("Call", @[vn.caller] & vn.children)

  of vnkGroup:
    (fmt"Group {vn.groupkind}", vn.children)

  of vnkScope:
    #TODO input
    let inp =
      if issome vn.input:
        $vn.input.get
      else:
        ""

    (fmt"Scope {vn.scope} {inp}", vn.children)

  of vnkCase:
    ("Case", @[vn.select] & vn.children)

  of vnkOf:
    ("Of", @[vn.comparator] & vn.children)

  of vnkElif:
    ("ElIf", vn.children)

  of vnkElifBranch:
    ("ElIfBr", vn.children)

  of vnkStmtList:
    ("StmtList", vn.children)

  of vnkPrefix:
    (fmt"Prefix {vn.operator}", vn.children)

  of vnkInfix:
    (fmt"Infix {vn.operator}", vn.children)

  of vnkTriplefix:
    (fmt"Triplefix {vn.operator}", vn.children)

  of vnkComment:
    (fmt"Comment inline?: {vn.inline}", @[toVString vn.comment])


func treeRepr(vast: VerilogAST, depth: int, result: var seq[string]) =
  result.add indent(vast.header, depth * indentSize)

  for n in vast.nodes:
    treeRepr n.getAST, depth + 1, result

func treeRepr*(vn: VNode): string =
  var acc: seq[string]
  treeRepr vn.getAST, 0, acc
  acc.join "\n"



func toDeclareKind(s: string): VerilogDeclareKinds =
  case s:
  of "input": vdkInput
  of "output": vdkOutput
  of "inout": vdkInOut
  of "reg": vdkReg
  of "wire": vdkWire
  of "integer": vdkInteger
  else: err "invalid declaration type"

func toScopeKind(s: string): ScopeKinds =
  case s:
  of "always": skAlways
  of "forever": skForever
  of "initial": skInitial
  else: err "invalid scope name"

func isEmpty(node: VNode): bool =
  node.kind == vnkEmpty


template genController(varname): untyped =
  template follow(v): untyped {.dirty.} =
    varname.add v

  template back: untyped {.dirty.} =
    del varname, varname.high

  template switch(v): untyped {.dirty.} =
    back
    follow(v)

func parseVerilogImpl(tokens: seq[VToken]): seq[VNode] =
  var
    i = 0
    nodeStack: seq[VNode]
    stateStack: seq[ParserState] = @[psTopLevel]

  genController stateStack

  while i < tokens.len:
    let ct = tokens[i] # current token
    if ct.kind == vtkComment:
      # debugEcho ">>> SKIPPED"
      inc i
      continue

    else:
      # debugecho " - - - - - - - - - - - - - - - - - "
      # debugecho ct
      # debugecho "/ ", stateStack.join" / "
      # debugecho "> ", nodeStack.mapIt(it.kind).join" > "
      discard

    ## every part must set the `i`(index) after his last match
    case stateStack.last:
      of psTopLevel:
        matchVtoken ct:
        of kw "module":
          nodeStack.add VNode(kind: vnkModule)
          stateStack.add psModuldeIdent
          inc i

        of kw"`define`", kw"`timescale":
          err "not defened"

        else: err "not implemented: " & $ct


      of psModuldeIdent:
        matchVtoken ct:
        of kw:
          nodeStack.last.name = toVNode ct
          switch psModuldeParams
          inc i

        else:
          err "expected module ident"

      of psModuldeParams:
        matchVtoken ct:
        of g vgcOpenPar:
          follow psModuleApplyParams
          follow psParStart

        of w skSemiColon:
          nodeStack.add VNode(kind: vnkStmtList)
          switch psModuleBody
          inc i

        else:
          err "invalid token"

      of psModuleApplyParams:
        let p = nodestack.pop
        nodestack.last.params = p.children
        back

      of psModuleBody:
        follow psModuleAddBody

        matchVtoken ct:
        of kw"input", kw"output", kw"inout", kw"wire", kw"reg", kw"integer":
          follow psDeclareStart

        of kw"assign":
          follow psAssignStart

        of kw"`define":
          follow psDefineStart

        of kw"always", kw"initial":
          follow psScopeStart

        of kw"endmodule":
          let p = nodeStack.pop
          nodeStack.last.children.add p
          result.add nodeStack.pop
          back
          back
          inc i

        else: # instantiation
          if ct.kind == vtkKeyword:
            follow psInstanciateStart
          else:
            err "expected module name, got: " & $ct

      of psModuleAddBody:
        let p = nodeStack.pop
        nodestack.last.children.add p
        back

      # ------------------------------------

      of psDeclareStart:
        nodeStack.add VNode(kind: vnkDeclare, dkind: toDeclareKind ct.keyword)
        switch psDeclareBus
        inc i

      of psDeclareBus:
        matchVtoken ct:
        of g vgcOpenBracket:
          switch psDeclareApplyBus
          follow psBracketStart

        else:
          switch psDeclareIdentDo

      of psDeclareApplyBus:
        let p = nodestack.pop
        nodestack.last.bus = some p

        switch psDeclareIdentDo

      of psDeclareIdentDo:
        follow psDeclareAddIdent
        follow psExprStart

      of psDeclareIdentCheck:
        matchVToken ct:
        of w skComma:
          switch psDeclareIdentDo
          inc i

        of w skSemiColon:
          back
          inc i

        else:
          err "invalid syntax" & $ct

      of psDeclareAddIdent:
        let p = nodestack.pop
        nodestack.last.idents.add p
        back
        switch psDeclareIdentCheck


      of psAssignStart:
        nodestack.add VNode(kind: vnkAssign)
        switch psAssignEnd
        follow psExprStart
        inc i

      of psAssignEnd:
        matchVToken ct:
        of w skSemiColon:
          let p = nodeStack.pop
          nodeStack.last.children.add p

          back
          inc i

        else:
          err "expected ; got:" & $ct


      of psDefineStart:
        nodestack.add VNode(kind: vnkDefine)
        switch psDefineIdent
        inc i

      of psDefineIdent:
        matchVToken ct:
        of kw:
          nodestack.last.ident = toVNode ct
          switch psDefineValue
          follow psExprStart
          inc i

        else:
          err "exptect and identifier. got: " & $ct

      of psDefineValue:
        let p = nodestack.pop
        nodestack.last.value = p
        back

      # ------------------------------------

      of psParStart:
        matchVtoken ct:
        of g vgcOpenPar:
          nodeStack.add VNode(kind: vnkGroup, groupkind: vskPar)
          switch psParBody

        else:
          err "invalid"

      of psParBody:
        matchVtoken ct:
        of g vgcOpenPar:
          # nodeStack.add Vnode(kind: vnkEmpty)
          follow psParAdd
          follow psExprStart

        of w skComma:
          follow psParAdd
          follow psExprStart

        of g vgcClosePar:
          back

        else:
          err "invalid syntax"

        inc i

      of psParAdd:
        let p = nodeStack.pop
        if not isEmpty p:
          nodeStack.last.children.add p
        back


      of psCurlyStart:
        matchVtoken ct:
        of g vgcOpenCurly:
          nodeStack.add VNode(kind: vnkGroup, groupkind: vskCurly)
          switch psCurlyBody
          inc i

        else:
          err "invalid"

      of psCurlyBody:


        matchVtoken ct:
        of w skComma:
          let p = nodeStack.pop
          nodeStack.last.children.add p

          follow psExprStart
          inc i

        of g vgcCloseCurly:
          let p = nodeStack.pop
          nodeStack.last.children.add p

          back
          inc i

        else:
          follow psExprStart


      of psBracketStart:
        matchVtoken ct:
        of g vgcOpenBracket:
          nodeStack.add VNode(kind: vnkGroup, groupkind: vskBracket)
          switch psBracketBody
          follow psExprStart
          inc i

        else:
          err "invalid"

      of psBracketBody:
        matchVtoken ct:
        of g vgcCloseBracket:
          # group/number | group/range/number
          let
            ex = nodeStack.pop
            up = nodeStack.pop

          if up.kind == vnkRange:
            up.tail = ex
            discard nodeStack.pop
            nodeStack.add up

          else:
            nodeStack.add ex


          back

        of w skColon:
          let p = nodestack.pop
          nodestack.add VNode(kind: vnkRange, head: p)
          follow psExprStart

        else:
          follow psExprStart

        inc i

      # TODO always without arg

      # ------------------------------------

      of psInstanciateStart:
        nodestack.add VNode(kind: vnkInstanciate, module: toVNode ct)
        switch psInstanciateInstanceIdent
        inc i

      of psInstanciateInstanceIdent:
        nodeStack.last.instanceIdent = toVNode ct
        switch psInstanciateArgs
        follow psParStart
        inc i

      of psInstanciateArgs:
        let p = nodeStack.pop
        nodeStack.last.children = p.children
        switch psInstanciateEnd

      of psInstanciateEnd:
        matchVtoken ct:
        of w skSemiColon:
          inc i
          back
        else:
          err "expected ; got: " & $ct

      # ------------------------------------

      of psScopeStart:
        let temp = block:
          matchVtoken ct:
          of kw:
            let sk = toScopeKind ct.keyword
            VNode(kind: vnkScope, scope: sk)

          of o "#":
            VNode(kind: vnkScope, scope: skDelay, inline: true)

          else:
            err "what?"

        nodeStack.add temp

        switch:
          case temp.scope:
          of skAlways, skDelay: psScopeInput
          else: psScopeBodyWrapper

        inc i

      of psScopeInput:
        switch psScopeApplyInput
        follow psExprStart

      of psScopeApplyInput:
        let p = nodeStack.pop
        nodestack.last.input = some p

        switch psScopeBodyWrapper

      of psScopeBodyWrapper:
        switch psScopeBodyAdd
        follow psBlockStart

      of psScopeBodyAdd:
        let p = nodestack.pop
        nodeStack.last.children.add p
        back


      of psBlockStart:
        nodeStack.add VNode(kind: vnkStmtList)

        matchVToken ct:
        of kw"begin":
          switch psAddToBlockWrapper

        else:
          switch psBlockEnd
          follow psAddSingleStmt
          follow psExprStart

      of psAddToBlockWrapper:
        matchVtoken ct:
        of kw"end":
          switch psBlockEnd

        of w skSemiColon, kw"begin":
          inc i

        else:
          follow psAddToBlock
          follow psExprStart

      of psAddToBlock:
        let p = nodestack.pop
        nodeStack.last.children.add p
        back

      of psAddSingleStmt:
        let p = nodeStack.pop
        nodeStack.last.children.add p
        back

      of psBlockEnd:
        matchVtoken ct:
        of w skSemiColon, kw"end", kw"endcase":
          back
          inc i

        else:
          back

      # ------------------------------------

      of psCaseStart:
        nodeStack.add VNode(kind: vnkCase)
        switch psCaseAddSelect
        follow psExprStart
        inc i

      of psCaseAddSelect:
        let p = nodeStack.pop
        nodeStack.last.select = p
        switch psCaseMatchExpr

      of psCaseMatchExpr:
        matchVtoken ct:
        of kw"endcase":
          back
          inc i

        else:
          nodeStack.add VNode(kind: vnkOf)
          switch psCaseMatchExprWrapper
          follow psExprStart

      of psCaseMatchExprWrapper:
        let p = nodeStack.pop
        nodeStack.last.comparator = p
        switch psCaseMatchSep

      of psCaseMatchSep:
        matchVtoken ct:
        of w skColon:
          switch psCaseMatchBodyWrapper
          follow psBlockStart
          inc i

        else:
          err "expected : got: " & $ct

      of psCaseMatchBodyWrapper:
        let stmt = nodestack.pop
        nodestack.last.children.add stmt

        let cOf = nodestack.pop
        nodestack.last.children.add cOf

        switch psCaseMatchExpr

      # ------------------------------------

      of psIfStart:
        nodeStack.add VNode(kind: vnkElif)
        switch psElIfBranch

      of psElIfBranch:
        nodeStack.add VNode(kind: vnkElifBranch)

        matchVtoken ct:
        of kw"if":
          switch psElifCond
          follow psParStart

        of kw"else":
          switch psElIfBody
          follow psBlockStart

        else:
          err "expected keyword if, got: " & $ct

        inc i

      of psElifCond:
        let p = nodestack.pop
        nodeStack.last.children.add p
        switch psElIfBody
        follow psBlockStart

      of psElIfBody:
        let stmt = nodestack.pop
        nodeStack.last.children.add stmt

        let br = nodestack.pop
        nodeStack.last.children.add br

        matchVtoken ct:
        of kw"else":
          inc i
          switch psElseBody
        else:
          back

      of psElseBody:
        matchVtoken ct:
        of kw"if":
          switch psElIfBranch
        else:
          nodeStack.add VNode(kind: vnkElifBranch)
          switch psElIfBody
          follow psBlockStart

      # ------------------------------------

      of psForStart:
        discard

      of psForParOpen:
        discard

      of psForInit:
        discard

      of psForCond:
        discard

      of psForSteppsForParClose:
        discard

      of psForAddBody:
        discard

      # ------------------------------------

      of psExprStart:
        matchVtoken ct:
        of kw"case":
          switch psCaseStart

        of kw"if":
          switch psIfStart

        of g vgcOpenPar:
          switch psExprBody
          follow psParStart

        of g vgcOpenCurly:
          switch psExprBody
          follow psCurlyStart

        of g vgcOpenBracket:
          switch psBracketStart

        of o, kw"posedge":
          switch:
            if matchOperator(ct, "#"): psScopeStart
            else: psPrefixStart

        else:
          if ct.kind in {vtkNumber, vtkString, vtkKeyword}:
            nodeStack.add toVNode ct
            switch psExprBody
            inc i

          else:
            nodeStack.add VNode(kind: vnkEmpty)
            back

      of psExprBody:
        matchVtoken ct:
        of g vgcOpenBracket:
          let p = nodestack.pop
          nodeStack.add VNode(kind: vnkBracketExpr, lookup: p)
          follow psBracketExprFinalize
          follow psExprStart

        of g vgcOpenPar:
          let p = nodestack.pop
          nodestack.add VNode(kind: vnkCall, caller: p)
          follow psApplyCallArgs
          follow psParStart

        of o, kw"or":
          switch psInfixStart

        of w skColon: # `?:` inline ifelse operator
          let ln = nodestack[^2]
          if ln.kind == vnkInfix and ln.operator == "?":
            switch psTriplefixStart
          else:
            back

        else: back

      of psApplyCallArgs:
        let p = nodestack.pop
        nodestack.last.children = p.children
        back

      of psBracketExprFinalize:
        let p = nodeStack.pop
        nodestack.last.index = p
        back

      # ------------------------------------

      of psPrefixStart:
        nodeStack.add VNode(kind: vnkPrefix, operator: getStrVal ct)
        switch psPrefixEnd
        follow psExprStart
        inc i

      of psPrefixEnd:
        let p = nodeStack.pop
        nodeStack.last.children.add p
        back


      of psInfixStart:
        let p = nodestack.pop
        nodeStack.add VNode(kind: vnkInfix, operator: getStrVal ct, children: @[p])
        switch psInfixEnd
        follow psExprStart
        inc i

      of psInfixEnd:
        let p = nodeStack.pop
        nodeStack.last.children.add p
        back


      of psTriplefixStart:
        let p = nodestack.pop
        nodeStack.add VNode(kind: vnkTriplefix,
            operator: $fromSep(ct.sepKind), children: @[p])

        switch psTriplefixEnd
        follow psExprStart
        inc i

      of psTriplefixEnd:
        let p = nodeStack.pop
        nodeStack.last.children.add p
        back


  # debugecho "~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~"
  # debugecho nodestack.mapIt it.kind

func parseVerilog*(content: string): seq[VNode] =
  let tokens = toseq extractVerilogTokens content
  parseVerilogImpl tokens
