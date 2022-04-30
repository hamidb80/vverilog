import std/[sequtils, options, strutils, strformat]
import ./lexer, ./conventions

type
  VerilogDeclareKinds* = enum
    vdkInput
    vdkOutput
    vdkInOut
    vdkReg
    vdkWire

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

  VerilogNodeKinds* = enum
    vnkNumber, vnkString, vnkRange
    vnkSymbol, vnkGroup

    vnkCall, vnkAction # dumpvars;

    vnkDeclare, vnkDefine, vnkAsgn, vnkInstantiate

    vnkModule, vnkScope
    vnkCase, vnkOf, vnkElif

    vnkInfix, vnkPrefix
    vnkBracketExpr

    vnkComment

  VerilogNode* = ref object
    body*: seq[VerilogNode]

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

    of vnkAction:
      action*: VerilogNode

    of vnkDeclare, vnkDefine:
      dkind*: VerilogDeclareKinds
      bus*: Option[VerilogNode]
      ident*: VerilogNode

    of vnkAsgn:
      container*, newValue*: VerilogNode

    of vnkModule:
      name*: VerilogNode
      params*: seq[VerilogNode]

    of vnkScope:
      scope: ScopeKinds

    of vnkInstantiate:
      module*, instance*: VerilogNode

    of vnkCase:
      select*: VerilogNode

    of vnkOf:
      comparator*: Option[VerilogNode]

    of vnkElif:
      condition*: Option[VerilogNode]

    of vnkInfix, vnkPrefix:
      operator*: VerilogNode

    of vnkGroup:
      groupKind: VerilogGroupKinds

    of vnkBracketExpr:
      lookup*: VerilogNode
      index*: VerilogNode

    of vnkComment:
      comment*: string
      inline*: bool

  VNode* = VerilogNode

  ParserState = enum
    psTopLevel
    psModuldeIdent, psModuldeParams, psModuleBody

    psDeclareStart, psDeclareIdent, psDeclareBus, psDeclareEnd

    psDefine, psAsgn


    psInstanceName, psInstanceArgs
    psScopeBody, psScopeArgs # always @ (...)


    psIdentDef, psIdentDefBus, psDeclareArray
    psRange

    psBracket, psBracketExpr
    psPar, psCurly
    psIfCond, psIfBody, psElseBody
    psCaseParam, psCaseOfParam, psCaseOfBody

    psEq, psPrefix, psInfix


func toVSymbol(name: string): VNode =
  VNode(kind: vnkSymbol, symbol: name)

func toVNumber(digits: string): VNode =
  VNode(kind: vnkNumber, digits: digits)

func toVNode(token: VToken): VNode =
  case token.kind:
  of vtkKeyword: toVSymbol token.keyword
  of vtkNumber: toVNumber token.digits
  of vtkComment:
    VNode(kind: vnkComment, inline: token.inline, comment: token.comment)
  else:
    err "this kind of converting is invalid"

func toDeclareKind(s: string): VerilogDeclareKinds =
  case s:
  of "input": vdkInput
  of "output": vdkOutput
  of "inout": vdkInOut
  of "reg": vdkReg
  of "wire": vdkWire
  else: err "invalid declare type"

template genController(varname): untyped =
  template follow(v): untyped {.dirty.} =
    debugecho "<<< added ", v
    stateStack.add v

  template back(): untyped {.dirty.} =
    debugecho ">>> backed ", stateStack[stateStack.high]
    del stateStack, stateStack.high

  template switch(v): untyped {.dirty.} =
    back()
    follow(v)


func parseVerilogImpl(tokens: seq[VToken]): seq[VNode] =
  var
    i = 0
    nodeStack: seq[VNode]
    stateStack: seq[ParserState] = @[psTopLevel]

  genController stateStack

  while i < tokens.len:
    let ct = tokens[i] # current token
    debugecho " - - - - - - - - - - - - - - - - - "
    debugecho ct
    debugecho "/ ", stateStack.join" / "
    debugecho "> ", nodeStack.mapIt(it.kind).join" > "
    debugecho ": : : : : :"

    case stateStack.last:

    of psTopLevel:
      matchVtoken ct:
      of kw "module":
        nodeStack.add VNode(kind: vnkModule)
        stateStack.add psModuldeIdent
        inc i

      else: err "not implemented: " & $ct


    of psModuldeIdent:
      assert ct.kind == vtkKeyword
      nodeStack.last.name = toVNode ct

      back()

      assert tokens[i+1].isGroup '('
      stateStack.add [psModuldeParams, psIdentDef]

      inc i, 2

    of psModuldeParams:
      matchVtoken ct:
      of w skComma:
        let p = nodeStack.pop
        nodeStack.last.params.add p
        inc i

      of g vgcClosePar:
        switch psModuleBody

        if nodeStack[^1].kind in {vnkSymbol, vnkBracketExpr}:
          let p = nodeStack.pop
          nodeStack.last.params.add p

        assert tokens[i+1].isSep ';'
        inc i, 2

      of kw:
        follow psIdentDef

      else:
        err "invalid token"

    of psModuleBody:
      matchVtoken ct:
      of kw"endmodule":
        result.add nodeStack.pop
        inc i

      of kw"input", kw"output", kw"inout", kw"wire", kw"reg":
        follow psDeclareStart


    of psDeclareStart:
      nodeStack.add VNode(kind: vnkDeclare, dkind: toDeclareKind ct.keyword)
      switch psDeclareBus
      inc i
    
    of psDeclareBus:
      matchVtoken ct:
      of g vgcOpenBracket:
        follow psRange
        inc i

      of g vgcCloseBracket:
        let p = nodeStack.pop
        nodeStack.last.bus = some p
        inc i

      of kw:
        switch psDeclareIdent

      else: err "what"

    of psDeclareIdent:
      matchVtoken ct:
      of kw:
        nodestack.last.ident = toVNode ct
        switch psDeclareArray

      else: err "what"

      inc i

    of psDeclareArray:
      matchVtoken ct:
      of w skSemiColon:
        switch psDeclareEnd

      of g vgcCloseBracket:
        inc i

      else: err "what"
    
    of psDeclareEnd:
      let p = nodeStack.pop
      nodestack.last.body.add p
      back

      inc i


    of psIdentDef:
      matchVtoken ct:
      of kw:
        nodeStack.add toVNode ct
        switch psIdentDefBus
        inc i

      else:
        err "invalid ident"

    of psIdentDefBus:
      matchVtoken ct:
      of g vgcOpenBracket:
        nodeStack.add VNode(kind: vnkBracketExpr, lookup: nodeStack.pop)
        switch psBracketExpr
        inc i

      of g vgcCloseBracket:
        back
        inc i

      else:
        back

    of psBracketExpr:
      assert tokens[i+1].isSep ':'
      nodeStack[^1].index = VNode(kind: vnkRange,
        head: toVnode ct,
        tail: toVNode tokens[i+2])

      switch psIdentDefBus
      inc i, 3

    of psRange:
      assert tokens[i+1].isSep ':'
      nodeStack.add VNode(kind: vnkRange,
        head: toVnode ct,
        tail: toVNode tokens[i+2])

      back
      inc i, 3
    
    else: err "this parser state is not implemented: " & $stateStack.last

func parseVerilog*(content: string): seq[VNode] =
  let tokens = toseq extractVerilogTokens content
  parseVerilogImpl(tokens)


func `$`*(k: VerilogDeclareKinds): string =
  case k:
  of vdkInput: "input"
  of vdkOutput: "output"
  of vdkInOut: "inout"
  of vdkReg: "reg"
  of vdkWire: "wire"

const IndentSize = 4

func `$`*(vn: VNode): string =
  case vn.kind:

  of vnkNumber: vn.digits
  of vnkString: '"' & vn.str & '"'
  of vnkRange: $vn.head & ':' & $vn.tail
  of vnkSymbol: vn.symbol

  of vnkAction: $vn.action

  of vnkGroup:
    let openClose =
      case vn.groupKind:
      of vskPar: ['(', ')']
      of vskBracket: ['[', ']']
      of vskCurly: ['{', '}']

    openClose[0] & vn.body.join(", ") & openClose[1]

  of vnkCall: $vn.caller & '(' & vn.body.join(", ") & ')'
  of vnkBracketExpr: fmt"{vn.lookup}[{vn.index}]"

  of vnkDeclare:
    let b =
      if issome vn.bus: '[' & $vn.bus.get & "] "
      else: ""

    $vn.dkind & ' ' & b & $vn.ident & ';'

  of vnkModule:
    "module " & $vn.name & '(' & vn.params.join(", ") & ");\n" &
    vn.body.mapIt(indent($it, IndentSize)).join("\n") &
    "\nendmodule"

  of vnkInfix: $vn.body[0] & ' ' & $vn.operator & ' ' & $vn.body[1]
  of vnkPrefix: $vn.operator & $vn.body[0]

  of vnkInstantiate: $vn.module & ' ' & $vn.instance & '(' & vn.body.join(
      ", ") & ");"
  # of vnkElif:

  # of vnkCase:
  # of vnkOf:

  # of vnkDefine:
  # of vnkAsgn:

  of vnkComment:
    if vn.inline:
      "//" & vn.comment
    else:
      "/*" & vn.comment & "*/"

  else:
    err "wow"
