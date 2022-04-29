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

    psDefine, psAsgn
    psDeclareIdent, psDeclareBus

    psInstanceName, psInstanceArgs
    psScopeBody, psScopeArgs # always @ (...)


    psIdentDef, psIdentDefBus
    psRange,

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


func push[T](s: var seq[T], v: T) =
  debugecho "<<< added ", v
  s.add v

func pull[T](s: var seq[T]) =
  debugecho ">>> pulled ", s[s.high]
  del s, s.high

func pullPush[T](s: var seq[T], v: T) =
  pull s
  push s,v


func parseVerilogImpl(tokens: seq[VToken]): seq[VNode] =
  var
    i = 0
    nodeStack: seq[VNode]
    stateStack: seq[ParserState] = @[psTopLevel]

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

      pull stateStack

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
        stateStack.pullPush psModuleBody

        if nodeStack[^1].kind in {vnkSymbol, vnkBracketExpr}:
          let p = nodeStack.pop
          nodeStack.last.params.add p

        assert tokens[i+1].isSep ';'
        inc i, 2

      of kw:
        stateStack.add psIdentDef

      else:
        err "invalid token"

    of psIdentDef:
      matchVtoken ct:
      of kw:
        nodeStack.add toVNode ct
        stateStack.pullPush psIdentDefBus
        inc i
      
      else:
        err "invalid ident"

    of psIdentDefBus:
      matchVtoken ct:
      of g vgcOpenBracket:
        nodeStack.add VNode(kind: vnkBracketExpr, lookup: nodeStack.pop)
        stateStack.pullPush psBracketExpr
        inc i

      of g vgcCloseBracket:
        stateStack.pull
        inc i

      else:
        pull stateStack

    of psBracketExpr:
      assert tokens[i+1].isSep ':'
      nodeStack[^1].index = VNode(kind: vnkRange,
        head: toVnode ct,
        tail: toVNode tokens[i+2])

      stateStack.pullPush psIdentDefBus
      inc i, 3

    of psDeclareIdent:
      matchVtoken ct:
      of kw:
        nodestack.last.ident = toVNode ct
        stateStack.pullPush psDeclareBus

      else: err "what"

      inc i

    of psDeclareBus:
      matchVtoken ct:
      of w skSemiColon:
        nodestack.last.body.add nodestack.pop
        stateStack.pull

      else: err "what"

      inc i

    of psModuleBody:
      matchVtoken ct:
      of kw"endmodule":
        result.add nodeStack.pop
        inc i

      of kw"input", kw"output", kw"inout", kw"wire", kw"reg":
        nodeStack.add VNode(kind: vnkDeclare, dkind: toDeclareKind ct.keyword)
        stateStack.add psDeclareIdent

        inc i

    else: err "why?"

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
  of vnkDeclare: $vn.dkind & ' ' & $vn.ident
  of vnkBracketExpr: fmt"{vn.lookup}[{vn.index}]"

  of vnkModule: "module " & $vn.name & '(' & vn.params.join(", ") & ");\nendmodule"

  # of vnkDefine:
  # of vnkAsgn:
  # of vnkInstantiate:
  # of vnkModule:
  # of vnkScope:
  # of vnkCase:
  # of vnkOf:
  # of vnkElif:
  # of vnkInfix:
  # of vnkPrefix:
  # of vnkBracketExpr:

  of vnkComment:
    if vn.inline:
      "//" & vn.comment
    else:
      "/*" & vn.comment & "*/"

  else:
    err "wow"
