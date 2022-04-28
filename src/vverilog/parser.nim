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
    vskBeginEnd
    vskPar
    vskBracket
    vsk

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

    psDefine, psDeclare, psAsgn
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

func `$`(node: VNode): string =
  case node.kind:
  of vnkSymbol: node.symbol
  of vnkBracketExpr: fmt"{node.lookup}[{node.index}]"
  of vnkRange: fmt"{node.head}:{node.tail}"
  of vnkNumber: node.digits
  of vnkModule: $node.name & '(' & node.params.join(", ") & ')'
  else:
    err fmt"invalid conversion to string. kind: {node.kind}"


func pull(s: var seq) =
  del s, s.high

func pull[T](s: var seq[T], v: T) =
  s[^1] = v


func toDeclareKind(s: string): VerilogDeclareKinds =
  case s:
  of "input": vdkInput
  of "output": vdkOutput
  of "inout": vdkInOut
  of "reg": vdkReg
  of "wire": vdkWire
  else: err "invalid declare type"


func parseVerilogImpl(tokens: seq[VToken]): seq[VNode] =
  var
    i = 0
    nodeStack: seq[VNode]
    stateStack: seq[ParserState] = @[psTopLevel]

  while i < tokens.len:
    let ct = tokens[i] # current token
    debugecho ct, stateStack

    case stateStack.last:
    of psTopLevel:
      matchVtoken ct:
      of kw "module":
        nodeStack.add VNode(kind: vnkModule)
        stateStack.add psModuldeIdent
        inc i

      else: err "not implemented: " &  $ct

    of psModuldeIdent:
      assert ct.kind == vtkKeyword
      nodeStack.last.name = toVNode ct

      pull stateStack

      assert tokens[i+1].isGroup '('
      stateStack.add [psModuldeParams, psIdentDef]

      inc i, 2

    of psModuldeParams:
      if ct.isSep ',':
        let t = nodeStack.pop
        nodeStack[^1].body.add t
        inc i

      elif ct.isGroup ')':
        let t = nodeStack.pop
        nodeStack[^1].body.add t 
        inc i
        pull stateStack, psModuleBody

      elif ct.kind == vtkKeyword:
        stateStack.pull psIdentDef

      else:
        err "invalid token"

    of psIdentDef:
      if ct.kind == vtkKeyword:
        nodeStack.add toVNode ct
        stateStack.pull psIdentDefBus
      else:
        err "invalid ident"

    of psIdentDefBus:
      if ct.isGroup '[':
        let t = nodeStack.pop
        nodeStack.add VNode(kind: vnkBracketExpr, lookup: t)
        stateStack.pull psBracketExpr
      else:
        pull stateStack

      inc i

    of psBracketExpr:
      assert tokens[i+1].isSep ':'
      nodeStack[^1].index = VNode(kind: vnkRange,
        head: toVnode ct,
        tail: toVNode tokens[i+2])

      pull stateStack
      inc i, 4

    of psModuleBody:
      matchVtoken ct:
      of kw"endmodule":
        stateStack.pull
        result.add nodeStack.pop
        inc i

      of kw"input", kw"output", kw"inout", kw"wire", kw"reg":
        # toDeclareKind ct.keyword
        # inc i
        discard

    else: err "why?"

func parseVerilog*(content: string): seq[VNode] =
  let tokens = toseq extractVerilogTokens content
  parseVerilogImpl(tokens)
