import std/[sequtils, options]
import ./lexer

type
  VerilogDeclareKinds* = enum
    vdkInput
    vdkOutput
    vdkInOut
    vdkReg
    vdkWire

  VerilogNodeKinds* = enum
    vnkNormNumber, vnkBinNumber, vnkHexNumber, vnkString

    vnkSymbol, vnkScope, vnkInstantiate
    vnkDeclare, vnkDefine, vnkAsgn

    vnkModule
    vnkCase, vnkOf
    vnkElif
    vnkInfix, vnkPrefix

    vnkPar, vnkCurlyGroup
    vnkBraketRange
    vnkBracketExpr

    vnkComment


  ScopeKinds* = enum
    skAlways
    skForever
    skInitial

  VerilogNode = ref object
    body: seq[VerilogNode]

    case kind: VerilogNodeKinds
    of vnkNormNumber, vnkBinNumber, vnkHexNumber:
      digits: string

    of vnkString:
      str: string

    of vnkSymbol:
      symbol: string

    of vnkScope:
      scope: ScopeKinds

    of vnkDeclare, vnkDefine:
      ident, bitRange, value: VerilogNode

    of vnkAsgn:
      container, newValue: VerilogNode

    of vnkModule:
      name: VerilogNode
      params: seq[VerilogNode]

    of vnkInstantiate:
      module, instance: VerilogNode

    of vnkCase:
      select: VerilogNode

    of vnkOf:
      comparator: Option[VerilogNode]

    of vnkElif:
      condition: Option[VerilogNode]

    of vnkInfix, vnkPrefix:
      operator: VerilogNode

    of vnkPar, vnkCurlyGroup:
      discard

    of vnkBraketRange:
      head, tail: VerilogNode

    of vnkBracketExpr:
      lookup: VerilogNode
      index: VerilogNode

    of vnkComment:
      content: string
      inline: bool

  VNode = VerilogNode

  ParserScopes = enum
    psInitial
    psModuleDef
    psModule
    psAlways
    psCase
    psElIf
    psOther # initial, forever, always

const VFinalKinds = {vnkNormNumber, vnkBinNumber, vnkHexNumber, vnkString}

func parseVerilogImpl(tokens: seq[VToken], acc: var seq[VNode]): int =
  var 
    i = 0
    nodeStack: seq[VNode]

  while true:
    let ct = tokens[i] # current token

    if ct.kind == vtkKeyword:
      case ct.keyword:
      "if"
      "else"
      "module"
      "case"
      "begin"
      "end"
      "endmodule"
      "`define"

      "input"
      "output"
      "inout"
      "wire"
      "reg"


  if nodeStack.len != 0:
    err "stack is not empty"

func parseVerilog*(content: string): seq[VNode] =
  let tokens = toseq extractVerilogTokens content

  discard parseVerilogImpl(tokens, result)