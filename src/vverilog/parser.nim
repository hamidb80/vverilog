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
      expr: VerilogNode


const VNodeLiteralKinds = {vnkNormNumber, vnkBinNumber, vnkHexNumber, vnkString}

func parseVerilog*(content: string): seq[VerilogNode] =
  let tokens = toseq extractVTokens content

  var i = 0

  while true:
    break
