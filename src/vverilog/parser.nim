import std/[sequtils, options, strutils]
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

    vnkDeclare, vnkDefine, vnkAssign, vnkInstantiate

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

    of vnkDeclare:
      dkind*: VerilogDeclareKinds
      bus*: Option[VerilogNode]
      idents*: seq[VerilogNode]

    of vnkDefine:
      ident*, value*: VerilogNode

    of vnkAssign:
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
      operator*: string

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
    psModuldeIdent, psModuldeParams, psModuleBody, psModuleAddBody

    psDeclareStart, psDeclareBus, psDeclareApplyBus, psDeclareIdentDo, psDeclareIdentCheck, psDeclareAddIdent

    psAssignStart, psAssignContainerSet, psAssignOperator, psAssignValueSet, psAssignEnd

    psDefineStart,psDefineIdent,  psDefineValue

    psParStart, psParBody
    psBracketStart, psBracketBody
    psCurlyStart, psCurlyBody

    psBracketExprFinalize

    psPrefixStart, psPrefixEnd
    psInfixStart, psInfixEnd

    psExprStart, psExprBody

    psInstanceName, psInstanceArgs
    psScopeBody, psScopeArgs # always @ (...)

    psElIfCond, psElIfBody, psElseBody
    psCaseParam, psCaseOfParam, psCaseOfBody

    psOperator

    psBlock                  # TODO



func `$`*(k: VerilogDeclareKinds): string =
  case k:
  of vdkInput: "input"
  of vdkOutput: "output"
  of vdkInOut: "inout"
  of vdkReg: "reg"
  of vdkWire: "wire"

const indentSize = 4

func toString(vn: VNode, depth: int = 0): string =
  let t =
    case vn.kind:

    of vnkNumber: vn.digits
    of vnkString: '"' & vn.str & '"'
    of vnkSymbol: vn.symbol

    of vnkAction: toString(vn.action, depth+1) & ";\n"
    of vnkRange: toString(vn.head) & ':' & toString(vn.tail)

    of vnkGroup:
      let openClose =
        case vn.groupKind:
        of vskPar: ['(', ')']
        of vskBracket: ['[', ']'] # TODO
        of vskCurly: ['{', '}']

      openClose[0] & vn.body.mapIt(it.toString).join(", ") & openClose[1]

    of vnkCall:
      toString(vn.caller) & '(' &
      vn.body.mapIt(it.toString).join(", ") & ')'

    of vnkBracketExpr:
      toString(vn.lookup) & '[' & toString(vn.index) & ']'

    of vnkDeclare:
      let b =
        if issome vn.bus: '[' & toString(vn.bus.get) & "] "
          else: ""

      $vn.dkind & ' ' & b & vn.idents.mapIt(toString it).join(", ") & ';'

    of vnkDefine:
      "`define " & toString(vn.ident) & ' ' & toString(vn.value)

    of vnkAssign:
      "assign " & toString(vn.container) & " = " & toString(vn.newValue) & ';'

    of vnkModule:
      "module " & toString(vn.name) &
      '(' & vn.params.mapIt(it.toString).join(", ") & ");\n" &
      vn.body.mapIt(it.toString depth+1).join("\n") &
      "\nendmodule"

    of vnkPrefix: vn.operator & toString(vn.body[0])

    of vnkInfix:
      toString(vn.body[0]) & ' ' &
      vn.operator & ' ' &
      toString(vn.body[1])

    of vnkInstantiate:
      toString(vn.module) & ' ' & toString(vn.instance) &
      '(' & vn.body.mapIt(it.toString).join(", ") & ");"

    # of vnkElif:

    # of vnkCase:
    # of vnkOf:

    of vnkComment:
      if vn.inline:
        "//" & vn.comment
      else:
        "/*" & vn.comment & "*/"

    else: err "wow"

  repeat(" ", depth * indentSize) & t

func `$`*(vn: VNode): string =
  toString vn


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
      debugecho " - - - - - - - - - - - - - - - - - "
      debugecho ct
      debugecho "/ ", stateStack.join" / "
      debugecho "> ", nodeStack.mapIt(it.kind).join" > "


    case stateStack.last:
      of psTopLevel:
        matchVtoken ct:
        of kw "module":
          nodeStack.add VNode(kind: vnkModule)
          stateStack.add psModuldeIdent
          inc i

        else: err "not implemented: " & $ct


      of psModuldeIdent:
        matchVtoken ct:
        of kw:
          nodeStack.last.name = toVNode ct
          switch psModuldeParams
          inc i

        else:
          err "hee"

      of psModuldeParams:
        matchVtoken ct:
        of g vgcOpenPar:
          follow psParStart

        of w skSemiColon:
          let ln = nodestack.last
          if ln.kind == vnkGroup and ln.groupKind == vskPar:
            let p = nodestack.pop
            nodestack.last.params = p.body

          switch psModuleBody
          inc i

        else:
          err "invalid token"

      of psModuleBody:
        follow psModuleAddBody

        matchVtoken ct:
        of kw"endmodule":
          result.add nodeStack.pop
          inc i

        of kw"input", kw"output", kw"inout", kw"wire", kw"reg":
          follow psDeclareStart

        of kw"assign":
          follow psAssignStart

        of kw"`define":
          follow psDefineStart

        else:
          err "ERR: " & $ct

      of psModuleAddBody:
        let p = nodeStack.pop
        nodestack.last.body.add p
        back


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
          err "invalid syntax"   & $ct

      of psDeclareAddIdent:
        let p = nodestack.pop
        nodestack.last.idents.add p
        back
        switch psDeclareIdentCheck


      of psAssignStart:
        nodestack.add VNode(kind: vnkAssign)
        switch psAssignContainerSet
        follow psExprStart
        inc i

      of psAssignContainerSet:
        let p = nodestack.pop
        nodestack.last.container = p

        switch psAssignOperator

      of psAssignOperator:
        matchVToken ct:
        of o "=":
          switch psAssignValueSet
          follow psExprStart
          inc i

        else:
          err "expected = got:" & $ct

      of psAssignValueSet:
        let p = nodestack.pop
        nodestack.last.newValue = p
        switch psAssignEnd

      of psAssignEnd:
        matchVToken ct:
        of w skSemiColon: 
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
        dec i

      of psParStart:
        matchVtoken ct:
        of g vgcOpenPar:
          nodeStack.add VNode(kind: vnkGroup, groupkind: vskPar)
          switch psParBody
          inc i

        else:
          err "invalid"

      of psParBody:
        matchVtoken ct:
        of w skComma:
          let p = nodeStack.pop
          nodeStack.last.body.add p

          follow psExprStart
          inc i

        of g vgcClosePar:
          let p = nodeStack.pop
          nodeStack.last.body.add p

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

      # of psCurlyStart:
      # of psCurlyBody:

      # define
      # alawyas
      # block
      # ifelse
      # case

      # ------------------------------------

      of psExprStart:
        matchVtoken ct:
        of kw, n:
          nodeStack.add toVNode ct
          switch psExprBody
          inc i

        of g vgcOpenBracket:
          switch psBracketStart

        of g vgcOpenPar:
          switch psParStart

        of o:
          switch psPrefixStart

        else:
          back

      of psExprBody:
        matchVtoken ct:
        of g vgcOpenBracket:
          let p = nodestack.pop
          nodeStack.add VNode(kind: vnkBracketExpr, lookup: p)
          follow psBracketExprFinalize
          follow psExprStart

        of g vgcOpenPar:
          err "call"

        of o:
          if ct.operator == "=":
            back
          else:
            switch psInfixStart

        else: back

      of psBracketExprFinalize:
        let p = nodeStack.pop
        nodestack.last.index = p
        back


      of psInfixStart:
        let p = nodestack.pop
        nodeStack.add VNode(kind: vnkInfix, operator: ct.operator, body: @[p])
        switch psPrefixEnd
        follow psExprStart
        inc i

      of psInfixEnd:
        let p = nodeStack.pop
        nodeStack.last.body.add p
        back


      of psPrefixStart:
        nodeStack.add VNode(kind: vnkPrefix, operator: ct.operator)
        switch psPrefixEnd
        follow psExprStart
        inc i

      of psPrefixEnd:
        let p = nodeStack.pop
        nodeStack.last.body.add p
        back

      else: err "this parser state is not implemented: " & $stateStack.last

  debugecho "~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~"
  debugecho nodestack.mapIt it.kind

func parseVerilog*(content: string): seq[VNode] =
  let tokens = toseq extractVerilogTokens content
  parseVerilogImpl tokens
