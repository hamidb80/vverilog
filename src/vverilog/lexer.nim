import std/[strutils, strformat, macros, sequtils]
import ./conventions

type
  VerilogTokenKinds* = enum
    vtkKeyword   # begin end if `pragma
    vtkSeparator # , : ;
    vtkOperator  # + - && ~^ !== ?:
    vtkGroup     # () [] {}

    vtkString    # "hello"
    vtkNumber    # 12 4.6 3b'101
    vtkComment   # //  /* */

  VerilogGroupChar* = enum
    vgcOpenPar, vgcClosePar
    vgcOpenBracket, vgcCloseBracket
    vgcOpenCurly, vgcCloseCurly

  VerilogToken* = object # verilog token
    case kind*: VerilogTokenKinds
    of vtkKeyword:
      keyword*: string

    of vtkString:
      content*: string

    of vtkNumber:
      digits*: string

    of vtkSeparator:
      sign*: char

    of vtkOperator:
      operator*: string

    of vtkGroup:
      group*: VerilogGroupChar

    of vtkComment:
      comment*: string
      inline*: bool

  VToken* = VerilogToken

  LexerState = enum
    lsInit
    lsNumber, lsKeyword, lsString
    lsOperator
    lsInlineComment, lsMultiLineComment


const
  VerilogIdentStartChars = IdentStartChars + {'$'}
  EoC = '\0' # end of content
  Stoppers = Whitespace + {EoC}
  Operators = {'+', '-', '*', '/', '#', '@', '~', '?', '^', '|', '&', '%',
      '<', '!', '=', '>', '.'}


func toGroupChar(ch: char): VerilogGroupChar =
  case ch:
  of '(': vgcOpenPar
  of ')': vgcClosePar
  of '[': vgcOpenBracket
  of ']': vgcCloseBracket
  of '{': vgcOpenCurly
  of '}': vgcCloseCurly
  else: err "invalid char"

iterator extractVerilogTokens*(content: string): VerilogToken =
  var
    lxState = lsInit
    i = 0
    start = 0

  alias fetchChar(i):
    if i in 0 ..< content.len: content[i]
    else: EoC
  alias lc: fetchChar(i-1) # last char
  alias fc: fetchChar(i+1) # forward char
  alias reset: lxState = lsInit
  alias push(newToken):
    yield newToken
    reset()


  while true:
    let cc = fetchChar i # current char

    case lxState:
    of lsInit:
      case cc:
      of Digits:
        lxState = lsNumber
        start = i
        inc i

      of VerilogIdentStartChars, '`':
        lxState = lsKeyword
        start = i
        inc i

      of '"':
        lxState = lsString
        inc i
        start = i

      of ',', ':', ';':
        push VerilogToken(kind: vtkSeparator, sign: cc)
        inc i

      of '(', ')', '[', ']', '{', '}':
        push VerilogToken(kind: vtkGroup, group: toGroupChar cc)
        inc i

      of Stoppers:
        inc i

      of Operators:
        if cc == '/':
          case fc:
          of '/':
            lxState = lsInlineComment
            inc i, 2
            start = i

          of '*':
            lxState = lsMultiLineComment
            inc i, 2
            start = i

          else:
            lxState = lsOperator
            start = i
            inc i

        else:
          lxState = lsOperator
          start = i
          inc i

      else:
        err fmt"invalid character at index {i}: '{cc}'"

    of lsKeyword:
      case cc:
      of IdentChars:
        inc i
      else:
        push VerilogToken(kind: vtkKeyword, keyword: content[start ..< i])

    of lsNumber:
      case cc:
      of '.', '_', '\'', 'b', 'h', Digits, 'A' .. 'F', 'X', 'x', 'Z', 'z':
        inc i
      else:
        push VerilogToken(kind: vtkNumber, digits: content[start ..< i])

    of lsString:
      if cc == '"' and lc != '\\':
        push VerilogToken(kind: vtkString, content: content[start ..< i])

      inc i

    of lsOperator:
      if cc in Operators:
        inc i
      else:
        push VerilogToken(kind: vtkOperator, operator: content[start ..< i])

    of lsInlineComment:
      if cc in Newlines:
        push VerilogToken(kind: vtkComment, comment: content[start ..< i], inline: true)

      inc i

    of lsMultiLineComment:
      if cc == '/' and lc == '*':
        push VerilogToken(kind: vtkComment, comment: content[start ..< i-1], inline: false)

      inc i

    if cc == EoC:
      break


func getField(kind: VerilogTokenKinds): string =
  case kind:
  of vtkKeyword: "keyword"
  of vtkSeparator: "sign"
  of vtkOperator: "operator"
  of vtkGroup: "scopeChar"
  else: err "invalid"

macro matchVtoken*(comparator: VToken, branches: varargs[untyped]): untyped =
  result = newTree(nnkCaseStmt, newDotExpr(comparator, ident"kind"))

  var
    acc1: array[vtkKeyword..vtkGroup, seq[tuple[cond, code: NimNode]]]
    acc2: array[vtkString..vtkComment, NimNode]
    elseBr = newTree(nnkElse, newStmtList newTree(nnkDiscardStmt, newEmptyNode()))

  for br in branches:
    case br.kind:
    of nnkOfBranch:
      let
        conds = br[0..^2]
        body = br[^1]

      for c in conds:
        if c.kind in {nnkCommand, nnkCallStrLit, nnkPrefix} and c.len == 2:
          let index =
            case c[0].strVal:
            of "kw": vtkKeyword
            of "w": vtkSeparator
            of "o": vtkOperator
            of "g": vtkGroup
            else: err "invalid type of condition: " & c[0].strVal

          acc1[index].add (c[1], body)

        elif c.kind == nnkIdent:
          let index =
            case c.strval:
            of "s": vtkString
            of "n": vtkNumber
            of "c": vtkComment
            else: err "invalid ident kind"

          acc2[index] = body

        else: err "invalid condition"

    of nnkElse:
      elseBr = br

    else: err "invalid entity. kind: " & $br.kind

  for i, brs in acc1:
    if brs.len != 0:
      let node =
        newtree(nnkOfBranch, ident $i).add newStmtList do:
          newTree(nnkCaseStmt, newDotExpr(comparator, ident getField i)).add:
            brs.mapIt newTree(nnkOfBranch, it.cond, it.code)

      node[^1][^1].add elsebr
      result.add node

  for i, code in acc2:
    if code != nil:
      result.add newtree(nnkOfBranch, ident $i, code)

  result.add elseBr

  # echo treeRepr result
  # echo repr result



func toKeyword*(s:string): VToken =
  VToken(kind: vtkKeyword, keyword: s)

func isGroup*(t: Vtoken, c:char): bool =
  t.kind ==  vtkGroup and t.group == toGroupChar c

func isSep*(t:VToken, c:char): bool =
  assert c in ",:;"
  t.kind == vtkseparator and t.sign == c


# test -----------------------------------------

# let t = VToken(kind: vtkString, content: "hey")
# matchVtoken t:
# of w '1': discard
# of kw"1", kw"2": discard
# of n: discard
# else: discard

# dumptree:
#   case 1:
#   of 2: discard
#   of 3, 4: discard
#   else: discard
