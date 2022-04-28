import std/[strutils, strformat, macros, sequtils]
import ./conventions

type
  VerilogTokenKinds* = enum
    vtkKeyword   # begin end if `pragma
    vtkString    # "hello"
    vtkNumber    # 12 4.6 3b'101

    vtkSeparator # , : ;
    vtkOperator  # + - && ~^ !== ?:

    vtkGroup     # () [] {}
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
      scopeChar*: VerilogGroupChar

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
        let gc = case cc:
          of '(': vgcOpenPar
          of ')': vgcClosePar
          of '[': vgcOpenBracket
          of ']': vgcCloseBracket
          of '{': vgcOpenCurly
          of '}': vgcCloseCurly
          else: impossible

        push VerilogToken(kind: vtkGroup, scopeChar: gc)
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
  of vtkString: "content"
  of vtkNumber: "digits"
  of vtkSeparator: "sign"
  of vtkOperator: "operator"
  of vtkGroup: "scopeChar"
  of vtkComment: "comment"


macro matchVtoken*(comparator: VToken, branches: varargs[untyped]): untyped =
  result = newTree(nnkCaseStmt, newDotExpr(comparator, ident"kind"))

  var
    acc: array[vtkKeyword..vtkOperator, seq[tuple[cond, code: NimNode]]]
    elseBr = newTree(nnkElse, newStmtList newNimNode nnkDiscardStmt)

  for br in branches:
    case br.kind:
    of nnkOfBranch:
      let
        cond = br[0]
        body = br[1]

      if cond.kind == nnkCommand and cond.len == 2:
        let index =
          case cond[0].strVal:
          of "kw": vtkKeyword
          of "$": vtkString
          of "n": vtkNumber
          of "w": vtkSeparator
          of "o": vtkOperator
          else: err "invalid type of condition: " & cond[0].strVal

        acc[index].add (cond[1], body)

      else: err "invalid condition"

    of nnkElse:
      elseBr = br

    else: err "invalid entity. kind: " & $br.kind

  for i, brs in acc:
    if brs.len != 0:
      let node =
        newtree(nnkOfBranch, ident $i).add newStmtList do:
          newTree(nnkCaseStmt, newDotExpr(comparator, ident getField i)).add:
            brs.mapIt newTree(nnkOfBranch, it.cond, it.code)

      node[^1][^1].add elsebr
      result.add node

  result.add elseBr

  echo treeRepr result
  echo repr result

# matchVtoken t:
# of s "1": discard
# of d "2": discard
# else: discard
