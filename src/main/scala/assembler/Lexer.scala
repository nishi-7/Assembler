package assembler

import assembler.Lexer.keywords

object Lexer {
  private val keywords = Map(
    "A" -> A,
    "D" -> D,
    "M" -> M,
    "AM" -> AM,
    "AD" -> AD,
    "MD" -> MD,
    "AMD" -> AMD,
    "JGT" -> Jgt,
    "JEQ" -> Jeq,
    "JGE" -> Jge,
    "JLT" -> Jlt,
    "JNE" -> Jne,
    "JLE" -> Jle,
    "JMP" -> Jmp,
    "SP" -> Sp,
    "LCL" -> Lcl,
    "ARG" -> Arg,
    "THIS" -> This,
    "THAT" -> That,
    "R0" -> R0,
    "R0" -> R0,
    "R1" -> R1,
    "R2" -> R2,
    "R3" -> R3,
    "R4" -> R4,
    "R5" -> R5,
    "R6" -> R6,
    "R7" -> R7,
    "R8" -> R8,
    "R9" -> R9,
    "R10" -> R10,
    "R11" -> R11,
    "R12" -> R12,
    "R13" -> R13,
    "R14" -> R14,
    "R15" -> R15,
    "SCREEN" -> Screen,
    "KEYBOARD" -> KeyBoard,
    "UARTRX1" -> UartRx1,
    "UARTRX2" -> UartRx2,
    "SPI" -> Spi,
    "-1" -> MinusOne,
    "!A" -> BangA,
    "!D" -> BangD,
    "!M" -> BangM,
    "-A" -> MinusA,
    "-D" -> MinusD,
    "-M" -> MinusM,
    "A+1" -> APlusOne,
    "D+1" -> DPlusOne,
    "M+1" -> MPlusOne,
    "A-1" -> AMinusOne,
    "D-1" -> DMinusOne,
    "M-1" -> MMinusOne,
    "D+A" -> DPlusA,
    "D+M" -> DPlusM,
    "D-A" -> DMinusA,
    "D-M" -> DMinusM,
    "A-D" -> AMinusD,
    "M-D" -> MMinusD,
    "D&A" -> DAndA,
    "D&M" -> DAndM,
    "D|A" -> DOrA,
    "D|M" -> DOrM
  )
}

class Lexer(code: String) {
  private var ch: Option[Char] = None
  private var pos = 0
  private var nextPos = 0
  private var line = 0
  private var left = -1
  this.readChar()

  def nextToken(): Token = {
    this.skipWithSpaces()
    ch match {
      case None => Token(Eof, Loc(line, left, 0))
      case Some('/') => {
        this.peekChar() match {
          case Some('/') => {
            while (!this.isNewLine()) {
              this.readChar()
            }
            this.nextToken()
          }
          case _ => Token(Ilegal('/'), Loc(line, left, 1))
        }
      }
      case Some('\n') => {
        val tok = Token(NewLine, Loc(line, left, 1))
        this.readChar()
        line += 1
        tok
      }
      case Some('(') => {
        val tok = Token(LParen, Loc(line, left, 1))
        this.readChar()
        tok
      }
      case Some(')') => {
        val tok = Token(RParen, Loc(line, left, 1))
        this.readChar()
        tok
      }
      case Some(',') => {
        val tok = Token(Comma, Loc(line, left, 1))
        this.readChar()
        tok
      }
      case Some(';') => {
        val tok = Token(SemiColon, Loc(line, left, 1))
        this.readChar()
        tok
      }
      case Some('@') => {
        val tok = Token(At, Loc(line, left, 1))
        this.readChar()
        tok
      }
      case Some('=') => {
        val tok = Token(Eq, Loc(line, left, 1))
        this.readChar()
        tok
      }
      case Some(c) => {
        if (this.isLetter()) {
          val ident = this.readIdentifier()
          keywords.get(ident) match {
            case Some(ty) =>
              Token(ty, Loc(line, left - ident.length, ident.length))
            case None =>
              Token(Symbol(ident), Loc(line, left - ident.length, ident.length))
          }
        } else if (this.isDigit()) {
          val num = this.readNumber()
          Token(Number(num.toInt), Loc(line, left - num.length, num.length))
        } else {
          Token(Ilegal(c), Loc(line, left, 1))
        }
      }
    }
  }

  def skipWithSpaces() = {
    while (this.isWhiteSpaces()) {
      this.readChar()
    }
  }

  def isWhiteSpaces() = {
    ch match {
      case Some(c) => c == ' ' || c == '\t'
      case None    => false
    }
  }

  def readChar() = {
    if (nextPos >= code.length) {
      ch = None
    } else {
      ch = Some(code(nextPos))
    }
    pos = nextPos
    left += 1
    nextPos += 1
  }

  def peekChar(): Option[Char] = {
    if (nextPos >= code.length) {
      None
    } else {
      Some(code(nextPos))
    }
  }

  def isLetter() = {
    ch match {
      case Some(c) =>
        ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') ||
          c == '!' || c == '+' || c == '-' || c == '&' || c == '|' ||
          c == '.' || c == '_' || c == '$'
      case None => false
    }
  }

  def readIdentifier() = {
    val p = pos
    while (this.isLetter() || this.isDigit()) {
      this.readChar()
    }
    code.slice(p, pos)
  }

  def isDigit() = {
    ch match {
      case Some(c) => ('0' <= c && c <= '9')
      case None    => false
    }
  }

  def readNumber() = {
    val p = pos
    while (this.isDigit()) {
      this.readChar()
    }
    code.slice(p, pos)
  }

  def isNewLine() = {
    ch match {
      case Some(c) => c == '\n'
      case _       => false
    }
  }

}
