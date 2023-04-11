package assembler

import assembler.Util._
import assembler.CodeWriter.keywords

object CodeWriter {
  private val keywords: Map[TokenType, Int] = Map(
    Sp -> 0,
    Lcl -> 1,
    Arg -> 2,
    This -> 3,
    That -> 4,
    R0 -> 0,
    R1 -> 1,
    R2 -> 2,
    R3 -> 3,
    R4 -> 4,
    R5 -> 5,
    R6 -> 6,
    R7 -> 7,
    R8 -> 8,
    R9 -> 9,
    R10 -> 10,
    R11 -> 11,
    R12 -> 12,
    R13 -> 13,
    R14 -> 14,
    R15 -> 15,
    Screen -> 16384,
    KeyBoard -> 24576
  )
}

class CodeWriter(asms: Seq[Asm], var symbolTable: Map[String, Int]) {
  private var countAddr = 16

  def codeGen(): Seq[String] = {
    asms
      .map(asm => translate(asm))
  }

  def translate(asm: Asm): String = {
    asm.ty match {
      case ACmd(addr)             => binaryGenACmd(addr, asm.loc)
      case ACmdLabel(label)       => binaryGenACmd(label, asm.loc)
      case CCmd(dest, comp, jump) => binaryGenCCmd(dest, comp, jump, asm.loc)
      case Label(label)           => throwCodeGenError("label", label, asm.loc)
      case tt => throwCodeGenError("xx", tt.toString(), asm.loc)
    }
  }

  def checkWidthOfData(inst: Int, tt: TokenType, loc: Loc) = {
    if (inst > (1 << 16) - 1) {
      throwCodeGenError("Out of width", tt.toString(), loc)
    }
  }

  def binaryGenACmd(addr: TokenType, loc: Loc): String = {
    // !(2 ** 16 - 1 - X)
    // @(2 ** 16 - 1 - X)
    // A = !A
    val n = addr match {
      case Number(n) => n
      case Symbol(s) => {
        symbolTable.get(s) match {
          case None => {
            val n = countAddr
            this.symbolTable = symbolTable.updated(s, countAddr)
            countAddr = countAddr + 1
            n
          }
          case Some(value) => value
        }
      }
      case tt => {
        keywords.get(tt) match {
          case Some(n) => n
          case None =>
            throwCodeGenError(
              "number, symbol or keywords (SP, LCL, ARG, THIS, THAT, R0-13, SCREEN, KEYBOARD, UARTRX, SPI, LED7SEG)",
              tt.toString(),
              loc
            )
        }
      }
    }

    checkWidthOfData(n, addr, loc)
    String.format("%16s", Integer.toBinaryString(n)).replace(' ', '0')
  }

  def binaryGenCCmd(
      dest: TokenType,
      comp: TokenType,
      jump: TokenType,
      loc: Loc
  ): String = {
    val binDest = binaryGenDest(dest, loc)
    val binComp = binaryGenComp(comp, loc)
    val binJump = binaryGenJump(jump, loc)

    "111" + binComp + binDest + binJump
  }

  def binaryGenDest(dest: TokenType, loc: Loc): String = {
    dest match {
      case Null => "000"
      case M    => "001"
      case D    => "010"
      case MD   => "011"
      case A    => "100"
      case AM   => "101"
      case AD   => "110"
      case AMD  => "111"
      case tt =>
        throwCodeGenError(
          "null, M, D, MD, A, AM, AD or AMD",
          tt.toString(),
          loc
        )
    }
  }

  def binaryGenComp(comp: TokenType, loc: Loc): String = {
    comp match {
      case Number(0) => "0101010"
      case Number(1) => "0111111"
      case MinusOne  => "0111010"
      case D         => "0001100"
      case A         => "0110000"
      case BangD     => "0001101"
      case BangA     => "0110001"
      case MinusD    => "0001111"
      case MinusA    => "0110011"
      case DPlusOne  => "0011111"
      case APlusOne  => "0110111"
      case DMinusOne => "0001110"
      case AMinusOne => "0110010"
      case DPlusA    => "0000010"
      case DMinusA   => "0010011"
      case AMinusD   => "0000111"
      case DAndA     => "0000000"
      case DOrA      => "0010101"
      case M         => "1110000"
      case BangM     => "1110001"
      case MinusM    => "1110011"
      case MPlusOne  => "1110111"
      case MMinusOne => "1110010"
      case DPlusM    => "1000010"
      case DMinusM   => "1010011"
      case MMinusD   => "1000111"
      case DAndM     => "1000000"
      case DOrM      => "1010101"
      case tt =>
        throwCodeGenError(
          "0, 1, -1, D, A, !D, !A, -D, -A, D+1, A+1, D-1, D+A, D-A, A-D, D&A, D|A, M, !M, -M, M+1, M-1, D+M, D-M, M-D, D&M, D|M",
          tt.toString(),
          loc
        )
    }
  }

  def binaryGenJump(jump: TokenType, loc: Loc): String = {
    jump match {
      case Null => "000"
      case Jgt  => "001"
      case Jeq  => "010"
      case Jge  => "011"
      case Jlt  => "100"
      case Jne  => "101"
      case Jle  => "110"
      case Jmp  => "111"
      case tt =>
        throwCodeGenError(
          "null, JGT, JEQ, JGE, JLT, JNE, JLE, JMP",
          tt.toString(),
          loc
        )
    }
  }
}
