package assembler

import java.io.PrintWriter

class CodeWriter(asms: Array[Asm], symbolTable: Map[String, Int]) {
  def codeGen(writer: PrintWriter): Unit = {
    asms
      .map(asm => transrate(asm))
      .map(code => writer.println(code));
  }

  def transrate(asm: Asm): String = {
    asm.ty match {
      case ACmd(addr)             => ???
      case CCmd(dest, comp, jump) => ???
      case Label(label)           => ???
      case _                      => ???
    }
  }

  def binaryGen(tok: Token): Int = {
    tok.ty match {
      case A => ???
    }
  }
}
