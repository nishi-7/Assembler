import assembler._
import error.SyntaxError

object Main {
  def main(args: Array[String]): Unit = {

    val code =
      """A;JGT
              |(xxx)
              |@1234
              |@label
              |M=1
              |0;JMP
              |D;JEQ
              |""".stripMargin
    val lex = new Lexer(code)
    val psr = new Parser(lex)
    val mst = new SymbolTable(psr)
    val (asms, table) = mst.makeSymbolTable()

    for (asm <- asms) {
      println(asm)
    }

    for ((label, counter) <- table) {
      println(s"${label} -> ${counter}")
    }
  }
}
