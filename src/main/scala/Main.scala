import assembler._
import error.SyntaxError
import error.SymbolNotFoundError

object Main {
  def main(args: Array[String]): Unit = {
    val code =
      """A;JGT
              |(xxx)
              |@1234
              |@label
              |M=1
              |0;JMP
              |@xxx
              |D;JEQ
              |(label)
              |""".stripMargin
    val lex = new Lexer(code)
    val psr = new Parser(lex)
    val mst = new SymbolTable(psr)

    try {
      val (asms, table) = mst.makeSymbolTable()
      for (asm <- asms) {
        println(asm)
      }

      for ((label, inst) <- table) {
        println(s"${label} -> ${inst}")
      }

      val codeWriter = new CodeWriter(asms, table)
      val insts = codeWriter.codeGen()

      for (inst <- insts) {
        println(inst)
      }
    } catch {
      case e: SymbolNotFoundError => println(e.errorReport(code))
      case e: SyntaxError         => println(e.errorReport(code))
    }

  }
}
