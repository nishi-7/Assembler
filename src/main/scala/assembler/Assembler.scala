package assembler

class Assembler(code: String) {

  def assemble(): Seq[String] = {
    val lex = new Lexer(code)
    val psr = new Parser(lex)
    val mst = new SymbolTable(psr)
    val (asms, table) = mst.makeSymbolTable()
    val cwr = new CodeWriter(asms, table)
    cwr.codeGen()
  }
}
