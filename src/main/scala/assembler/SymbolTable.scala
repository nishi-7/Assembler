package assembler

class SymbolTable(psr: Parser) {
  def makeSymbolTable(): (Vector[Asm], Map[String, Int]) = {
    var counter = 0
    var table: Map[String, Int] = Map.empty
    var asms: Vector[Asm] = Vector.empty
    while (!psr.isEnd()) {
      val cmd = psr.parseOne()
      cmd.ty match {
        case Label(label) => {
          table = table.updated(label, counter)
        }
        case _ => {
          counter += 1
          asms = asms :+ cmd
        }
      }
    }
    (asms, table)
  }
}
