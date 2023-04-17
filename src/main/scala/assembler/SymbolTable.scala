package assembler

class SymbolTable(asms: Seq[Asm]) {
  def makeSymbolTable(): Map[String, Int] = {
    var counter = 0
    var table: Map[String, Int] = Map.empty

    for (asm <- asms) {
      asm.ty match {
        case Label(label) => {
          table = table.updated(label, counter)
        }
        case DummyACmd(label) => {
          counter += 2
        }
        case _ => {
          counter += 1
        }
      }
    }
    table
  }
}
