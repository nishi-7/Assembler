package error

import assembler.Loc

class SymbolNotFoundError(msg: String, loc: Loc) extends HackError(msg, loc)
