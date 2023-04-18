package error

import assembler.Loc

class OutofWidthError(msg: String, loc: Loc) extends HackError(msg, loc)
