package error

import assembler.Loc

class SyntaxError(msg: String, loc: Loc) extends HackError(msg, loc)
