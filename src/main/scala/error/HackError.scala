package error

import assembler.Loc

class HackError(msg: String, loc: Loc) extends RuntimeException {
    def errorReport(code: String) = {
        val pos = s"line: ${this.loc.line}"
        var lpos = this.loc.left
        var rpos = this.loc.left

        while(lpos > 0 && code(lpos-1) != '\n') {
            lpos -= 1
        }

        while(rpos < code.length && code(rpos) != '\n') {
            rpos += 1
        }
        s"""
           |${pos}
           |${" " * 5 + s"${code.slice(lpos, rpos)}"}
           |${" " * (4 + this.loc.left - lpos) + s" ${"^" * this.loc.len}"}
           |${s"error: ${msg}"}
           |""".stripMargin
    }
}
