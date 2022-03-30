import assembler._
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
import scala.util.Try
import scala.util.{Success, Failure}
import error.HackError

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length < 0) {
      Console.err.println("[Error] no input file")
    } else {
      for (arg <- args) {
        if (!arg.endsWith(".asm")) {
          Console.err.println(
            "[Error] input files should be Hack VM files such as *.asm"
          )
        }
        val inputPath = Paths.get(arg)
        val outputPath =
          Option(
            inputPath
              .getParent()
          ).getOrElse(Paths.get(""))
            .resolve(
              inputPath.getFileName().toString().stripSuffix(".asm") + ".hack"
            )
        Try(io.Source.fromFile(inputPath.toFile())) match {
          case Success(reader) => {
            val src = reader
              .getLines()
              .foldLeft("")((acc, e) => acc + "\n" + e)
            val code = if (src.endsWith("\n")) { src }
            else { src + "\n" }

            Try({
              val assembler = new Assembler(code)
              val insts = assembler.assemble()
              val writer =
                Files.newBufferedWriter(outputPath, StandardCharsets.UTF_8)
              try {
                for (inst <- insts) {
                  writer.write(inst)
                  writer.newLine()
                }
              } catch {
                case e: java.io.FileNotFoundException =>
                  Console.err.println(
                    s"[Error] Can not find ${inputPath.getFileName().toString()}"
                  )
              } finally {
                reader.close()
                writer.close()
              }
            }) match {
              case Success(value) =>
              case Failure(e: HackError) =>
                Console.err.println(e.errorReport(code))
              case Failure(e) =>
                Console.err.println(
                  s"[Error] failed to assemble ${outputPath.toString()}"
                )
            }
          }
          case Failure(exception) =>
            Console.err.println(
              s"[Error] can not find ${outputPath.toString()}"
            )
        }

      }
    }

  }
}
