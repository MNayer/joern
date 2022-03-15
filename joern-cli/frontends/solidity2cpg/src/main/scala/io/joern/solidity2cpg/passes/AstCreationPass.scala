package io.joern.solidity2cpg.passes

import io.joern.solidity2cpg.domain.SuryaObject.SourceUnit
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{ConcurrentWriterCpgPass, IntervalKeyPool}
import org.slf4j.LoggerFactory
import spray.json._

import java.util.concurrent.ConcurrentSkipListSet
import scala.util.{Failure, Success, Try, Using}

case class Global(usedTypes: ConcurrentSkipListSet[String] = new ConcurrentSkipListSet[String]())

class AstCreationPass(codeDir: String, filenames: List[String], cpg: Cpg, keyPool: IntervalKeyPool)
    extends ConcurrentWriterCpgPass[String](cpg, keyPool = Some(keyPool)) {

  val global: Global = Global()

  private val logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  override def generateParts(): Array[_ <: AnyRef] = filenames.toArray

  /** Creates an AST from the given JSON file.
    * @param filename
    *   the path to the Surya generated AST JSON file.
    * @return
    *   a list of changes generated from this pass.
    */
  override def runOnPart(builder: DiffGraphBuilder, part: String): Unit = {
    import io.joern.solidity2cpg.domain.SuryaJsonProtocol._

    Using.resource(scala.io.Source.fromFile(part)) { source =>
      val lines = source.getLines() mkString "\n"
      Try(lines.parseJson.convertTo[SourceUnit]) match {
        case Failure(e)          => logger.error(s"Unable to convert JSON to SourceUnit $part due to an exception", e)
        case Success(sourceUnit) => new AstCreator(part, builder, global).createAst(sourceUnit)
      }
    }
  }

}