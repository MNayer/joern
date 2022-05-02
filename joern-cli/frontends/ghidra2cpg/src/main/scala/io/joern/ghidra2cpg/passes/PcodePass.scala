package io.joern.ghidra2cpg.passes

import ghidra.program.model.listing.{CodeUnitFormat, CodeUnitFormatOptions, Function, Instruction, Program}
import ghidra.program.model.pcode.PcodeOp._
import ghidra.program.model.pcode.{HighFunction, PcodeOp, Varnode}
import io.joern.ghidra2cpg._
import io.joern.ghidra2cpg.utils.{Decompiler, PCodeMapper}
import io.joern.ghidra2cpg.utils.Utils._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNodeNew, NewBlock, NewMethod}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.passes.ConcurrentWriterCpgPass
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters._
import scala.language.implicitConversions

class PcodePass(
  currentProgram: Program,
  address2Literal: Map[Long, String],
  fileName: String,
  functions: List[Function],
  cpg: Cpg,
  decompiler: Decompiler
) extends ConcurrentWriterCpgPass[Function](cpg) {

  private val logger = LoggerFactory.getLogger(classOf[PcodePass])

  // needed by ghidra for decompiling reasons
  protected val codeUnitFormat = new CodeUnitFormat(
    new CodeUnitFormatOptions(
      CodeUnitFormatOptions.ShowBlockName.NEVER,
      CodeUnitFormatOptions.ShowNamespace.NEVER,
      "",
      true,
      true,
      true,
      true,
      true,
      true,
      true
    )
  )

  def getHighFunction(function: Function): HighFunction = decompiler.toHighFunction(function).orNull

  def resolveVarNode(instruction: Instruction, input: Varnode, index: Int): CfgNodeNew = {
    var returnNode: CfgNodeNew = createIdentifier(
      "TODO",
      "TODO",
      index + 1,
      Types.registerType("TODO"),
      instruction.getMinAddress.getOffsetAsBigInteger.intValue
    )
    if (input.isRegister) {
      returnNode = createIdentifier(
        currentProgram.getRegister(input).getName,
        currentProgram.getRegister(input).getName,
        index + 1,
        Types.registerType(name),
        instruction.getMinAddress.getOffsetAsBigInteger.intValue
      )
    } else if (input.isConstant)
      returnNode = createLiteral(
        "0x" + input.getWordOffset.toHexString,
        index + 1,
        index + 1,
        "0x" + input.getWordOffset.toHexString,
        instruction.getMinAddress.getOffsetAsBigInteger.intValue
      )
    else if (input.isUnique) {
      var valueString = ""
      if (input.getDescendants != null) {

        if (input.getDescendants.asScala.toList.head.getOutput == null)
          valueString = input.getDef.getInputs.toList.head.getAddress.getOffset.toString
        else valueString = input.getDescendants.asScala.toList.head.getOutput.getHigh.getName

        val value = address2Literal.getOrElse(input.getDef.getInputs.toList.head.getAddress.getOffset, valueString)

        returnNode = createLiteral(
          value,
          index + 1,
          index + 1,
          input.getWordOffset.toHexString,
          instruction.getMinAddress.getOffsetAsBigInteger.intValue
        )
      }
    } else {
      // we default to literal
      // identifier could be useful too
      returnNode = createLiteral(
        input.toString(),
        index + 1,
        index + 1,
        input.toString(),
        instruction.getMinAddress.getOffsetAsBigInteger.intValue
      )
    }
    returnNode
  }

  // def handleAssignment(
  //                      diffGraphBuilder: DiffGraphBuilder,
  //                      instruction: Instruction,
  //                      callNode: CfgNodeNew,
  //                      to: Varnode,
  //                      index: Int
  //                    ): Unit = {
  //  val node = resolveVarNode(instruction, to, index)
  //  connectCallToArgument(diffGraphBuilder, callNode, node)
  // }

  def handleStore(
    diffGraphBuilder: DiffGraphBuilder,
    instruction: Instruction,
    callNode: CfgNodeNew,
    pcodeOp: PcodeOp
  ): Unit = {
    val firstOp  = resolveVarNode(instruction, pcodeOp.getInput(1), 2)
    val secondOp = resolveVarNode(instruction, pcodeOp.getInput(2), 1)
    connectCallToArgument(diffGraphBuilder, callNode, firstOp)
    connectCallToArgument(diffGraphBuilder, callNode, secondOp)
  }
  def handleAssignment(
    diffGraphBuilder: DiffGraphBuilder,
    instruction: Instruction,
    callNode: CfgNodeNew,
    pcodeOp: PcodeOp
  ): Unit = {
    val firstOp  = resolveVarNode(instruction, pcodeOp.getOutput, 2)
    val secondOp = resolveVarNode(instruction, pcodeOp.getInput(0), 1)
    connectCallToArgument(diffGraphBuilder, callNode, firstOp)
    connectCallToArgument(diffGraphBuilder, callNode, secondOp)
  }

  def handleTwoArguments(
    diffGraphBuilder: DiffGraphBuilder,
    instruction: Instruction,
    callNode: CfgNodeNew,
    pcodeOp: PcodeOp,
    operand: String,
    name: String
  ): Unit = {
    val firstOp  = resolveVarNode(instruction, pcodeOp.getInput(0), 1)
    val secondOp = resolveVarNode(instruction, pcodeOp.getInput(1), 2)
    val code     = s"${firstOp.code} $operand ${secondOp.code}"
    val opNode   = createCallNode(code = code, name, instruction.getMinAddress.getOffsetAsBigInteger.intValue)

    connectCallToArgument(diffGraphBuilder, opNode, firstOp)
    connectCallToArgument(diffGraphBuilder, opNode, secondOp)
    // connectCallToArgument(diffGraphBuilder, callNode, opNode)
  }

  def handlePtrSub(
    diffGraphBuilder: DiffGraphBuilder,
    instruction: Instruction,
    callNode: CfgNodeNew,
    varNode: Varnode,
    index: Int
  ): Unit = {
    val arg = resolveVarNode(instruction, varNode, index)
    connectCallToArgument(diffGraphBuilder, callNode, arg)
  }

  def handleDefault(varNode: PcodeOp): Unit = {
    println("Unsupported " + varNode.toString + " " + varNode.getOpcode)
  }

  def handleParameters(diffGraphBuilder: DiffGraphBuilder, function: Function, methodNode: NewMethod): Unit = {
    if (function.isThunk)
      function
        .getThunkedFunction(true)
        .getParameters
        .zipWithIndex
        .foreach { case (parameter, index) =>
          val node = createParameterNode(
            parameter.getName,
            parameter.getName,
            index + 1,
            parameter.getDataType.getName,
            function.getEntryPoint.getOffsetAsBigInteger.intValue()
          )
          diffGraphBuilder.addNode(node)
          diffGraphBuilder.addEdge(methodNode, node, EdgeTypes.AST)
        }
    else
      decompiler
        .toHighFunction(function)
        .get
        .getLocalSymbolMap
        .getSymbols
        .asScala
        .toSeq
        .filter(_.isParameter)
        .foreach { parameter =>
          val checkedParameterName = Option(parameter.getStorage)
            .flatMap(x => Option(x.getRegister))
            .flatMap(x => Option(x.getName))
            .getOrElse(parameter.getName)
          val node =
            createParameterNode(
              checkedParameterName,
              checkedParameterName,
              parameter.getCategoryIndex + 1,
              parameter.getDataType.getName,
              function.getEntryPoint.getOffsetAsBigInteger.intValue()
            )
          diffGraphBuilder.addNode(node)
          diffGraphBuilder.addEdge(methodNode, node, EdgeTypes.AST)
        }
  }

  def handleLocals(diffGraphBuilder: DiffGraphBuilder, function: Function, blockNode: NewBlock): Unit = {
    function.getLocalVariables.foreach { local =>
      val localNode = nodes
        .NewLocal()
        .name(local.getName)
        .code(local.toString)
        .typeFullName(Types.registerType(local.getDataType.toString))
      val identifier =
        createIdentifier(local.getName, local.getSymbol.getName, -1, local.getDataType.toString, -1)

      diffGraphBuilder.addNode(localNode)
      diffGraphBuilder.addNode(identifier)
      diffGraphBuilder.addEdge(blockNode, localNode, EdgeTypes.AST)
      diffGraphBuilder.addEdge(blockNode, identifier, EdgeTypes.AST)
      diffGraphBuilder.addEdge(identifier, localNode, EdgeTypes.REF)
    }
  }

  def handleInstruction(diffGraphBuilder: DiffGraphBuilder, instruction: Instruction): CfgNodeNew = {
    val p        = new PCodeMapper(diffGraphBuilder, instruction)
    val pcodeOp  = instruction.getPcode().toList.lastOption
    var callNode = createCallNode("UNKNOWN", "UNKNOWN", instruction.getMinAddress.getOffsetAsBigInteger.intValue)
    if (pcodeOp.nonEmpty) {
      // println(s"INSTRUCTION $instruction ${instruction.getPcode().mkString("\n\t")}")
      pcodeOp.get.getOpcode match {
        case INT_EQUAL | INT_NOTEQUAL | INT_SLESS | INT_SLESSEQUAL | INT_LESS | INT_LESSEQUAL =>
          // println("TODO: INT_EQUAL | INT_NOTEQUAL | INT_SLESS | INT_SLESSEQUAL | INT_LESS | INT_LESSEQUAL ")
          createCallNode(
            instruction.toString,
            instruction.toString,
            instruction.getMinAddress.getOffsetAsBigInteger.intValue
          )
        case COPY =>
          callNode = createCallNode(
            instruction.toString,
            "<operator>.assignment",
            instruction.getMinAddress.getOffsetAsBigInteger.intValue
          )
          handleAssignment(diffGraphBuilder, instruction, callNode, pcodeOp.get)
        case LOAD =>
          callNode = createCallNode(
            instruction.toString,
            "<operator>.assignment",
            instruction.getMinAddress.getOffsetAsBigInteger.intValue
          )
        // handleAssignment(diffGraphBuilder, instruction, callNode, pcodeOp.get)
        case SUBPIECE =>
          callNode = createCallNode(
            instruction.toString,
            "<operator>.assignment",
            instruction.getMinAddress.getOffsetAsBigInteger.intValue
          )
          handleAssignment(diffGraphBuilder, instruction, callNode, pcodeOp.get)
        case STORE =>
          callNode = createCallNode(
            instruction.toString,
            "<operator>.assignment",
            instruction.getMinAddress.getOffsetAsBigInteger.intValue
          )
          handleStore(diffGraphBuilder, instruction, callNode, pcodeOp.get)
        case CALL =>
          val calledFunction =
            codeUnitFormat.getOperandRepresentationString(instruction, 0)
          // TODO PARAMETER
          callNode =
            createCallNode(calledFunction, calledFunction, instruction.getMinAddress.getOffsetAsBigInteger.intValue)
        case CALLOTHER =>
          val calledFunction =
            codeUnitFormat.getOperandRepresentationString(instruction, 0)
          // TODO PARAMETER
          callNode =
            createCallNode(calledFunction, calledFunction, instruction.getMinAddress.getOffsetAsBigInteger.intValue)
        case CALLIND =>
          val calledFunction =
            codeUnitFormat
              .getOperandRepresentationString(instruction, 0)
              .split(">")
              .last
              .replace("[", "")
              .replace("]", "")
          // TODO PARAMETER
          callNode =
            createCallNode(calledFunction, calledFunction, instruction.getMinAddress.getOffsetAsBigInteger.intValue)
        case INT_ADD | FLOAT_ADD =>
          callNode = createCallNode(
            instruction.toString,
            "<operator>.addition",
            instruction.getMinAddress.getOffsetAsBigInteger.intValue
          )
          handleTwoArguments(diffGraphBuilder, instruction, callNode, pcodeOp.get, "+", "<operator>.addition")
        case RETURN =>
          // println("TODO: |")
          createCallNode("UNKNOWN", "UNKNOWN", instruction.getMinAddress.getOffsetAsBigInteger.intValue)
        case INT_AND =>
          callNode = createCallNode("UNKNOWN", "UNKNOWN", instruction.getMinAddress.getOffsetAsBigInteger.intValue)
          handleTwoArguments(diffGraphBuilder, instruction, callNode, pcodeOp.get, "&&", "<operator>.add")
        case INT_OR =>
          callNode = createCallNode(
            instruction.toString,
            "<operator>.or",
            instruction.getMinAddress.getOffsetAsBigInteger.intValue
          )
          handleTwoArguments(diffGraphBuilder, instruction, callNode, pcodeOp.get, "|", "<operator>.or")
        case INT_DIV | FLOAT_DIV | INT_SDIV =>
          callNode = createCallNode(
            instruction.toString,
            "<operator>.division",
            instruction.getMinAddress.getOffsetAsBigInteger.intValue
          )
          handleTwoArguments(diffGraphBuilder, instruction, callNode, pcodeOp.get, "/", "<operator>.division")
        case INT_SUB | FLOAT_SUB =>
          callNode = createCallNode(
            instruction.toString,
            "<operator>.subtraction",
            instruction.getMinAddress.getOffsetAsBigInteger.intValue
          )
          handleTwoArguments(diffGraphBuilder, instruction, callNode, pcodeOp.get, "-", "<operator>.subtraction")
        case INT_MULT | FLOAT_MULT =>
          callNode = createCallNode(
            instruction.toString,
            "<operator>.multiplication",
            instruction.getMinAddress.getOffsetAsBigInteger.intValue
          )
          handleTwoArguments(diffGraphBuilder, instruction, callNode, pcodeOp.get, "*", "<operator>.multiplication")
        case MULTIEQUAL | INDIRECT | PIECE => // not handled
        case INT_XOR =>
          callNode = createCallNode(
            instruction.toString,
            "<operator>.xor",
            instruction.getMinAddress.getOffsetAsBigInteger.intValue
          )
          handleTwoArguments(diffGraphBuilder, instruction, callNode, pcodeOp.get, "^", "<operator>.xor")
        case CAST =>
          // println("TODO: CAST")
          createCallNode("UNKNOWN", "UNKNOWN", instruction.getMinAddress.getOffsetAsBigInteger.intValue)
        // we need to "unpack" the def of the first input of the cast
        // eg. "(param_1 + 5)" in "(void *)(param_1 + 5)"
        // if (pcodeOp.getInput(0).getDef != null)
        //  resolveArgument(diffGraphBuilder, instruction, opNode, pcodeOps.last.getInput(0).getDef, index)
        case BRANCH | BRANCHIND | CBRANCH =>
          // println("TODO BRANCH")
          createCallNode("UNKNOWN", "UNKNOWN", instruction.getMinAddress.getOffsetAsBigInteger.intValue)
        case PTRSUB | PTRADD =>
          // handlePtrSub(diffGraphBuilder, instruction, callNode, pcodeOp.getOutput, index)
          createCallNode("UNKNOWN", "UNKNOWN", instruction.getMinAddress.getOffsetAsBigInteger.intValue)
        case _ => createCallNode("UNKNOWN", "UNKNOWN", instruction.getMinAddress.getOffsetAsBigInteger.intValue)
        /* TODO:
          BOOL_AND BOOL_NEGATE BOOL_OR
          BOOL_XOR CPOOLREF EXTRACT
          FLOAT_ABS FLOAT_CEIL FLOAT_EQUAL
          FLOAT_FLOAT2FLOAT FLOAT_FLOOR FLOAT_INT2FLOAT
          FLOAT_LESS FLOAT_LESSEQUAL
          FLOAT_MULT FLOAT_NAN FLOAT_NEG
          FLOAT_NOTEQUAL FLOAT_ROUND
          FLOAT_SQRT FLOAT_TRUNC INSERT
          INT_2COMP INT_CARRY INT_EQUAL
          INT_LEFT INT_LESS INT_LESSEQUAL
          INT_MULT INT_NEGATE INT_NOTEQUAL
          INT_REM INT_RIGHT INT_SBORROW
          INT_SCARRY INT_SEXT INT_SLESS
          INT_SLESSEQUAL INT_SREM INT_SRIGHT
          INT_ZEXT MULTIEQUAL NEW PCODE_MAX
          POPCOUNT SEGMENTOP UNIMPLEMENTED
         */
      }
    }
    callNode
  }

  def handleBody(
    diffGraphBuilder: DiffGraphBuilder,
    function: Function,
    methodNode: NewMethod,
    blockNode: NewBlock
  ): Unit = {
    // get asm instructions
    val instructionNodes = currentProgram.getListing
      .getInstructions(function.getBody, true)
      .iterator()
      .asScala
      .toList
      .map { instruction =>
        handleInstruction(diffGraphBuilder, instruction)
      }

    if (instructionNodes.nonEmpty) {
      diffGraphBuilder.addEdge(blockNode, instructionNodes.head, EdgeTypes.AST)
      diffGraphBuilder.addEdge(methodNode, instructionNodes.head, EdgeTypes.CFG)
      instructionNodes.sliding(2).foreach { nodes =>
        val prevInstructionNode = nodes.head
        val instructionNode     = nodes.last
        diffGraphBuilder.addEdge(blockNode, prevInstructionNode, EdgeTypes.AST)
        diffGraphBuilder.addEdge(methodNode, prevInstructionNode, EdgeTypes.CFG)
        diffGraphBuilder.addNode(prevInstructionNode)
        diffGraphBuilder.addEdge(blockNode, instructionNode, EdgeTypes.AST)
      }
    }
  }

  override def runOnPart(diffGraphBuilder: DiffGraphBuilder, function: Function): Unit = {
    val localDiffGraph = new DiffGraphBuilder
    // we need it just once with default settings
    val blockNode  = nodes.NewBlock().code("").order(0)
    val methodNode = createMethodNode(decompiler, function, fileName, checkIfExternal(currentProgram, function.getName))
    val methodReturn = createReturnNode()

    localDiffGraph.addNode(methodNode)
    localDiffGraph.addNode(blockNode)
    localDiffGraph.addEdge(methodNode, blockNode, EdgeTypes.AST)
    localDiffGraph.addNode(methodReturn)
    localDiffGraph.addEdge(methodNode, methodReturn, EdgeTypes.AST)

    handleParameters(diffGraphBuilder, function, methodNode)
    handleLocals(diffGraphBuilder, function, blockNode)
    handleBody(diffGraphBuilder, function, methodNode, blockNode)
    diffGraphBuilder.absorb(localDiffGraph)
  }

  override def generateParts(): Array[Function] = functions.toArray
}
