package io.joern.solidity2cpg.passes

import io.joern.solidity2cpg.domain.SuryaObject._
import io.joern.x2cpg.{Ast, AstCreatorBase}
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, EvaluationStrategies, NodeTypes, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.{NewAnnotation, NewAnnotationLiteral, NewAnnotationParameter, NewAnnotationParameterAssign, NewArrayInitializer, NewBinding, NewBlock, NewCall, NewClosureBinding, NewControlStructure, NewFieldIdentifier, NewFile, NewIdentifier, NewJumpTarget, NewLiteral, NewLocal, NewMember, NewMethod, NewMethodParameterIn, NewMethodRef, NewMethodReturn, NewModifier, NewNamespaceBlock, NewNode, NewReturn, NewTypeDecl, NewTypeRef, NewUnknown}
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate.DiffGraphBuilder

/** Creates an AST using [[createAst]].
  * @param filename
  *   the name of the file this file is generated from. This should correspond to the .sol file without the temporary
  *   directory prefixed.
  * @param sourceUnit
  *   the parsed [[SourceUnit]] representation of a Surya JSON AST.
  * @param global
  *   shared project-wide information.
  */
class AstCreator(filename: String, sourceUnit: SourceUnit, global: Global) extends AstCreatorBase(filename) {

  private val logger = LoggerFactory.getLogger(classOf[AstCreator])

  /** Add `typeName` to a global map and return it. The map is later passed to a pass that creates TYPE nodes for each
    * key in the map.
    */
  private def registerType(typeName: String): String = {
    global.usedTypes.put(typeName, true)
    typeName
  }

  /** Creates the AST for the given Surya [[SourceUnit]].
    * @return
    *   the changes associated to building the AST.
    */
  override def createAst(): DiffGraphBuilder = {
    val astRoot = astForCompilationUnit(sourceUnit)
    storeInDiffGraph(astRoot)
    diffGraph
  }

  /** Copy nodes/edges of given `AST` into the diff graph
    */
  private def storeInDiffGraph(ast: Ast): scala.Unit = {
    ast.nodes.foreach { node =>
      diffGraph.addNode(node)
    }
    ast.edges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.AST)
    }
    ast.conditionEdges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.CONDITION)
    }
    ast.argEdges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.ARGUMENT)
    }
  }

  private def astForCompilationUnit(sourceUnit: SourceUnit): Ast = {
    // TODO: Import directives may help with resolving types but for now let's ignore
    sourceUnit.children.collectFirst { case x: ImportDirective => x }
    // TODO: Pragma directive will be useful to get which version of Solidity we're using but for now we don't have
    //  anywhere to store that information
    sourceUnit.children.collectFirst { case x: PragmaDirective => x }

    val packageDecl = astForPackageDeclaration()
    val namespaceBlockFullName = {
      packageDecl.root.collect { case x: NewNamespaceBlock => x.fullName }.getOrElse("none")
    }
    val typeDecls: Seq[Ast] = sourceUnit.children.flatMap {
      case x: ContractDefinition => Some(astForTypeDecl(x, namespaceBlockFullName))
      case _                     => None
    }
    packageDecl
      .withChildren(typeDecls)

  }
//TODO: Fix
  private def astForPackageDeclaration(): Ast = {
    val fullName = filename.replace(java.io.File.separator, ".")
    var tmp = filename
    val namespaceBlock = fullName
      .split("\\.")
      .lastOption
      .getOrElse("")
      .replace("\\.*.sol", "") // removes the .SolidityFile.sol at the end of the filename

    if (tmp.contains(".json")) {
      tmp = filename.replace("json", "sol")
    }
//    println("namespaceBlock: "+namespaceBlock + " : "+ tmp+ " : "+ fullName)

    Ast(
      NewNamespaceBlock()
        .name(namespaceBlock)
        .fullName(fullName)
        .filename(tmp)
    )
  }

  private def astForTypeDecl(contractDef: ContractDefinition, astParentFullName: String): Ast = {
    val fullName  = registerType(contractDef.name)
    val shortName = fullName.split("\\.").lastOption.getOrElse(contractDef).toString
    // TODO: Should look out for inheritance/implemented types I think this is in baseContracts? Make sure
    val superTypes = contractDef.baseContracts.map {
      case x: InheritanceSpecifier => {
        x.baseName.namePath
      }
    }
    val typeDecl = NewTypeDecl()
      .name(shortName)
      .fullName(fullName)
      .astParentType(NodeTypes.NAMESPACE_BLOCK)
      .astParentFullName(astParentFullName)
      .filename(filename.substring(0,filename.length -4 )+"sol")
      .inheritsFromTypeFullName(superTypes)
      .code(shortName)
      .isExternal(false)


    val methods= contractDef.subNodes.map(x => astsForMethod(x, fullName))
    val memberAsts = contractDef.subNodes
      .collect{
        case x: StateVariableDeclaration => astForField(x)
        case x: StructDefinition => astForStruct(x, contractDef.name)
      }

    Ast(typeDecl)
      .withChildren(methods)
      .withChildren(memberAsts)
  }

  private def astsForMethod(methods: BaseASTNode, contractname: String): Ast = {
    methods match {
      case x: ModifierDefinition => {
        val methodNode = NewMethod()
          .name(x.name)
        val parameters = x.parameters.collect { case x: VariableDeclaration => x }.map(astForParameter)
        // TODO: Fill these in, try find out what the method return type would be. If multiple then there exists an "any" type
        val methodReturn = NewMethodReturn().typeFullName("")
        Ast(methodNode)
          .withChildren(parameters)
          .withChild(astForBody(x.body.asInstanceOf[Block]))
          .withChild(Ast(methodReturn))
      }
      case x: FunctionDefinition => {
        val parameters = x.parameters.collect { case x: VariableDeclaration => x }.map(astForParameter)
        var name = ""
        var methodReturn = Ast()
        var funcType = ""
        var types = ""
        var params = ""
        var code = ""
        var signature = ""
        var thisNode = Ast()
        var modifiers = Ast()
        /**
          * allowing for constructors or functions
          */
        if (x.name != null) {
          name = x.name
        } else {
          name = "<init>"
        }

        /**
          *  passing returnParameters if found
          */
        if (x.returnParameters != null) {
          methodReturn = astForMethodReturn(x.returnParameters);
          x.returnParameters.collect { case y: VariableDeclaration => {
            y.typeName match {
              case z: ElementaryTypeName => funcType += ":"+z.name
            }
          }
          }
        } else {
          methodReturn = Ast(NewMethodReturn())
          funcType += ":void"
        }

        if (x.modifiers.nonEmpty) {
          modifiers = astForModifiers(x.modifiers);
        }

        /**
          * creating ast node "thisNode"
          */

          thisNode = createThisParameterNode(contractname)
        /**
          * getting names of types and variable names
          */
        params =  parameters.flatMap(_.root).map(_.properties(PropertyNames.CODE)).mkString(", ")
        types = parameters.flatMap(_.root).map(_.properties(PropertyNames.TYPE_FULL_NAME)).mkString(",")
        if (x.name != null ) {
          code = "function " + name + "("
        } else {
          code = "constructor "+ "("
        }
        code += params
        code += ")";

        /**
          * adding visibility into "code"
          */
        if (x.visibility != null && !x.visibility.equals("default")) {
          code += " "+ x.visibility
        }

        /**
          * adding returns into "code" if given
          */
        if (x.returnParameters != null) {
          if (!funcType.equals("")) {
            code += " returns" + " (" + funcType.substring(1, funcType.length) + ")"
          } else {
            code += " returns" + " (" + funcType + ")"
          }
        }

        /**
          * adding to variable "signature"
          */
        if (!funcType.equals("")) {
          signature = funcType.substring(1,funcType.length) + "("+types+")"
        } else {
          signature = funcType + "("+types+")"
        }


        /**
          * creating the new method
          */
        val methodNode = NewMethod()
          .name(name)
          .fullName(contractname+"."+name + funcType + "("+types+")")
          .signature(signature)
          .code(code)
          .filename(filename.substring(0,filename.length -4 )+"sol")

        Ast(methodNode)
          .withChild(thisNode)
          .withChildren(parameters)
          .withChild(astForBody(x.body.asInstanceOf[Block]))
          .withChild(methodReturn)
          .withChild(modifiers)
      }
      case x =>
        logger.warn(s"Unhandled statement of type ${x.getClass}")
        Ast() // etc
    }


  }

  // TODO: I assume the only types coming into parameter are var decls but it's worth making sure in tests
  private def astForParameter(varDecl: VariableDeclaration): Ast = {
    val NewMethodParameter = NewMethodParameterIn();
    var typefullName = ""
    var code = ""
    var visibility = "";
    var storage = ""
    varDecl.typeName match  {
      case x: ElementaryTypeName => typefullName = registerType(x.name)
      case x: Mapping =>  {
        typefullName = registerType("mapping")
        code = getMappingKeyAndValue(x)}
      case x: ArrayTypeName => {
        x.baseTypeName match {
          case x: ElementaryTypeName => typefullName += x.name
        }
        typefullName += "["
        if (x.length != null) {
          typefullName += x.length
        }
        typefullName += "]"
        registerType(typefullName)
      }
//      case x:
    }

    varDecl.visibility match {
      case x: String => visibility = " "+x
      case _ => visibility = ""
    }

    varDecl.storageLocation match {
      case x: String => storage = " "+x
      case _=> storage = ""
    }



    NewMethodParameter
      .name(varDecl.name)
      .code(typefullName + code +visibility + storage + " "+varDecl.name)
      .typeFullName(typefullName)
      .order(1)
      .evaluationStrategy(getEvaluationStrategy(varDecl.typeName.getType))

    Ast(NewMethodParameter)
  }

  private def astForBody(body: Block): Ast = {
    val blockNode = NewBlock()
    Ast(blockNode)
      .withChildren(body.statements.map(astForStatement))
  }

  private def astForStatement(statement: BaseASTNode): Ast = {
    // TODO : Finish all of these statements
    statement match {
      case x: ExpressionStatement => astForExpression(x.expression)
      case x: VariableDeclaration => astForVarDecl(x)
      case x: EmitStatement => Ast()
      case x: ForStatement => Ast()
      case x: IfStatement => Ast()
      case x: ReturnStatement => astForReturn(x)
      case x: VariableDeclarationStatement=> astForVarDeclStat(x)
      case x =>
        logger.warn(s"Unhandled statement of type ${x.getClass}")
        Ast() // etc
    }
  }

  private def astForVarDecl(varDecl: VariableDeclaration): Ast = {
    val newMember = NewMember();
    var typefullName = ""
    var code = ""
    varDecl.typeName match  {
      case x: ElementaryTypeName => typefullName = registerType(x.name)
      case x: Mapping =>  {
        typefullName = registerType("mapping")
        code = getMappingKeyAndValue(x)}
      case x : ArrayTypeName => x.baseTypeName match {
        case x : ElementaryTypeName => typefullName = registerType(x.name)
      }
      case x : UserDefinedTypeName => typefullName = registerType(x.namePath)
      case x : FunctionTypeName => typefullName = registerType("function")
    }
    var visibility = "";
    varDecl.visibility match {
      case x: String => visibility = " "+x
      case _ => visibility = ""
    }
        newMember
          .name(varDecl.name)
          .code(typefullName + code +visibility+ " "+varDecl.name)
          .typeFullName(typefullName)
          .order(1)


      Ast(newMember)

    // TODO: VarDecls should be Local nodes in their block and NOT be duplicated

    // TODO: When a variable is referenced, it should always be referenced as an identifier

  }

  private def astForField(stateVariableDeclaration: StateVariableDeclaration):Ast = {
    var counter = 0
    val fieldType = stateVariableDeclaration.variables.collect{case x: VariableDeclaration => x}.map(astForVarDecl);
    Ast().withChildren(fieldType)
  }

  private def getMappingKeyAndValue (mapping: Mapping) : String = {
    val key = mapping.keyType match {
      case x: ElementaryTypeName => x.name
      case x: Mapping => (getMappingKeyAndValue(x))
    }
    val value = mapping.valueType match {
      case x: ElementaryTypeName => x.name
      case x: Mapping => (getMappingKeyAndValue(x))
    }
    (" ("+key +" => " + value+")")
  }

  private def createThisParameterNode(str : String): Ast = {
    if (str != null) {
      Ast(
        NewMethodParameterIn()
          .name("this")
          .code("this")
          .typeFullName(str)
          //        .typeFullName(registerType(method.getType.toQuotedString))
          //        .dynamicTypeHintFullName(Seq(registerType(method.getType.toQuotedString)))
          .order(0)
          .evaluationStrategy(getEvaluationStrategy("constructor"))
      )
    } else {
      Ast(
        NewMethodParameterIn()
          .name("this")
          .code("this")
          //        .typeFullName(registerType(method.getType.toQuotedString))
          //        .dynamicTypeHintFullName(Seq(registerType(method.getType.toQuotedString)))
          .order(0)
      )
    }
  }

  private def astForMethodReturn(value: List[BaseASTNode]): Ast = {
    val returnMethod= NewMethodReturn()
    var code = ""
    var mapkey = ""
    var visibility = ""
    var name = ""
      value.collect{
        case x: VariableDeclaration => {

          x.typeName match  {
            case x: ElementaryTypeName => name = registerType(x.name)
            case x: Mapping =>  {
              name = registerType("mapping")
              mapkey = getMappingKeyAndValue(x)}
          }

          x.visibility match {
            case x: String => visibility = " "+x
            case _ => visibility = ""
          }
        }
      }
    code =  code +visibility+name
    returnMethod
      .code(code)
      .typeFullName(name)
      .order(1)
    Ast(returnMethod)

  }

  private def astForReturn(returnStatement: ReturnStatement): Ast = {

    val exprAst = astForExpression(returnStatement.expression)
    val returnNode = NewReturn()
      .code(s"return ${(exprAst.root).map(_.properties(PropertyNames.CODE)).mkString(" ")};")
    Ast(
      returnNode
    ).withChild(exprAst)
      .withArgEdges(returnNode, exprAst.root.toList)
  }

  private def astForModifiers(modifiers: List[BaseASTNode]): Ast= {
    val modifierNode = NewModifier()
    var args = ""
    modifiers.collect{
      case x: ModifierInvocation => {
        if (x.arguments != null) {
          x.arguments.collect {
            case x: Identifier => {
              args += x.name + ","
            }
          }
        }
        if (!args.equals("")) {
          args = args.substring(0, args.length - 1)
        }
        modifierNode
          .modifierType(x.name)
          .code(x.name +" ("+args+")")
      }

    }
    (Ast(modifierNode))
  }


  private def astForExpression(expr : BaseASTNode): Ast = {

    expr match {
      case x: MemberAccess => astForMemberAccess(x)
      case x: Identifier => astForIdentifier(x)
      case x: FunctionCall => astForFunctionCall(x)
      case x: BinaryOperation => astForBinaryOperation(x)
      case x: UnaryOperation => astForUnaryOperation(x)
      case x: NumberLiteral => astForNumberLiteral(x)
      case x: BooleanLiteral => astForBooleanLiteral(x)
      case x: StringLiteral => astForStringLiteral(x)
      case x: IndexAccess => astForIndexAccess(x)
      case x: TupleExpression => astForTupleExpression(x)
    }
  }
  private def astForNumberLiteral(numberLiteral: NumberLiteral): Ast ={
    var code = ""
    val typeFullName = registerType(numberLiteral.number)
    if (numberLiteral.subdenomination != null) {
      code = numberLiteral.number + numberLiteral.subdenomination
    } else {
      code = numberLiteral.number
    }
    Ast(NewCall()
      .name(numberLiteral.number)
      .code(code)
      .typeFullName(typeFullName)
    )
  }

  private def astForUnaryOperation(operation: UnaryOperation): Ast = {
    //TODO : finx all cases with "Ast()"
    Ast()
  }

  private def astForBinaryOperation(operation: BinaryOperation): Ast = {
    //TODO : finx all cases with "Ast()"
    Ast()
  }

  private def astForFunctionCall(call: FunctionCall): Ast = {
    var name = ""
    var code = ""
    var args = ""
    var methodFullNameText = ""
    var sig = ""
    val expr = astForExpression(call.expression)
    val arguments = call.arguments.map(astForExpression)
    name = expr.root.map(_.properties(PropertyNames.NAME)).mkString("")
    args = arguments.flatMap(_.root).map(_.properties(PropertyNames.NAME)).mkString(", ")

//    methodFullNameText = expr.root.map(_.properties(PropertyNames.TYPE_FULL_NAME)).mkString("")
//    sig = expr.root.map(_.properties(PropertyNames.SIGNATURE)).mkString("")
    code = name + "(" + args + ")"
    val func = NewCall()
      .name(name)
      .code(name)
//      .methodFullName(methodFullNameText)
//      .signature()

//      .code()
    Ast(func)
      .withChild(expr)
      .withChildren(arguments)
  }

  private def astForIdentifier(identifier: Identifier): Ast = {
    val typeFullName = registerType(identifier.name)
    Ast(NewIdentifier()
      .name(identifier.name)
      .code(identifier.name)
      .typeFullName(typeFullName)
    )
  }

  private def astForMemberAccess(memberAccess: MemberAccess): Ast = {
    Ast()
  }

  private def astForBooleanLiteral(literal: BooleanLiteral): Ast = {
    Ast()
  }


  private def astForStringLiteral(x: StringLiteral): Ast = {
    Ast()
  }

  private def astForIndexAccess (x: IndexAccess) : Ast = {
    Ast()
  }

  private def astForTupleExpression(expression: TupleExpression) : Ast = {
    Ast()
  }

  private def astForVarDeclStat(statement: VariableDeclarationStatement): Ast = {
    val vars = statement.variables.map(x => astForStatement(x))


  Ast().withChildren(vars)
  }

  private def astForStruct(structDefinition: StructDefinition, contractName : String): Ast = {
    val typeFullName = registerType(contractName+"."+structDefinition.name)
    val memberNode = NewMember()
      .typeFullName(contractName+"."+typeFullName)
      .name(typeFullName)

    val members = structDefinition.members.collect {
      case x: VariableDeclaration => astForVarDecl(x)
    }
    Ast(memberNode).withChildren(members)
  }



  private def getEvaluationStrategy(typ: String): String =
    typ match {
      case x: String  => {
        if (x.equals("ElementaryTypeName")) {
          EvaluationStrategies.BY_VALUE
        } else if (x.equals("UserDefinedTypeName")) {
          EvaluationStrategies.BY_VALUE
        } else if (x.equals("ArrayTypeName")) {
          EvaluationStrategies.BY_REFERENCE
        } else if (x.equals("Mapping")) {
          EvaluationStrategies.BY_REFERENCE
        } else {
          EvaluationStrategies.BY_SHARING
        }
      }
      case _              => EvaluationStrategies.BY_SHARING
    }

//  private def astForInheritanceSpecifier(inheritanceSpecifier: InheritanceSpecifier) : Ast = {
//
//  }

}