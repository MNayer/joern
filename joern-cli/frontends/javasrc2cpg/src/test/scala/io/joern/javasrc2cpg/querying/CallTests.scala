package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.{JavaSrcCode2CpgFixture, JavaSrcCodeToCpgFixture}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators, nodes}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier, Literal}
import io.shiftleft.semanticcpg.language.NoResolve
import io.shiftleft.semanticcpg.language._

class NewCallTests extends JavaSrcCode2CpgFixture {
  "call to method in different class" should {
    val cpg = code(
      """
        |class Base {
        |  void method(int aaa) {}
        |}
        |""".stripMargin,
      "Base.java"
    ).moreCode(
      """
        |class Derived extends Base {}
        |""".stripMargin,
      "Derived.java"
    ).moreCode("""
        |class User {
        |  static void user() {
        |    Derived derived = new Derived();
        |    derived.method(1);
        |  }
        |}
        |""".stripMargin)

    "have correct methodFullName" in {
      cpg.call.nameExact("method").methodFullName.head shouldBe "Derived.method:void(int)"
    }
  }

  "call to method in same class" should {
    val cpg = code(
      """
        |class Base {
        |  void method(int aaa) {}
        |}
        |""".stripMargin,
      "Base.java"
    ).moreCode(
      """
        |class Derived extends Base {}
        |""".stripMargin,
      "Derived.java"
    ).moreCode("""
        |class MoreDerived extends Derived {
        |  void user() {
        |    method(1);
        |  }
        |}
        |""".stripMargin)

    "have correct methodFullName" in {
      cpg.call.nameExact("method").methodFullName.head shouldBe "MoreDerived.method:void(int)"
    }
  }

  "code fields" should {
    "be correct for chained calls starting at a constructor invocation" in {
      val cpg = code("""
          |class Foo {
          |  private String value;
          |
          |  public String getValue() {
          |    return value;
          |  }
          |
          |  public static void test() {
          |    String s = new Foo().getValue();
          |  }
          |}
          |""".stripMargin)

      cpg.call.name("getValue").l match {
        case List(getValueCall) =>
          getValueCall.code shouldBe "new Foo().getValue()"

        case result => fail(s"Expected single getValue call but got $result")
      }
    }

    "be correct for constructor invocations" in {
      val cpg = code("""
          |class Foo {
          |
          |  public static void test() {
          |    Foo f = new Foo();
          |  }
          |}
          |""".stripMargin)
      cpg.call.name("<init>").l match {
        case List(initCall: Call) =>
          initCall.code shouldBe "new Foo()"

        case result => fail(s"Expected single init call but got $result")
      }
    }
  }

  "call to method with generic return type" should {
    val cpg = code("""
        |class Foo {
        |  void method(java.util.function.Function<String, Integer> supplier) {
        |     supplier.apply("abc");
        |  }
        |}
        |""".stripMargin)

    "have correct substitute type as expression type" in {
      cpg.call.name("apply").evalType.head shouldBe "java.lang.Integer"
    }
    "have correct methodFullName to erased method signature" in {
      cpg.call.name("apply").methodFullName.head shouldBe
        "java.util.function.Function.apply:java.lang.Object(java.lang.Object)"
    }
  }

  "call to generic method of generic type" should {
    val cpg = code("""
        |class Foo <T extends Number> {
        |  <S extends T> void foo(S i) {}
        |
        |  static void method() {
        |    Foo<Integer> obj = new Foo();
        |    obj.foo(1);
        |  }
        |}
        |""".stripMargin)

    "have correct methodFullName" in {
      cpg.call("foo").methodFullName.head shouldBe "Foo.foo:void(java.lang.Number)"
    }
  }

  "call to method with generic array parameter" should {
    val cpg = code("""
        |class Foo <T> {
        |  void foo(T[] aaa) {}
        |
        |  static void method() {
        |    Foo<Integer> obj = new Foo();
        |    Integer[] array = new Integer[3];
        |    obj.foo(array);
        |  }
        |}
        |""".stripMargin)

    "should have correct methodFullName" in {
      cpg.call("foo").methodFullName.head shouldBe "Foo.foo:void(java.lang.Object[])"
    }
  }

  "call to super method" should {
    val cpg = code("""
        |class Foo {
        |  @Override
        |  public String toString() {
        |    return super.toString();
        |  }
        |}
        |""".stripMargin)

    "create a `super` receiver with fields correctly set" in {
      val superReceiver = cpg.call.name("toString").receiver.collectAll[Identifier].head
      superReceiver.name shouldBe "this"
      superReceiver.code shouldBe "super"
      superReceiver.typeFullName shouldBe "java.lang.Object"
      superReceiver.order shouldBe 1
      superReceiver.argumentIndex shouldBe 0
      superReceiver.lineNumber shouldBe Some(5)
      superReceiver.columnNumber shouldBe Some(12)
    }
  }
}

class CallTests extends JavaSrcCodeToCpgFixture {

  implicit val resolver: ICallResolver = NoResolve

  override val code: String =
    """
      |package test;
      | class Foo {
      |   int add(int x, int y) {
      |     return x + y;
      |   }
      |
      |   int main(int argc, char argv) {
      |     return add(argc, 3);
      |   }
      |
      |   int bar(int argc) {
      |     foo(argc);
      |   }
      | }
      |
      |class MyObject {
      |    public static String staticCall(String s) {
      |        return s;
      |    }
      |
      |    public String myMethod(String s) {
      |        return s;
      |    }
      |}
      |
      |public class Bar {
      |    MyObject obj = new MyObject();
      |
      |    public static void staticMethod() {}
      |
      |    public String foo(MyObject myObj) {
      |        return myObj.myMethod("Hello, world!");
      |    }
      |
      |    public void bar() {
      |        foo(obj);
      |    }
      |
      |    public void baz() {
      |        this.foo(obj);
      |    }
      |
      |    public void qux() {
      |        staticMethod();
      |    }
      |
      |    public void quux() {
      |      bar();
      |    }
      |}
      |""".stripMargin

  "should contain a call node for `add` with correct fields" in {
    val List(x) = cpg.call("add").l
    x.code shouldBe "this.add(argc, 3)"
    x.name shouldBe "add"
    x.order shouldBe 1
    x.methodFullName shouldBe "test.Foo.add:int(int,int)"
    x.signature shouldBe "int(int,int)"
    x.argumentIndex shouldBe 1
    x.lineNumber shouldBe Some(9)
  }

  "should allow traversing from call to arguments" in {
    cpg.call("add").argument.size shouldBe 3
    val List(arg0) = cpg.call("add").receiver.l
    arg0.isInstanceOf[nodes.Identifier] shouldBe true
    arg0.asInstanceOf[nodes.Identifier].name shouldBe "this"
    arg0.code shouldBe "this"
    arg0.order shouldBe 1
    arg0.argumentIndex shouldBe 0

    val List(arg1) = cpg.call("add").argument(1).l
    arg1.isInstanceOf[nodes.Identifier] shouldBe true
    arg1.asInstanceOf[nodes.Identifier].name shouldBe "argc"
    arg1.code shouldBe "argc"
    arg1.order shouldBe 2
    arg1.argumentIndex shouldBe 1

    val List(arg2) = cpg.call("add").argument(2).l
    arg2.asInstanceOf[nodes.Literal].code shouldBe "3"
    arg2.isInstanceOf[nodes.Literal] shouldBe true
    arg2.code shouldBe "3"
    arg2.order shouldBe 3
    arg2.argumentIndex shouldBe 2
  }

  "should allow traversing from call to surrounding method" in {
    val List(x) = cpg.call("add").method.l
    x.name shouldBe "main"
  }

  "should allow traversing from call to callee method" in {
    val List(x) = cpg.call("add").callee.l
    x.name shouldBe "add"
  }

  "should allow traversing from argument to parameter" in {
    val List(x) = cpg.call("add").argument(1).parameter.l
    x.name shouldBe "x"
  }

  "should handle unresolved calls with appropriate defaults" in {
    val List(call: Call) = cpg.typeDecl.name("Foo").ast.isCall.name("foo").l
    call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
    call.methodFullName shouldBe "test.Foo.foo:void(int)"
    call.signature shouldBe "void(int)"
    call.code shouldBe "foo(argc)"
  }

  "should create a call node for call on explicit object" in {
    val call = cpg.typeDecl.name("Bar").method.name("foo").call.nameExact("myMethod").head

    call.code shouldBe "myObj.myMethod(\"Hello, world!\")"
    call.name shouldBe "myMethod"
    call.methodFullName shouldBe "test.MyObject.myMethod:java.lang.String(java.lang.String)"
    call.signature shouldBe "java.lang.String(java.lang.String)"
    call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

    val List(objName: Identifier, argument: Literal) = call.astChildren.l

    objName.order shouldBe 1
    objName.argumentIndex shouldBe 0
    objName.code shouldBe "myObj"
    objName.name shouldBe "myObj"

    argument.code shouldBe "\"Hello, world!\""
    argument.order shouldBe 2
    argument.argumentIndex shouldBe 1
  }

  "should create a call node for a call with an implicit `this`" in {
    val call = cpg.typeDecl.name("Bar").method.name("bar").call.nameExact("foo").head

    call.code shouldBe "this.foo(obj)"
    call.name shouldBe "foo"
    call.methodFullName shouldBe "test.Bar.foo:java.lang.String(test.MyObject)"
    call.signature shouldBe "java.lang.String(test.MyObject)"
    call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

    val List(identifier: Identifier, argument: Call) = call.argument.l
    identifier.order shouldBe 1
    identifier.argumentIndex shouldBe 0
    identifier.code shouldBe "this"
    identifier.name shouldBe "this"

    argument.name shouldBe Operators.fieldAccess
    argument.typeFullName shouldBe "test.MyObject"

    val List(ident: Identifier, fieldIdent: FieldIdentifier) = argument.argument.l
    ident.name shouldBe "this"
    fieldIdent.canonicalName shouldBe "obj"
  }

  "should create a call node for a call with an explicit `this`" in {
    val call = cpg.typeDecl.name("Bar").method.name("baz").call.nameExact("foo").head

    call.code shouldBe "this.foo(obj)"
    call.name shouldBe "foo"
    call.methodFullName shouldBe "test.Bar.foo:java.lang.String(test.MyObject)"
    call.signature shouldBe "java.lang.String(test.MyObject)"
    call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

    val List(objName: Identifier, argument: Call) = call.astChildren.l

    objName.order shouldBe 1
    objName.argumentIndex shouldBe 0
    objName.name shouldBe "this"
    objName.code shouldBe "this"

    argument.name shouldBe Operators.fieldAccess
    argument.typeFullName shouldBe "test.MyObject"
    argument.code shouldBe "obj"
    argument.order shouldBe 2
    argument.argumentIndex shouldBe 1

    val List(ident: Identifier, fieldIdent: FieldIdentifier) = argument.argument.l
    ident.name shouldBe "this"
    fieldIdent.canonicalName shouldBe "obj"
  }

  "should create correct code field for static call" in {
    val call = cpg.typeDecl.name("Bar").method.name("qux").call.head
    call.name shouldBe "staticMethod"
    call.methodFullName shouldBe "test.Bar.staticMethod:void()"
    call.code shouldBe "staticMethod()"
  }

  "should create correct call signature for method call without args" in {
    val call = cpg.typeDecl.name("Bar").method.name("quux").call.head
    call.name shouldBe "bar"
    call.methodFullName shouldBe "test.Bar.bar:void()"
    call.signature shouldBe "void()"
  }
}

class CallTests2 extends JavaSrcCodeToCpgFixture {
  override val code: String =
    """
      |class Foo {
      |    public static class Ops {
      |        public <T> T ident(T x) {
      |            return x;
      |        }
      |    }
      |    public Integer method(Integer aaa) {
      |        Ops ops = new Ops();
      |        Integer ret = ops.ident(aaa);
      |        return ret;
      |    }
      |}
      |""".stripMargin

  "test methodFullName for call to generic function" in {
    cpg.call(".*ident.*").methodFullName.head shouldBe "Foo$Ops.ident:java.lang.Object(java.lang.Object)"
  }
}
