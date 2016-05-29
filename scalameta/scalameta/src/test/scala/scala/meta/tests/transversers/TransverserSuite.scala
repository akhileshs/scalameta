package scala.meta.tests
package transversers

import org.scalatest._
import scala.compat.Platform.EOL
import scala.meta._
import scala.meta.internal.semantic._
import scala.meta.internal.prettyprinters._
import scala.meta.internal.ast.Origin

class TransverserSuite extends FunSuite {  
  test("Traverser Ok") {
    val tree0 = q"""
      def foo(x: x)(x: Int) = x + x
      class C(x: x) {
        def bar(x: x) = ???
      }
    """
    val log = scala.collection.mutable.ListBuffer[String]()
    object traverser extends Traverser {
      override def apply(tree: Tree): Unit = {
        log += tree.toString.trim.replace("\n", " ")
        super.apply(tree)
      }
    }
    traverser(tree0)
    assert(log.mkString(EOL) === """
      |{   def foo(x: x)(x: Int) = x + x   class C(x: x) { def bar(x: x) = ??? } }
      |def foo(x: x)(x: Int) = x + x
      |foo
      |x: x
      |x
      |x
      |x: Int
      |x
      |Int
      |x + x
      |x
      |+
      |x
      |class C(x: x) { def bar(x: x) = ??? }
      |C
      |def this(x: x)
      |this
      |x: x
      |x
      |x
      |{ def bar(x: x) = ??? }
      |_
      |_
      |def bar(x: x) = ???
      |bar
      |x: x
      |x
      |x
      |???
    """.trim.stripMargin)
  }

  test("Transformer Ok") {
    val tree0 = q"""
      def foo(x: x)(x: Int) = x + x
      class C(x: x) {
        def bar(x: x) = ???
      }
    """
    val log = scala.collection.mutable.ListBuffer[String]()
    object transformer extends Transformer {
      override def apply(tree: Tree): Tree = tree match {
        case Term.Name("x") => Term.Name("y")
        case Type.Name("x") => Type.Name("y")
        case _ => super.apply(tree)
      }
    }
    val tree1 = transformer(tree0)
    assert(tree1.toString === """
      |{
      |  def foo(y: y)(y: Int) = y + y
      |  class C(y: y) { def bar(y: y) = ??? }
      |}
    """.trim.stripMargin)
  }

  test("Transformer Fail") {
    val tree0 = q"""
      def foo(x: x)(x: Int) = x + x
      class C(x: x) {
        def bar(x: x) = ???
      }
    """
    val log = scala.collection.mutable.ListBuffer[String]()
    object transformer extends Transformer {
      override def apply(tree: Tree): Tree = {
        if (tree.toString == "x") q"y"
        else super.apply(tree)
      }
    }
    intercept[UnsupportedOperationException]{ transformer(tree0) }
  }

  test("Transformed Attributes") {
    def attributeTypeName(name: Type.Name): Type.Name = name.withAttrs(Denotation.Single(Prefix.None, Symbol.RootPackage))
    val Foo = attributeTypeName(Type.Name("Foo"))
    def attributeTermName(name: Term.Name): Term.Name = name.withAttrs(Denotation.Single(Prefix.None, Symbol.RootPackage), Foo.setTypechecked)
    def attributeTerm(term: Term): Term = term.withAttrs(Foo.setTypechecked)
    val denot1 = Denotation.Single(Prefix.None, Symbol.RootPackage)
    val denot2 = Denotation.Single(Prefix.None, Symbol.EmptyPackage)
    val typing = Foo.setTypechecked
    val x = q"x".withAttrs(denot1, typing).setTypechecked
    val z = q"z".withAttrs(denot2, typing).setTypechecked
    val attr0 = q"$x + $z".withAttrs(typing)
    assert(attr0.show[Attributes] === """
      |Term.ApplyInfix(Term.Name("x")[1]{1}, Term.Name("+")*, Nil, Seq(Term.Name("z")[2]{1})){1}*
      |[1] {0}::_root_
      |[2] {0}::_empty_
      |{1} Type.Name("Foo")[1]
    """.trim.stripMargin)

    object transformer extends Transformer {
      override def apply(tree: Tree): Tree = tree match {
        case Term.Name("x") => Term.Name("y")
        case Type.Name("x") => Type.Name("y")
        case _ => super.apply(tree)
      }
    }

    val attr1 = transformer(attr0)
    assert(attr1.show[Attributes] === """
      |Term.ApplyInfix(Term.Name("y")*, Term.Name("+")*, Nil, Seq(Term.Name("z")[1]{1}))*
      |[1] {0}::_empty_
      |[2] {0}::_root_
      |{1} Type.Name("Foo")[2]
    """.trim.stripMargin)
  }

  test("Tree.transform") {
    val tree0 = q"x + y"
    val tree1 = tree0.transform { case Term.Name(s) => Term.Name(s + s) }
    assert(tree1.toString == "xx ++ yy")
  }

  test("Tree.traverse") {
    var cnt = 0
    val tree0 = q"x + y"
    tree0.traverse { case Term.Name(s) => cnt += 1 }
    assert(cnt == 3)
  }

  test("Tree.collect") {
    val tree0 = q"x + y"
    val result1 = tree0.collect { case Term.Name(s) => s }
    assert(result1.toString == "List(x, +, y)")
  }

  test("Origin preserving transforms") {
    val tree0 = "{ /* hello */ def foo(bar: Int) = bar }".parse[Term].get
    val result1 = tree0 transform { case q"bar" => q"baz" }
    result1.origin match {
      case Origin.Transformed(tree) =>
        assert(tree0 eq tree)
        assert(tree.origin eq tree0.origin)
        assert(tree0.children.map(_.origin) == tree.children.map(_.origin))
      case _ => assert(false)

    }
  }

  
  test("Preserve formatting basic test") {
    val tree0 = "{ /* hello */ def foo(bar: Int) = bar }".parse[Term].get
    val result1 = tree0 transform { case q"bar" => q"baz" }
    assert(result1.toString == "{ /* hello */ def foo(baz: Int) = baz }")
  }

  test("Basic transform tests") {
    val tree0 = "{ /* hello */ def foo(bar: Int) = bar }".parse[Term].get
    val result1 = tree0 transform { case q"bar" => q"bar" }
    assert(result1.toString == tree0.toString)
  }

  test("weirdly indented code") {
    val tree = """{
      if (true) {
           1
      } else {
        2

        }

    }""".parse[Stat].get
    val result1 = tree transform { case q"true" => q"false" }
    val s = """{
      if (false) {
           1
      } else {
        2

        }

    }"""
    assert(result1.toString == s)
  }

  test("simple transform with match") {
    val tree0 = """
      def foo(bar: Int) = bar match {
        case 1 => 1
        case _ => 2
        }""".parse[Stat].get
    val result1 = tree0 transform { case q"bar" => q"baz" }
    val s = """
      def foo(baz: Int) = baz match {
        case 1 => 1
        case _ => 2
        }"""

    assert(result1.toString == s)
  }

  test("Simple if test") {
    val tree0 = """ if (true) 1 else 2""".parse[Term].get
    val result1 = tree0 transform { case q"true" => q"false" }
    val s = """ if (false) 1 else 2"""
    assert(result1.toString == s)
  }

  test("Weirdly indented if") {
    val tree0 = """if          (x)
       true
else
               false
""".parse[Term].get
    val result1 = tree0 transform { case q"true" => q"false" }
    val s = """if          (x)
       false
else
               false
"""
    assert(result1.toString == s)
  }
}
