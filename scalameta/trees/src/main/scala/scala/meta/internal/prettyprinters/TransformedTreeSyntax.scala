package scala.meta
package internal
package prettyprinters

import scala.meta.dialects.Quasiquote
import scala.meta.prettyprinters._
import scala.meta.prettyprinters.Syntax._
import Show.{ sequence => s }

object TransformedTreeSyntax {
  def printTransformedTree[T <: Tree](orig: T, transformed: T): Show.Result = {
    val origInputChars = orig.pos.input.chars
    val origPosStart = orig.pos.start.offset
    val sb = new StringBuilder
    def printResultTree(sb: StringBuilder, t1: Tree, t2: Tree, startPos: Int): String = {
      if (t1 ne t2) {
        sb.append(new String(origInputChars, startPos, t1.pos.start.offset - startPos))
        sb.append(t2)
      }
      else sb.append(new String(origInputChars, startPos, t1.pos.end.offset - startPos))      
      sb.toString
    }

    def printResultOptionTree(sb: StringBuilder, t1: Option[Tree], t2: Option[Tree], startPos: Int): String = {
      (t1, t2) match {
        case (Some(t11), Some(t22)) =>
          sb.append(printResultTree(sb, t11, t22, startPos))
          sb.toString
        case _ => sb.toString
      }      
    }

    def printResultSeqTree(sb: StringBuilder, t1: Seq[Tree], t2: Seq[Tree], startPos: Int): String = {
      if (t1.length == 0) sb.toString
      else {
        if (t1.head ne t2.head) {
          sb.append(new String(origInputChars, startPos, t1.head.pos.start.offset - startPos))
          sb.append(t2.head)
        }
        else sb.append(new String(origInputChars, startPos, t1.head.pos.end.offset - startPos))
        
        for (List(t10, t11) <- (t1 zip t2).grouped(2)) {
          if (t11._1 ne t11._2) {
            sb.append(new String(origInputChars, t10._1.pos.end.offset, t11._1.pos.start.offset - t10._1.pos.end.offset))
            sb.append(t11._2)
          }
          else sb.append(new String(origInputChars, t10._1.pos.end.offset, t11._1.pos.end.offset - t10._1.pos.end.offset))
        }

        if (t1.length > 2) {
          if (t1.last ne t2.last) {
            sb.append(new String(origInputChars, t1(t1.length - 2).pos.end.offset, t1.last.pos.start.offset - t1(t1.length - 2).pos.end.offset))
            sb.append(t2.last)
          }
          else sb.append(new String(origInputChars, t1(t1.length - 2).pos.end.offset, t1.last.pos.end.offset - t1(t1.length - 2).pos.end.offset))
        }
        sb.toString
      }
    }

    case class TreeTree(t1: Tree, t2: Tree)
    case class SeqSeq(t1: Seq[Tree], t2: Seq[Tree])
    case class SeqTree(t1: Seq[Tree], t2: Tree)
     
    def printRemainder(sb: StringBuilder, t1: Any, startPos: Int): String = {
      t1 match {
        case x0: Tree => sb.append(new String(origInputChars, startPos, orig.pos.end.offset - startPos))
        case x0: Seq[Tree] =>
          if (x0.length > 0) sb.append(new String(origInputChars, startPos, orig.pos.end.offset - startPos))
          else sb.toString
        case _ => "Handle this later"
      }
      sb.toString
    }

    def printThreeChildren(sb: StringBuilder, p1: TreeTree, p2: TreeTree, p3: TreeTree) = {
      printResultTree(sb, p1.t1, p1.t2, origPosStart)
      printResultTree(sb, p2.t1, p2.t2, p1.t1.pos.end.offset)
      printResultTree(sb, p3.t1, p3.t2, p2.t1.pos.end.offset)
      printRemainder(sb, p3.t1, p3.t1.pos.end.offset)
    }

    def printTwoChildren(sb: StringBuilder, c1: Any, c2: Any) = {
      (c1, c2) match {        
        case (x1: SeqSeq, x2: TreeTree) =>
          printResultSeqTree(sb, x1.t1, x1.t2, origPosStart)
          if (x1.t1.length > 0) printResultTree(sb, x2.t1, x2.t2, x1.t1.last.pos.end.offset)
          else printResultTree(sb, x2.t1, x2.t2, origPosStart)
        case (x1: TreeTree, x2: SeqSeq) =>
          printResultTree(sb, x1.t1, x1.t2, origPosStart)
          printResultSeqTree(sb, x2.t1, x2.t2, x1.t1.pos.end.offset)
          if (x2.t1.length > 0) printRemainder(sb, x2.t1, x2.t1.last.pos.end.offset)
          else printRemainder(sb, x1.t1, x1.t1.pos.end.offset)
        case (x1: TreeTree, x2: TreeTree) =>
          printResultTree(sb, x1.t1, x1.t2, origPosStart)
          printResultTree(sb, x2.t1, x2.t2, x1.t1.pos.end.offset)
          printRemainder(sb, x2.t1, x2.t1.pos.end.offset)
        case _ => ""
      }      
    }

    def printOneChild(sb: StringBuilder, p1: Any) = {
      p1 match {
        case x1: TreeTree =>
          printResultTree(sb, x1.t1, x1.t2, origPosStart)
          printRemainder(sb, x1.t1, x1.t1.pos.end.offset)

        case x1: SeqSeq =>
          printResultSeqTree(sb, x1.t1, x1.t2, origPosStart)
          if (x1.t1.length > 0) printRemainder(sb, x1.t1, x1.t1.last.pos.end.offset)
          else printRemainder(sb, x1.t1, origPosStart)        
        case _ => ""
      }     
    }

    (orig, transformed) match {
      case (Term.If(cond0, thenp0, elsep0), Term.If(cond1, thenp1, elsep1)) =>
        printThreeChildren(sb, TreeTree(cond0, cond1), TreeTree(thenp0, thenp1), TreeTree(elsep0, elsep1))        
        s(sb.toString)
        
      case (Term.While(expr0, body0), Term.While(expr1, body1)) =>
        printTwoChildren(sb, TreeTree(expr0, expr1), TreeTree(body0, body1))
        s(sb.toString)
        
      case (Term.Do(body0, expr0), Term.Do(body1, expr1)) =>
        printTwoChildren(sb, TreeTree(body0, body1), TreeTree(expr0, expr1))
        s(sb.toString)

      case (Term.Tuple(es0), Term.Tuple(es1)) =>
        printOneChild(sb, SeqSeq(es0, es1))
        s(sb.toString)
        
      case (Term.Eta(t0), Term.Eta(t1)) =>
        printOneChild(sb, TreeTree(t0, t1))
        s(sb.toString)
        
      case (Term.Throw(t0), Term.Throw(t1)) =>
        printOneChild(sb, TreeTree(t0, t1))
        s(sb.toString)
        
      case (Term.Return(t0), Term.Return(t1)) =>
        printOneChild(sb, TreeTree(t0, t1))
        s(sb.toString)
        
      case (Term.Assign(lhs0, rhs0), Term.Assign(lhs1, rhs1)) =>
        printTwoChildren(sb, TreeTree(lhs0, lhs1), TreeTree(rhs0, rhs1))
        s(sb.toString)
        
      case (Term.Ascribe(e0, tpe0), Term.Ascribe(e1, tpe1)) =>
        printTwoChildren(sb, TreeTree(e0, e1), TreeTree(tpe0, tpe1))
        s(sb.toString)
        
      case (Term.Select(qual0, name0), Term.Select(qual1, name1)) =>
        printTwoChildren(sb, TreeTree(qual0, qual1), TreeTree(name0, name1))
        s(sb.toString)
        
      case (Term.ApplyUnary(op0, arg0), Term.ApplyUnary(op1, arg1)) =>
        printTwoChildren(sb, TreeTree(op0, op1), TreeTree(arg0, arg1))
        s(sb.toString)
        
      case (Term.Arg.Named(name0, rhs0), Term.Arg.Named(name1, rhs1)) =>
        printTwoChildren(sb, TreeTree(name0, name1), TreeTree(rhs0, rhs1))
        s(sb.toString)

      case (Term.Arg.Repeated(t0), Term.Arg.Repeated(t1)) =>
        printOneChild(sb, TreeTree(t0, t1))
        s(sb.toString)
        /*
      case (Term.Match(scrut0, cases0), Term.Match(scrut1, cases1)) =>
        // this is probably the only hairy case
        sb.append(printResultTree(scrut0, scrut1, origPosStart))

        (cases0.head, cases1.head) match {
          case (Case(pat0, cond0, body0), Case(pat1, cond1, body1)) =>
            sb.append(printResultTree(pat0, pat1, scrut0.pos.end.offset))
            sb.append(printResultOptionTree(cond0, cond1, pat0.pos.end.offset))
              (cond0, cond1) match {
              case (Some(cond00), Some(cond11)) => sb.append(printResultTree(body0, body1, cond00.pos.end.offset))
              case _ => sb.append(printResultTree(body0, body1, pat0.pos.end.offset))
            }
        }

        for (List(cases00, cases01) <- (cases0 zip cases1).grouped(2)) {
          (cases01._1, cases01._2) match {
            case (Case(pat0, cond0, body0), Case(pat1, cond1, body1)) =>
              sb.append(printResultTree(pat0, pat1, cases00._1.body.pos.end.offset))
              sb.append(printResultOptionTree(cond0, cond1, pat0.pos.end.offset))
                (cond0, cond1) match {
                case (Some(cond00), Some(cond11)) => sb.append(printResultTree(body0, body1, cond00.pos.end.offset))
                case _ => sb.append(printResultTree(body0, body1, pat0.pos.end.offset))
              }
          }
        }

        if (cases0.length > 2) {
          (cases0.last, cases1.last) match {
            case (Case(pat0, cond0, body0), Case(pat1, cond1, body1)) =>
              sb.append(printResultTree(pat0, pat1, cases0(cases0.length - 2).body.pos.end.offset))
              sb.append(printResultOptionTree(cond0, cond1, pat0.pos.end.offset))
                (cond0, cond1) match {
                case (Some(cond00), Some(cond11)) => sb.append(printResultTree(body0, body1, cond00.pos.end.offset))
                case _ => sb.append(printResultTree(body0, body1, pat0.pos.end.offset))
              }
              sb.append(printRemainder(body0, body0.pos.end.offset))
          }
        }
        else sb.append(new String(origInputChars, cases0.last.body.pos.end.offset, orig.pos.end.offset - cases0.last.body.pos.end.offset))

        s(sb.toString)

         */

      case (Term.Function(params0, body0), Term.Function(params1, body1)) =>
        printTwoChildren(sb, (params0, params1), (body0, body1))
        s(sb.toString)
        /*
      case (Term.ApplyInfix(lhs0, op0, targs0, args0), Term.ApplyInfix(lhs1, op1, targs1, args1)) =>

        sb.append(printResultTree(lhs0, lhs1, origPosStart))
        sb.append(printResultTree(op0, op1, lhs0.pos.end.offset))
        sb.append(printResultSeqTree(targs0, targs1, op0.pos.end.offset))
        if (targs0.length > 0) sb.append(printResultSeqTree(args0, args1, targs0.last.pos.end.offset))
        else sb.append(printResultSeqTree(args0, args1, op0.pos.end.offset))
        if (args0.length > 0) sb.append(printRemainder(args0, args0.last.pos.end.offset))
        else {
          if (targs0.length > 0) sb.append(printRemainder(targs0, targs0.last.pos.end.offset))
          else sb.append(printRemainder(op0, op0.pos.end.offset))
        }
        
        s(sb.toString)
         */
        
      case (Term.For(enums0, body0), Term.For(enums1, body1)) =>
        printTwoChildren(sb, (enums0, enums1), (body0, body1))        
        s(sb.toString)
        
      case (Term.ForYield(enums0, body0), Term.ForYield(enums1, body1)) =>
        printTwoChildren(sb, (enums0, enums1), (body0, body1))        
        s(sb.toString)

        /*
      case (Term.Param(mods0, name0, decltpe0, default0), Term.Param(mods1, name1, decltpe1, default1)) =>

        sb.append(printResultSeqTree(mods0, mods1, origPosStart))
        if (mods0.length > 0) sb.append(printResultTree(name0, name1, mods0.last.pos.end.offset))
        else sb.append(printResultTree(name0, name1, origPosStart))
        sb.append(printResultOptionTree(decltpe0, decltpe1, name0.pos.end.offset))
        decltpe0 match {
          case Some(decltpe00) => sb.append(printResultOptionTree(default0, default1, decltpe00.pos.end.offset))
          case _ => sb.append(printResultOptionTree(default0, default1, name0.pos.end.offset))
        }
        default0 match {
          case (Some(default00)) => sb.append(printRemainder(default00, default00.pos.end.offset))
          case _ =>
            decltpe0 match {
              case Some(decltpe00) => sb.append(printRemainder(decltpe00, decltpe00.pos.end.offset))
              case _ => sb.append(printRemainder(name0, name0.pos.end.offset))
            }
        }

        s(sb.toString)
         
        
      case (Term.TryWithTerm(expr0, catchp0, finallyp0), Term.TryWithTerm(expr1, catchp1, finallyp1)) =>

        sb.append(printResultTree(expr0, expr1, origPosStart))
        sb.append(printResultTree(catchp0, catchp1, expr0.pos.end.offset))
        sb.append(printResultOptionTree(finallyp0, finallyp1, catchp0.pos.end.offset))
        finallyp0 match {
          case (Some(finallyp00)) => sb.append(printRemainder(finallyp00, finallyp00.pos.end.offset))
          case _ => sb.append(printRemainder(catchp0, catchp0.pos.end.offset))

        }

        s(sb.toString)
        
      case (Term.TryWithCases(expr0, catchp0, finallyp0) , Term.TryWithCases(expr1, catchp1, finallyp1)) =>

        sb.append(printResultTree(expr0, expr1, origPosStart))
        sb.append(printResultSeqTree(catchp0, catchp1, expr0.pos.end.offset))
        if (catchp0.length > 0) sb.append(printResultOptionTree(finallyp0, finallyp1, catchp0.last.pos.end.offset))
        else sb.append(printResultOptionTree(finallyp0, finallyp1, expr0.pos.end.offset))
        finallyp0 match {
          case (Some(finallyp00)) => sb.append(printRemainder(finallyp00, finallyp00.pos.end.offset))
          case _ =>
            if (catchp0.length > 0) sb.append(printRemainder(catchp0, catchp0.last.pos.end.offset))
            else sb.append(printRemainder(expr0, expr0.pos.end.offset))
        }

        s(sb.toString)
         */
        
      case (Term.Block(stats0), Term.Block(stats1)) =>
        printOneChild(sb, (stats0, stats1))
        s(sb.toString)       

      case (Term.Name(v0), Term.Name(v1)) =>
        val sb = new StringBuilder

        if (v0 ne v1) sb.append(v1)
        else sb.append(v0)

        s(sb.toString)

      case (Term.Select(qual0, name0), Term.Select(qual1, name1)) =>
        printTwoChildren(sb, (qual0, qual1), (name0, name1))
        s(sb.toString)

      case (Term.Apply(fun0, args0), Term.Apply(fun1, args1)) =>
        printTwoChildren(sb, (fun0, fun1), (args0, args1))
        s(sb.toString)
        
      case (Term.Annotate(expr0, annots0), Term.Annotate(expr1, annots1)) =>
        printTwoChildren(sb, (expr0, expr1), (annots0, annots1))
        s(sb.toString)

      case (Term.PartialFunction(cases0), Term.PartialFunction(cases1)) =>
        printOneChild(sb, (cases0, cases1))
        s(sb.toString)

      /* Pat cases */
      case (Pat.Var.Term(name0), Pat.Var.Term(name1)) =>
        printOneChild(sb, (name0, name1))
        s(sb.toString)

      case (Pat.Var.Type(name0), Pat.Var.Type(name1)) =>
        printOneChild(sb, (name0, name1))
        s(sb.toString)
        
      case (Pat.Bind(lhs0, rhs0), Pat.Bind(lhs1, rhs1)) =>
        printTwoChildren(sb, (lhs0, lhs1), (rhs0, rhs1))
        s(sb.toString)
        
      case (Pat.Alternative(lhs0, rhs0), Pat.Alternative(lhs1, rhs1)) =>
        printTwoChildren(sb, (lhs0, lhs1), (rhs0, rhs1))
        s(sb.toString)
        
      case (Pat.Tuple(elements0), Pat.Tuple(elements1)) =>
        printOneChild(sb, (elements0, elements1))
        s(sb.toString)
        
      case (Pat.Typed(lhs0, rhs0), Pat.Typed(lhs1, rhs1)) =>
        printTwoChildren(sb, (lhs0, lhs1), (rhs0, rhs1))
        s(sb.toString)
        
      case (Pat.Type.Project(qual0, name0), Pat.Type.Project(qual1, name1)) =>
        printTwoChildren(sb, (qual0, qual1), (name0, name1))
        s(sb.toString)
        
      case (Pat.Type.Apply(tpe0, args0), Pat.Type.Apply(tpe1, args1)) =>
        printTwoChildren(sb, (tpe0, tpe1), (args0, args1))
        s(sb.toString)
        
      case (Pat.Type.ApplyInfix(lhs0, op0, rhs0), Pat.Type.ApplyInfix(lhs1, op1, rhs1)) =>
        printThreeChildren(sb, TreeTree(lhs0, lhs1), TreeTree(op0, op1), TreeTree(rhs0, rhs1))
        s(sb.toString)
        
      case (Pat.Type.Function(params0, res0), Pat.Type.Function(params1, res1)) =>
        printTwoChildren(sb, (params0, params1), (res0, res1))        
        s(sb.toString)
        
      case (Pat.Type.Tuple(elements0), Pat.Type.Tuple(elements1)) =>
        printOneChild(sb, (elements0, elements1))
        s(sb.toString)
        
      case (Pat.Type.Existential(tpe0, quants0), Pat.Type.Existential(tpe1, quants1)) =>
        printTwoChildren(sb, (tpe0, tpe1), (quants0, quants1))
        s(sb.toString)
        
      case (Pat.Type.Annotate(tpe0, annots0), Pat.Type.Annotate(tpe1, annots1)) =>
        printTwoChildren(sb, (tpe0, tpe1), (annots0, annots1))
        s(sb.toString)
      /*
       TODO: more Pat cases to go here
       */
      case (Lit(a0), Lit(a1)) =>
        if (a0 != a1) sb.append(a1)
        else sb.append(a0)

        s(sb.toString)

      /* Ctor cases */
      case (Ctor.Ref.Name(v0), Ctor.Ref.Name(v1)) =>
        if (v0 ne v1) sb.append(v1)
        else sb.append(v0)

        s(sb.toString)
        
      case (Ctor.Ref.Select(qual0, name0), Ctor.Ref.Select(qual1, name1)) =>
        printTwoChildren(sb, (qual0, qual1), (name0, name1))
        s(sb.toString)

      case (Ctor.Ref.Project(qual0, name0), Ctor.Ref.Project(qual1, name1)) =>
        printTwoChildren(sb, (qual0, qual1), (name0, name1))
        s(sb.toString)

      case (Ctor.Ref.Function(name0), Ctor.Ref.Function(name1)) =>
        printOneChild(sb, (name0, name1))
        s(sb.toString)

      /* Mod cases */
      case (Mod.Annot(body0), Mod.Annot(body1)) =>
        printOneChild(sb, (body0, body1))
        s(sb.toString)

      /* Enumerator cases */
      case (Enumerator.Generator(pat0, rhs0), Enumerator.Generator(pat1, rhs1)) =>
        printTwoChildren(sb, (pat0, pat1), (rhs0, rhs1))
        s(sb.toString)
        
      case (Enumerator.Val(pat0, rhs0), Enumerator.Val(pat1, rhs1)) =>
        printTwoChildren(sb, (pat0, pat1), (rhs0, rhs1))
        s(sb.toString)
        
      case (Enumerator.Guard(cond0), Enumerator.Guard(cond1)) =>
        printOneChild(sb, (cond0, cond1))
        s(sb.toString)

      /* Importee cases */
      case (Importee.Name(v0), Importee.Name(v1)) =>
        printOneChild(sb, (v0, v1))
        s(sb.toString)

      case (Importee.Unimport(name0), Importee.Unimport(name1)) =>
        printOneChild(sb, (name0, name1))
        s(sb.toString)

      /* Source case */
      case (Source(stats0), Source(stats1)) =>
        printOneChild(sb, (stats0, stats1))
        s(sb.toString)

      /* TYPE CASES */
      case (Type.Name(v0), Type.Name(v1)) =>
        if (v0 ne v1) sb.append(v1)
        else sb.append(v0)
        s(sb.toString)
        
      case (Type.Select(qual0, name0), Type.Select(qual1, name1)) =>
        printTwoChildren(sb, (qual0, qual1), (name0, name1))
        s(sb.toString)
        
      case (Type.Project(qual0, name0), Type.Project(qual1, name1)) =>
        printTwoChildren(sb, (qual0, qual1), (name0, name1))
        s(sb.toString)

      case (Type.Singleton(ref0), Type.Singleton(ref1)) =>
        printOneChild(sb, (ref0, ref1))
        s(sb.toString)

      case (Type.Apply(tpe0, args0), Type.Apply(tpe1, args1)) =>
        printTwoChildren(sb, (tpe0, tpe1), (args0, args1))
        s(sb.toString)

      case (Type.Function(params0, res0), Type.Function(params1, res1)) =>
        printTwoChildren(sb, (params0, params1), (res0, res1))
        s(sb.toString)
        
      case (Type.Existential(tpe0, quants0), Type.Existential(tpe1, quants1)) =>
        printTwoChildren(sb, (tpe0, tpe1), (quants0, quants1))
        s(sb.toString)
        
      case (Type.Annotate(tpe0, annots0), Type.Annotate(tpe1, annots1)) =>
        printTwoChildren(sb, (tpe0, tpe1), (annots0, annots1))
        s(sb.toString)
        
      case (Type.ApplyInfix(lhs0, op0, rhs0), Type.ApplyInfix(lhs1, op1, rhs1)) =>
        printThreeChildren(sb, TreeTree(lhs0, lhs1), TreeTree(op0, op1), TreeTree(rhs0, rhs1))
        s(sb.toString)
        
      case (Type.Tuple(elements0), Type.Tuple(elements1)) =>
        printOneChild(sb, (elements0, elements1))
        s(sb.toString)
        
      case (Type.Arg.ByName(tpe0), Type.Arg.ByName(tpe1)) =>
        printOneChild(sb, (tpe0, tpe1))
        s(sb.toString)
        
      case (Type.Arg.Repeated(tpe0), Type.Arg.Repeated(tpe1)) =>
        printOneChild(sb, (tpe0, tpe1))
        s(sb.toString)

      case _ => s("$hole")
    }
  }
}
