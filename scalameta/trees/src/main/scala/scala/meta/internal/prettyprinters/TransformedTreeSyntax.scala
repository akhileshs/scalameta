package scala.meta
package internal
package prettyprinters

import scala.meta.dialects.Quasiquote
import scala.meta.prettyprinters._
import scala.meta.prettyprinters.Syntax._
import Show.{ sequence => s}

object TransformedTreeSyntax {
  def printTransformedTree[T <: Tree](orig: T, transformed: T): Show.Result = {
    val origInputChars = orig.pos.input.chars
    val origPosStart = orig.pos.start.offset
    val sb = new StringBuilder
    def printResultTree(t1: Tree, t2: Tree, startPos: Int): String = {
      val sb = new StringBuilder
      
      if (t1 ne t2) {
        sb.append(new String(origInputChars, startPos, t1.pos.start.offset - startPos))
        sb.append(t2)
      }
      else sb.append(new String(origInputChars, startPos, t1.pos.end.offset - startPos))
      
      sb.toString
    }

    def printResultOptionTree(t1: Option[Tree], t2: Option[Tree], startPos: Int): String = {
      val sb = new StringBuilder
      (t1, t2) match {
        case (Some(t11), Some(t22)) =>
          sb.append(printResultTree(t11, t22, startPos))
          sb.toString
        case _ => sb.toString
      }      
    }

    def printResultSeqTree(t1: Seq[Tree], t2: Seq[Tree], startPos: Int): String = {
      val sb = new StringBuilder
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
     
    def printRemainder(t1: Any, startPos: Int): String = {
      val sb = new StringBuilder
      t1 match {
        case x0: Tree => sb.append(new String(origInputChars, startPos, orig.pos.end.offset - startPos))
        case x0: Seq[Tree] =>
          if (x0.length > 0) sb.append(new String(origInputChars, startPos, orig.pos.end.offset - startPos))
          else sb.append("")
        case _ => "Handle this later"
      }
      sb.toString
    }

    (orig, transformed) match {
      case (Term.If(cond0, thenp0, elsep0), Term.If(cond1, thenp1, elsep1)) =>

        sb.append(printResultTree(cond0, cond1, origPosStart))
        sb.append(printResultTree(thenp0, thenp1, cond0.pos.end.offset))
        sb.append(printResultTree(elsep0, elsep1, thenp0.pos.end.offset))
        sb.append(printRemainder(elsep0, elsep0.pos.end.offset))
        
        s(sb.toString)

      case (Term.While(expr0, body0), Term.While(expr1, body1)) =>        

        sb.append(printResultTree(expr0, expr1, origPosStart))
        sb.append(printResultTree(body0, body1, expr0.pos.end.offset))
        sb.append(printRemainder(body0, body0.pos.end.offset))

        s(sb.toString)
        
      case (Term.Do(body0, expr0), Term.Do(body1, expr1)) =>        

        sb.append(printResultTree(body0, body1, origPosStart))
        sb.append(printResultTree(expr0, expr1, body0.pos.end.offset))
        sb.append(printRemainder(expr0, expr0.pos.end.offset))

        s(sb.toString)

      case (Term.Tuple(es0), Term.Tuple(es1)) =>

        sb.append(printResultSeqTree(es0, es1, origPosStart))
        sb.append(printRemainder(es0, es0.last.pos.end.offset))

        s(sb.toString)
        
      case (Term.Eta(t0), Term.Eta(t1)) =>        

        sb.append(printResultTree(t0, t1, origPosStart))
        sb.append(printRemainder(t0, t0.pos.end.offset))

        s(sb.toString)
        
      case (Term.Throw(t0), Term.Throw(t1)) =>        

        sb.append(printResultTree(t0, t1, origPosStart))
        sb.append(printRemainder(t0, t0.pos.end.offset))

        s(sb.toString)
        
      case (Term.Return(t0), Term.Return(t1)) =>        

        sb.append(printResultTree(t0, t1, origPosStart))
        sb.append(printRemainder(t0, t0.pos.end.offset))

        s(sb.toString)
        
      case (Term.Assign(lhs0, rhs0), Term.Assign(lhs1, rhs1)) =>        

        sb.append(printResultTree(lhs0, lhs1, origPosStart))
        sb.append(printResultTree(rhs0, rhs1, lhs0.pos.end.offset))
        sb.append(printRemainder(rhs0, rhs0.pos.end.offset))

        s(sb.toString)
        
      case (Term.Ascribe(e0, tpe0), Term.Ascribe(e1, tpe1)) =>        

        sb.append(printResultTree(e0, e1, origPosStart))
        sb.append(printResultTree(tpe0, tpe1, e0.pos.end.offset))
        sb.append(printRemainder(tpe0, tpe0.pos.end.offset))

        s(sb.toString)
        
      case (Term.Select(qual0, name0), Term.Select(qual1, name1)) =>        

        sb.append(printResultTree(qual0, qual1, origPosStart))
        sb.append(printResultTree(name0, name1, qual0.pos.end.offset))
        sb.append(printRemainder(name0, name0.pos.end.offset))

        s(sb.toString)
        
      case (Term.ApplyUnary(op0, arg0), Term.ApplyUnary(op1, arg1)) =>        

        sb.append(printResultTree(op0, op1, origPosStart))
        sb.append(printResultTree(arg0, arg1, op0.pos.end.offset))
        sb.append(printRemainder(arg0, arg0.pos.end.offset))

        s(sb.toString)
        
      case (Term.Arg.Named(name0, rhs0), Term.Arg.Named(name1, rhs1)) =>        

        sb.append(printResultTree(name0, name1, origPosStart))
        sb.append(printResultTree(rhs0, rhs1, name0.pos.end.offset))
        sb.append(printRemainder(rhs0, rhs0.pos.end.offset))

        s(sb.toString)

      case (Term.Arg.Repeated(t0), Term.Arg.Repeated(t1)) =>        

        sb.append(printResultTree(t0, t1, origPosStart))
        sb.append(printRemainder(t0, t0.pos.end.offset))

        s(sb.toString)
        
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

      case (Term.Function(params0, body0), Term.Function(params1, body1)) =>        

        sb.append(printResultSeqTree(params0, params1, origPosStart))
        if (params0.length > 0) sb.append(printResultTree(body0, body1, params0.last.pos.end.offset))
        else sb.append(printResultTree(body0, body1, origPosStart))
        sb.append(printRemainder(body0, body0.pos.end.offset))

        s(sb.toString)
        
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
        
      case (Term.For(enums0, body0), Term.For(enums1, body1)) =>        

        sb.append(printResultSeqTree(enums0, enums1, origPosStart))
        sb.append(printResultTree(body0, body1, enums0.last.pos.end.offset))
        sb.append(printRemainder(body0, body0.pos.end.offset))
        
        s(sb.toString)
        
      case (Term.ForYield(enums0, body0), Term.ForYield(enums1, body1)) =>        

        sb.append(printResultSeqTree(enums0, enums1, origPosStart))
        sb.append(printResultTree(body0, body1, enums0.last.pos.end.offset))
        sb.append(printRemainder(body0, body0.pos.end.offset))
        
        s(sb.toString)

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
        
      case (Term.Block(stats0), Term.Block(stats1)) =>        

        sb.append(printResultSeqTree(stats0, stats1, origPosStart))

        if (stats0.length > 0) {
          sb.append(printRemainder(stats0, stats0.last.pos.end.offset))
          s(sb.toString)

        }
        else s(sb.toString)        

      case (Term.Name(v0), Term.Name(v1)) =>
        val sb = new StringBuilder

        if (v0 ne v1) sb.append(v1)
        else sb.append(v0)

        s(sb.toString)

      case (Term.Select(qual0, name0), Term.Select(qual1, name1)) =>       

        sb.append(printResultTree(qual0, qual1, origPosStart))
        sb.append(printResultTree(name0, name1, qual0.pos.end.offset))
        sb.append(printRemainder(name0, name0.pos.end.offset))

        s(sb.toString)
      case (Term.Apply(fun0, args0), Term.Apply(fun1, args1)) =>        

        sb.append(printResultTree(fun0, fun1, origPosStart))
        sb.append(printResultSeqTree(args0, args1, fun0.pos.end.offset))
        if (args0.length > 0) sb.append(printRemainder(args0, args0.last.pos.end.offset))
        else sb.append(printRemainder(fun0, fun0.pos.end.offset))

        s(sb.toString)
        
      case (Term.Annotate(expr0, annots0), Term.Annotate(expr1, annots1)) =>        

        sb.append(printResultTree(expr0, expr1, origPosStart))
        sb.append(printResultSeqTree(annots0, annots1, expr0.pos.end.offset))
        sb.append(printRemainder(annots0, annots0.last.pos.end.offset))

        s(sb.toString)
      case (Term.PartialFunction(cases0), Term.PartialFunction(cases1)) =>        

        sb.append(printResultSeqTree(cases0, cases1, origPosStart))
        sb.append(printRemainder(cases0, cases0.last.pos.end.offset))

        s(sb.toString)

      /* Pat cases */
      case (Pat.Var.Term(name0), Pat.Var.Term(name1)) =>        

        sb.append(printResultTree(name0, name1, origPosStart))
        sb.append(printRemainder(name0, name0.pos.end.offset))

        s(sb.toString)

      case (Pat.Var.Type(name0), Pat.Var.Type(name1)) =>        

        sb.append(printResultTree(name0, name1, origPosStart))
        sb.append(printRemainder(name0, name0.pos.end.offset))

        s(sb.toString)
        
      case (Pat.Bind(lhs0, rhs0), Pat.Bind(lhs1, rhs1)) =>        

        sb.append(printResultTree(lhs0, lhs1, origPosStart))
        sb.append(printResultTree(rhs0, rhs1, lhs0.pos.end.offset))
        sb.append(printRemainder(rhs0, rhs0.pos.end.offset))

        s(sb.toString)
        
      case (Pat.Alternative(lhs0, rhs0), Pat.Alternative(lhs1, rhs1)) =>        

        sb.append(printResultTree(lhs0, lhs1, origPosStart))
        sb.append(printResultTree(rhs0, rhs1, lhs0.pos.end.offset))
        sb.append(printRemainder(rhs0, rhs0.pos.end.offset))

        s(sb.toString)
        
      case (Pat.Tuple(elements0), Pat.Tuple(elements1)) =>        

        sb.append(printResultSeqTree(elements0, elements1, origPosStart))
        sb.append(printRemainder(elements0, elements0.last.pos.end.offset))

        s(sb.toString)
        
      case (Pat.Typed(lhs0, rhs0), Pat.Typed(lhs1, rhs1)) =>        

        sb.append(printResultTree(lhs0, lhs1, origPosStart))
        sb.append(printResultTree(rhs0, rhs1, lhs0.pos.end.offset))
        sb.append(printRemainder(rhs0, rhs0.pos.end.offset))

        s(sb.toString)
        
      case (Pat.Type.Project(qual0, name0), Pat.Type.Project(qual1, name1)) =>        

        sb.append(printResultTree(qual0, qual1, origPosStart))
        sb.append(printResultTree(name0, name1, qual0.pos.end.offset))
        sb.append(printRemainder(name0, name0.pos.end.offset))

        s(sb.toString)
        
      case (Pat.Type.Apply(tpe0, args0), Pat.Type.Apply(tpe1, args1)) =>        

        sb.append(printResultTree(tpe0, tpe1, origPosStart))
        sb.append(printResultSeqTree(args0, args1, tpe0.pos.end.offset))
        sb.append(printRemainder(args0, args0.last.pos.end.offset))

        s(sb.toString)
        
      case (Pat.Type.ApplyInfix(lhs0, op0, rhs0), Pat.Type.ApplyInfix(lhs1, op1, rhs1)) =>        

        sb.append(printResultTree(lhs0, lhs1, origPosStart))
        sb.append(printResultTree(op0, op1, lhs0.pos.end.offset))
        sb.append(printResultTree(rhs0, rhs1, op0.pos.end.offset))
        sb.append(printRemainder(rhs0, rhs0.pos.end.offset))

        s(sb.toString)
        
      case (Pat.Type.Function(params0, res0), Pat.Type.Function(params1, res1)) =>        

        sb.append(printResultSeqTree(params0, params1, origPosStart))
        if (params0.length > 0) sb.append(printResultTree(res0, res1, params0.last.pos.end.offset))
        else sb.append(printResultTree(res0, res1, origPosStart))
        sb.append(printRemainder(res0, res0.pos.end.offset))
        
        s(sb.toString)
        
      case (Pat.Type.Tuple(elements0), Pat.Type.Tuple(elements1)) =>        
        
        sb.append(printResultSeqTree(elements0, elements1, origPosStart))
        sb.append(printRemainder(elements0, elements0.last.pos.end.offset))

        s(sb.toString)
        
      case (Pat.Type.Existential(tpe0, quants0), Pat.Type.Existential(tpe1, quants1)) =>        

        sb.append(printResultTree(tpe0, tpe1, origPosStart))
        sb.append(printResultSeqTree(quants0, quants1, tpe0.pos.end.offset))
        sb.append(printRemainder(quants0, quants0.last.pos.end.offset))

        s(sb.toString)
        
      case (Pat.Type.Annotate(tpe0, annots0), Pat.Type.Annotate(tpe1, annots1)) =>        

        sb.append(printResultTree(tpe0, tpe1, origPosStart))
        sb.append(printResultSeqTree(annots0, annots1, tpe0.pos.end.offset))
        sb.append(printRemainder(annots0, annots0.last.pos.end.offset))

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

        sb.append(printResultTree(qual0, qual1, origPosStart))
        sb.append(printResultTree(name0, name1, qual0.pos.end.offset))
        sb.append(printRemainder(name0, name0.pos.end.offset))

        s(sb.toString)
      case (Ctor.Ref.Project(qual0, name0), Ctor.Ref.Project(qual1, name1)) =>        

        sb.append(printResultTree(qual0, qual1, origPosStart))
        sb.append(printResultTree(name0, name1, qual0.pos.end.offset))
        sb.append(printRemainder(name0, name0.pos.end.offset))

        s(sb.toString)
      case (Ctor.Ref.Function(name0), Ctor.Ref.Function(name1)) =>        

        sb.append(printResultTree(name0, name1, origPosStart))
        sb.append(printRemainder(name0, name0.pos.end.offset))

        s(sb.toString)

      /* Mod cases */
      case (Mod.Annot(body0), Mod.Annot(body1)) =>

        sb.append(printResultTree(body0, body1, origPosStart))
        sb.append(printRemainder(body0, body0.pos.end.offset))

        s(sb.toString)

      /* Enumerator cases */
      case (Enumerator.Generator(pat0, rhs0), Enumerator.Generator(pat1, rhs1)) =>        

        sb.append(printResultTree(pat0, pat1, origPosStart))
        sb.append(printResultTree(rhs0, rhs1, pat0.pos.end.offset))
        sb.append(printRemainder(rhs0, rhs0.pos.end.offset))

        s(sb.toString)
        
      case (Enumerator.Val(pat0, rhs0), Enumerator.Val(pat1, rhs1)) =>        

        sb.append(printResultTree(pat0, pat1, origPosStart))
        sb.append(printResultTree(rhs0, rhs1, pat0.pos.end.offset))
        sb.append(printRemainder(rhs0, rhs0.pos.end.offset))

        s(sb.toString)
        
      case (Enumerator.Guard(cond0), Enumerator.Guard(cond1)) =>        

        sb.append(printResultTree(cond0, cond1, origPosStart))
        sb.append(printRemainder(cond0, cond0.pos.end.offset))

        s(sb.toString)

      /* Importee cases */
      case (Importee.Name(v0), Importee.Name(v1)) =>        

        sb.append(printResultTree(v0, v1, origPosStart))
        sb.append(printRemainder(v0, v0.pos.end.offset))

        s(sb.toString)

        /*
         case (Importee.Rename(from0), Importee.Rename(from1)) =>
         val sb = new StringBuilder

         sb.append(printResult2Params(from0, from1, origPosStart))
         sb.append(printRemainder(from0, from0.pos.end.offset))

         s(sb.toString)
         */

        
      case (Importee.Unimport(name0), Importee.Unimport(name1)) =>        

        sb.append(printResultTree(name0, name1, origPosStart))
        sb.append(printRemainder(name0, name0.pos.end.offset))

        s(sb.toString)

      /* Source case */
      case (Source(stats0), Source(stats1)) =>        

        sb.append(printResultSeqTree(stats0, stats1, origPosStart))
        if (stats0.length > 0) {
          sb.append(printRemainder(stats0, stats0.last.pos.end.offset))
          s(sb.toString)

        }
        else s(sb.toString)

      /* TYPE CASES */
      case (Type.Name(v0), Type.Name(v1)) =>        

        if (v0 ne v1) sb.append(v1)
        else sb.append(v0)

        s(sb.toString)
        
      case (Type.Select(qual0, name0), Type.Select(qual1, name1)) =>        

        sb.append(printResultTree(qual0, qual1, origPosStart))
        sb.append(printResultTree(name0, name1, qual0.pos.end.offset))
        sb.append(printRemainder(name0, name0.pos.end.offset))

        s(sb.toString)
        
      case (Type.Project(qual0, name0), Type.Project(qual1, name1)) =>        

        sb.append(printResultTree(qual0, qual1, origPosStart))
        sb.append(printResultTree(name0, name1, qual0.pos.end.offset))
        sb.append(printRemainder(name0, name0.pos.end.offset))

        s(sb.toString)
      case (Type.Singleton(ref0), Type.Singleton(ref1)) =>        

        sb.append(printResultTree(ref0, ref1, origPosStart))
        sb.append(printRemainder(ref0, ref0.pos.end.offset))

        s(sb.toString)
      case (Type.Apply(tpe0, args0), Type.Apply(tpe1, args1)) =>        

        sb.append(printResultTree(tpe0, tpe1, origPosStart))
        sb.append(printResultSeqTree(args0, args1, tpe0.pos.end.offset))
        sb.append(printRemainder(args0, args0.last.pos.end.offset))

        s(sb.toString)
      case (Type.Function(params0, res0), Type.Function(params1, res1)) =>        

        sb.append(printResultSeqTree(params0, params1, origPosStart))
        if (params0.length > 0) sb.append(printResultTree(res0, res1, params0.last.pos.end.offset))
        else sb.append(printResultTree(res0, res1, origPosStart))
        sb.append(printRemainder(res0, res0.pos.end.offset))

        s(sb.toString)
        
      case (Type.Existential(tpe0, quants0), Type.Existential(tpe1, quants1)) =>        

        sb.append(printResultTree(tpe0, tpe1, origPosStart))
        sb.append(printResultSeqTree(quants0, quants1, tpe0.pos.end.offset))
        sb.append(printRemainder(quants0, quants0.last.pos.end.offset))

        s(sb.toString)
        
      case (Type.Annotate(tpe0, annots0), Type.Annotate(tpe1, annots1)) =>        

        sb.append(printResultTree(tpe0, tpe1, origPosStart))
        sb.append(printResultSeqTree(annots0, annots1, tpe0.pos.end.offset))
        sb.append(printRemainder(annots0, annots0.last.pos.end.offset))

        s(sb.toString)
        
      case (Type.ApplyInfix(lhs0, op0, rhs0), Type.ApplyInfix(lhs1, op1, rhs1)) =>        

        sb.append(printResultTree(lhs0, lhs1, origPosStart))
        sb.append(printResultTree(op0, op1, lhs0.pos.end.offset))
        sb.append(printResultTree(rhs0, rhs1, op0.pos.end.offset))
        sb.append(printRemainder(rhs0, rhs0.pos.end.offset))

        s(sb.toString)
        
      case (Type.Tuple(elements0), Type.Tuple(elements1)) =>        

        sb.append(printResultSeqTree(elements0, elements1, origPosStart))
        sb.append(printRemainder(elements0, elements0.last.pos.end.offset))

        s(sb.toString)
        
      case (Type.Arg.ByName(tpe0), Type.Arg.ByName(tpe1)) =>        

        sb.append(printResultTree(tpe0, tpe1, origPosStart))
        sb.append(printRemainder(tpe0, tpe0.pos.end.offset))

        s(sb.toString)
        
      case (Type.Arg.Repeated(tpe0), Type.Arg.Repeated(tpe1)) =>        

        sb.append(printResultTree(tpe0, tpe1, origPosStart))
        sb.append(printRemainder(tpe0, tpe0.pos.end.offset))

        s(sb.toString)

      /* DECL cases */        
      case (Decl.Val(mods0, pats0, decltpe0), Decl.Val(mods1, pats1, decltpe1)) =>  

        sb.append(printResultSeqTree(mods0, mods1, origPosStart))
        if (mods0.length > 0) sb.append(printResultSeqTree(pats0, pats1, mods0.last.pos.end.offset))
        else sb.append(printResultSeqTree(pats0, pats1, origPosStart))
        sb.append(printResultTree(decltpe0, decltpe1, pats0.last.pos.end.offset))
        sb.append(printRemainder(decltpe0, decltpe0.pos.end.offset))
        
        s(sb.toString)
        
      case (Decl.Var(mods0, pats0, decltpe0), Decl.Var(mods1, pats1, decltpe1)) =>        

        sb.append(printResultSeqTree(mods0, mods1, origPosStart))
        if (mods0.length > 0) sb.append(printResultSeqTree(pats0, pats1, mods0.last.pos.end.offset))
        else sb.append(printResultSeqTree(pats0, pats1, origPosStart))
        sb.append(printResultTree(decltpe0, decltpe1, pats0.last.pos.end.offset))
        sb.append(printRemainder(decltpe0, decltpe0.pos.end.offset))
        
        s(sb.toString)     
        
      case _ => s("$hole")
    }
  }
}
