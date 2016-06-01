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
    (orig, transformed) match {
      case (Term.If(cond0, thenp0, elsep0), Term.If(cond1, thenp1, elsep1)) =>
        val sb = new StringBuilder
        if (cond0 ne cond1) {
          sb.append(
            new String(origInputChars, origPosStart, cond0.pos.start.offset - origPosStart) +
              (cond1)
          )
        }
        else sb.append(new String(origInputChars, origPosStart, cond0.pos.end.offset - origPosStart))

        if (thenp0 ne thenp1) {
          sb.append(
            new String(origInputChars, cond0.pos.end.offset, thenp0.pos.start.offset - cond0.pos.end.offset) +
              (thenp1)
          )
        }
        else sb.append(new String(origInputChars, cond0.pos.end.offset, thenp0.pos.end.offset - cond0.pos.end.offset))

        if (elsep0 ne elsep1) {
          sb.append(
            new String(origInputChars, thenp0.pos.end.offset, elsep0.pos.start.offset - thenp0.pos.end.offset) +
              (elsep1)
          )
        }
        else sb.append(new String(origInputChars, thenp0.pos.end.offset, orig.pos.end.offset - thenp0.pos.end.offset))
        s(sb.toString)
      
      case (Term.While(expr0, body0), Term.While(expr1, body1)) =>
        val sb = new StringBuilder
        if (expr0 ne expr1) {
          sb.append(
            new String(origInputChars, origPosStart, expr0.pos.start.offset - origPosStart) +
              (expr1)
          )
        }
        else sb.append(new String(origInputChars, origPosStart, expr0.pos.end.offset - origPosStart))

        if (body0 ne body1) {
          sb.append(
            new String(origInputChars, expr0.pos.end.offset, body0.pos.start.offset - expr0.pos.end.offset) +
              (body1)
          )
        }
        else sb.append(new String(origInputChars, expr0.pos.end.offset, body0.pos.end.offset - expr0.pos.end.offset))

        s(sb.toString)
      case (Term.Do(body0, expr0), Term.Do(body1, expr1)) =>
        val sb = new StringBuilder
        if (body0 ne body1) {
          sb.append(
            new String(origInputChars, origPosStart, body0.pos.start.offset - origPosStart) +
              (body1)
          )
        }
        else sb.append(new String(origInputChars, origPosStart, body0.pos.end.offset - origPosStart))

        if (expr0 ne expr1) {
          sb.append(
            new String(origInputChars, body0.pos.end.offset, expr0.pos.start.offset - body0.pos.end.offset) +
              (expr1) +
              new String(origInputChars, expr0.pos.end.offset, orig.pos.end.offset - expr0.pos.end.offset)
          )
        }
        else sb.append(new String(origInputChars, body0.pos.end.offset, orig.pos.end.offset - body0.pos.end.offset))

        s(sb.toString)

      // other cases involving simple terms is straightforward.
      // naive work around with Seq[Term]
      case (Term.Tuple(es0), Term.Tuple(es1)) =>
        val sb = new StringBuilder
        if (es0.length == 0) s(orig.toString)
        else {
          
          if (es0.head ne es1.head) sb.append(
            new String(origInputChars, origPosStart, es0.head.pos.start.offset - origPosStart) +
              (es1.head)
          )
          else sb.append(new String(origInputChars, origPosStart, es0.head.pos.end.offset - origPosStart))
          
          for (List(es00, es01) <- (es0 zip es1).grouped(2)) {
            if (es01._1 ne es01._2) sb.append(new String(origInputChars, es00._1.pos.end.offset, es01._1.pos.start.offset - es00._1.pos.end.offset) + (es01._2))
            else sb.append("")
          }

          if (es0.length > 2) {
            if (es0.last ne es1.last) sb.append(
              new String(origInputChars, es0(es0.length - 2).pos.end.offset, es0.last.pos.start.offset - es0(es0.length - 2).pos.end.offset) +
                (es1.last) +
                new String(origInputChars, es0.last.pos.end.offset, orig.pos.end.offset - es0.last.pos.end.offset)
            )
            else sb.append("")
          }
          else sb.append(new String(origInputChars, es0.last.pos.end.offset, orig.pos.end.offset - es0.last.pos.end.offset))

          s(sb.toString)
        }
      case (Term.Eta(t0), Term.Eta(t1)) =>
        val sb = new StringBuilder
        if (t0 ne t1) {
          sb.append(
            new String(origInputChars, origPosStart, t0.pos.start.offset - origPosStart) +
              (t1) +
              new String(origInputChars, t0.pos.end.offset, orig.pos.end.offset - t0.pos.end.offset)
          )
        }
        else sb.append(orig.toString)

        s(sb.toString)
      case (Term.Throw(t0), Term.Throw(t1)) =>
        val sb = new StringBuilder
        if (t0 ne t1) {
          sb.append(
            new String(origInputChars, origPosStart, t0.pos.start.offset - origPosStart) +
              (t1) +
              new String(origInputChars, t0.pos.end.offset, orig.pos.end.offset - t0.pos.end.offset)
          )
        }
        else sb.append(orig.toString)

        s(sb.toString)
      case (Term.Return(t0), Term.Return(t1)) =>
        val sb = new StringBuilder
        if (t0 ne t1) {
          sb.append(
            new String(origInputChars, origPosStart, t0.pos.start.offset - origPosStart) +
              (t1) +
              new String(origInputChars, t0.pos.end.offset, orig.pos.end.offset - t0.pos.end.offset)
          )
        }
        else sb.append(orig.toString)

        s(sb.toString)
      case (Term.Assign(lhs0, rhs0), Term.Assign(lhs1, rhs1)) =>
        val sb = new StringBuilder
        if (lhs0 ne lhs1) {
          sb.append(
            new String(origInputChars, origPosStart, lhs0.pos.start.offset - origPosStart) +
              (lhs1)
          )
        }
        else sb.append(new String(origInputChars, origPosStart, lhs0.pos.end.offset - origPosStart))

        if (rhs0 ne rhs1) {
          sb.append(
            new String(origInputChars, lhs0.pos.end.offset, rhs0.pos.start.offset - lhs0.pos.end.offset) +
              (rhs1) +
              new String(origInputChars, rhs0.pos.end.offset, orig.pos.end.offset - rhs0.pos.end.offset)
          )
        }
        else sb.append(new String(origInputChars, lhs0.pos.end.offset, orig.pos.end.offset - lhs0.pos.end.offset))

        s(sb.toString)
      case (Term.Ascribe(e0, tpe0), Term.Ascribe(e1, tpe1)) =>
        val sb = new StringBuilder
        if (e0 ne e1) {
          sb.append(
            new String(origInputChars, origPosStart, e0.pos.start.offset - origPosStart) +
              (e1)
          )
        }
        else sb.append(new String(origInputChars, origPosStart, e0.pos.end.offset - origPosStart))

        if (tpe0 ne tpe1) {
          sb.append(
            new String(origInputChars, e0.pos.end.offset, tpe0.pos.start.offset - e0.pos.end.offset) +
              (tpe1) +
              new String(origInputChars, tpe0.pos.end.offset, orig.pos.end.offset - tpe0.pos.end.offset)
          )
        }
        else sb.append(new String(origInputChars, e0.pos.end.offset, orig.pos.end.offset - e0.pos.end.offset))

        s(sb.toString)
      case (Term.Select(qual0, name0), Term.Select(qual1, name1)) =>
        val sb = new StringBuilder
        if (qual0 ne qual1) {
          sb.append(
            new String(origInputChars, origPosStart, qual0.pos.start.offset - origPosStart) +
              (qual1)
          )
        }
        else sb.append(new String(origInputChars, origPosStart, qual0.pos.end.offset - origPosStart))

        if (name0 ne name1) {
          sb.append(
            new String(origInputChars, qual0.pos.end.offset, name0.pos.start.offset - qual0.pos.end.offset) +
              (name1) +
              new String(origInputChars, name0.pos.end.offset, orig.pos.end.offset - name0.pos.end.offset)
          )
        }
        else sb.append(new String(origInputChars, qual0.pos.end.offset, orig.pos.end.offset - qual0.pos.end.offset))

        s(sb.toString)
      case (Term.ApplyUnary(op0, arg0), Term.ApplyUnary(op1, arg1)) =>
        val sb = new StringBuilder
        if (op0 ne op1) {
          sb.append(
            new String(origInputChars, origPosStart, op0.pos.start.offset - origPosStart) +
              (op1)
          )
        }
        else sb.append(new String(origInputChars, origPosStart, op0.pos.end.offset - origPosStart))

        if (arg0 ne arg1) {
          sb.append(
            new String(origInputChars, op0.pos.end.offset, arg0.pos.start.offset - op0.pos.end.offset) +
              (arg1) +
              new String(origInputChars, arg0.pos.end.offset, orig.pos.end.offset - arg0.pos.end.offset)
          )
        }
        else sb.append(new String(origInputChars, op0.pos.end.offset, orig.pos.end.offset - op0.pos.end.offset))

        s(sb.toString)
      case (Term.Arg.Named(name0, rhs0), Term.Arg.Named(name1, rhs1)) =>
        val sb = new StringBuilder
        if (name0 ne name1) {
          sb.append(
            new String(origInputChars, origPosStart, name0.pos.start.offset - origPosStart) +
              (name1)
          )
        }
        else sb.append(new String(origInputChars, origPosStart, name0.pos.end.offset - origPosStart))

        if (rhs0 ne rhs1) {
          sb.append(
            new String(origInputChars, name0.pos.end.offset, rhs0.pos.start.offset - name0.pos.end.offset) +
              (rhs1) +
              new String(origInputChars, rhs0.pos.end.offset, orig.pos.end.offset - rhs0.pos.end.offset)
          )
        }
        else sb.append(new String(origInputChars, name0.pos.end.offset, orig.pos.end.offset - name0.pos.end.offset))

        s(sb.toString)
      case (Term.Arg.Repeated(t0), Term.Arg.Repeated(t1)) =>
        val sb = new StringBuilder
        if (t0 ne t1) {
          sb.append(
            new String(origInputChars, origPosStart, t0.pos.start.offset - origPosStart) +
              (t1) +
              new String(origInputChars, t0.pos.end.offset, orig.pos.end.offset - t0.pos.end.offset)
          )
        }
        else sb.append(orig.toString)

        s(sb.toString)
      case (Term.Match(scrut0, cases0), Term.Match(scrut1, cases1)) =>
        val sb = new StringBuilder
        
        if (scrut0 ne scrut1) {
          sb.append(
            new String(origInputChars, origPosStart, scrut0.pos.start.offset - origPosStart) +
              (scrut1)
          )
        }
        else sb.append("")
        

        /* handle seq cases */
        
        (cases0.head, cases1.head) match {
          case (Case(pat0, cond0, body0), Case(pat1, cond1, body1)) =>
            if (pat0 ne pat1) sb.append(
              new String(origInputChars, scrut0.pos.end.offset, pat0.pos.start.offset - scrut0.pos.end.offset) +
                (pat1)
            )
            else sb.append(new String(origInputChars, scrut0.pos.end.offset, pat0.pos.end.offset - scrut0.pos.end.offset))

            /*

             if (cond0 ne cond1) sb.append(
             new String(origInputChars, pat0.pos.end.offset, cond0.get.pos.start.offset - pat0.pos.start.offset) +
             (cond1.get)
             )
             else sb.append(new String(origInputChars, pat0.pos.end.offset, cond0.get.pos.end.offset - pat0.pos.end.offset))
             */               

            if (body0 ne body1) sb.append(
              new String(origInputChars, pat0.pos.end.offset, body0.pos.start.offset - pat0.pos.end.offset) +
                (body1)
            )
            else sb.append(new String(origInputChars, pat0.pos.end.offset, body0.pos.end.offset - pat0.pos.end.offset))
        }
        

        for (List(cases00, cases01) <- (cases0 zip cases1).grouped(2)) {
          (cases01._1, cases01._2) match {
            case (Case(pat0, cond0, body0), Case(pat1, cond1, body1)) =>
              if (pat0 ne pat1) sb.append(
                new String(origInputChars, cases00._1.body.pos.end.offset, pat0.pos.start.offset - cases00._1.body.pos.end.offset) +
                  (pat1)
              )
              else sb.append("blahblahblah") //new String(origInputChars, cases00._1.pos.end.offset, cases01._1.pos.end.offset - cases00._1.pos.end.offset))

              /*

               if (cond0 ne cond1) sb.append(
               new String(origInputChars, pat0.pos.end.offset, cond0.get.pos.start.offset - pat0.pos.start.offset) +
               (cond1.get)
               )
               else sb.append(new String(origInputChars, pat0.pos.end.offset, cond0.get.pos.end.offset - pat0.pos.end.offset))
               */               

              if (body0 ne body1) sb.append(
                new String(origInputChars, pat0.pos.end.offset, body0.pos.start.offset - pat0.pos.end.offset) +
                  (body1)
                  // new String(origInputChars, body0.pos.end.offset, cases01._1.pos.end.offset - body0.pos.end.offset)
              )
              else sb.append(new String(origInputChars, pat0.pos.end.offset, body0.pos.end.offset - pat0.pos.end.offset))
          }
        }

        if (cases0.length > 2) {
          (cases0.last, cases1.last) match {
            case (Case(pat0, cond0, body0), Case(pat1, cond1, body1)) =>
              if (pat0 ne pat1) sb.append(
                new String(origInputChars, cases0(cases0.length - 2).body.pos.end.offset, pat0.pos.start.offset - cases0(cases0.length - 2).body.pos.end.offset) +
                  (pat1)
              )
              else sb.append("blahblahblah") //new String(origInputChars, cases00._1.pos.end.offset, cases01._1.pos.end.offset - cases00._1.pos.end.offset))

              /*

               if (cond0 ne cond1) sb.append(
               new String(origInputChars, pat0.pos.end.offset, cond0.get.pos.start.offset - pat0.pos.start.offset) +
               (cond1.get)
               )
               else sb.append(new String(origInputChars, pat0.pos.end.offset, cond0.get.pos.end.offset - pat0.pos.end.offset))
               */               

              if (body0 ne body1) sb.append(
                new String(origInputChars, pat0.pos.end.offset, body0.pos.start.offset - pat0.pos.end.offset) +
                  (body1) +
                  new String(origInputChars, body0.pos.end.offset, orig.pos.end.offset - body0.pos.end.offset)
              )
              else sb.append(new String(origInputChars, pat0.pos.end.offset, body0.pos.end.offset - pat0.pos.end.offset))
          }
        }
        else sb.append(new String(origInputChars, cases0.last.body.pos.end.offset, orig.pos.end.offset - cases0.last.body.pos.end.offset))

        s(sb.toString)
      case (Term.Block(stats0), Term.Block(stats1)) =>
        val sb = new StringBuilder
        if (stats0.head ne stats1.head) sb.append(
          new String(origInputChars, origPosStart, stats0.head.pos.start.offset - origPosStart) +
            (stats1.head).toString
        )
        else sb.append(new String(origInputChars, origPosStart, stats0.head.pos.end.offset - origPosStart))

        for (List(stats00, stats01) <- (stats0 zip stats1).grouped(2)) {
          if (stats01._1 ne stats01._2) sb.append(new String(origInputChars, stats00._1.pos.end.offset, stats01._1.pos.start.offset - stats00._1.pos.end.offset) + (stats01._2).toString)
          else sb.append("")
        }

        if (stats0.length > 2) {
          if (stats0.last ne stats1.last) sb.append(
            new String(origInputChars, stats0(stats0.length - 2).pos.end.offset, stats0.last.pos.start.offset - stats0(stats0.length - 2).pos.end.offset) +
              (stats1.last).toString +
              new String(origInputChars, stats0.last.pos.end.offset, orig.pos.end.offset - stats0.last.pos.end.offset)
          )
          else sb.append("")
        }
        else sb.append(new String(origInputChars, stats0.last.pos.end.offset, orig.pos.end.offset - stats0.last.pos.end.offset))

        s(sb.toString)
      case (Term.Name(v0), Term.Name(v1)) =>
        val sb = new StringBuilder

        if (v0 ne v1) sb.append(v1)
        else sb.append(v0)

        s(sb.toString)
      case (Term.Select(qual0, name0), Term.Select(qual1, name1)) =>
        val sb = new StringBuilder
        if (qual0 ne qual1) {
          sb.append(
            new String(origInputChars, origPosStart, qual0.pos.start.offset - origPosStart) +
              (qual1).toString
          )
        }
        else sb.append(new String(origInputChars, origPosStart, qual0.pos.end.offset - origPosStart))

        if (name0 ne name1) {
          sb.append(
            new String(origInputChars, qual0.pos.end.offset, name0.pos.start.offset - qual0.pos.end.offset) +
              (name1).toString +
              new String(origInputChars, name0.pos.end.offset, orig.pos.end.offset - name0.pos.end.offset)
          )
        }
        else sb.append(new String(origInputChars, qual0.pos.end.offset, orig.pos.end.offset - qual0.pos.end.offset))

        s(sb.toString)
      case (Term.Apply(fun0, args0), Term.Apply(fun1, args1)) =>
        val sb = new StringBuilder
        if (fun0 ne fun1) {
          sb.append(
            new String(origInputChars, origPosStart, fun0.pos.start.offset - origPosStart) +
              (fun1).toString
          )
        }
        else sb.append(new String(origInputChars, origPosStart, fun0.pos.end.offset - origPosStart))

        /* handle Seq[Arg] */
        if (args0.head ne args1.head) sb.append(
          new String(origInputChars, origPosStart, args0.head.pos.start.offset - origPosStart) +
            (args1.head).toString
        )
        else sb.append(new String(origInputChars, origPosStart, args0.head.pos.end.offset - origPosStart))

        for (List(args00, args01) <- (args0 zip args1).grouped(2)) {
          if (args01._1 ne args01._2) sb.append(new String(origInputChars, args00._1.pos.end.offset, args01._1.pos.start.offset - args00._1.pos.end.offset) + (args01._2).toString)
          else sb.append("")
        }

        if (args0.length > 2) {
          if (args0.last ne args1.last) sb.append(
            new String(origInputChars, args0(args0.length - 2).pos.end.offset, args0.last.pos.start.offset - args0(args0.length - 2).pos.end.offset) +
              (args1.last).toString +
              new String(origInputChars, args0.last.pos.end.offset, orig.pos.end.offset - args0.last.pos.end.offset)
          )
          else sb.append("")
        }
        else sb.append(new String(origInputChars, args0.last.pos.end.offset, orig.pos.end.offset - args0.last.pos.end.offset))

        s(sb.toString)
      case (Term.Annotate(expr0, annots0), Term.Annotate(expr1, annots1)) =>
        val sb = new StringBuilder
        if (expr0 ne expr1) {
          sb.append(
            new String(origInputChars, origPosStart, expr0.pos.start.offset - origPosStart) +
              (expr1).toString
          )
        }
        else sb.append(new String(origInputChars, origPosStart, expr0.pos.end.offset - origPosStart))

        /* handle Seq[Arg] */
        if (annots0.head ne annots1.head) sb.append(
          new String(origInputChars, origPosStart, annots0.head.pos.start.offset - origPosStart) +
            (annots1.head).toString
        )
        else sb.append(new String(origInputChars, origPosStart, annots0.head.pos.end.offset - origPosStart))

        for (List(annots00, annots01) <- (annots0 zip annots1).grouped(2)) {
          if (annots01._1 ne annots01._2) sb.append(new String(origInputChars, annots00._1.pos.end.offset, annots01._1.pos.start.offset - annots00._1.pos.end.offset) + (annots01._2).toString)
          else sb.append("")
        }

        if (annots0.length > 2) {
          if (annots0.last ne annots1.last) sb.append(
            new String(origInputChars, annots0(annots0.length - 2).pos.end.offset, annots0.last.pos.start.offset - annots0(annots0.length - 2).pos.end.offset) +
              (annots1.last).toString +
              new String(origInputChars, annots0.last.pos.end.offset, orig.pos.end.offset - annots0.last.pos.end.offset)
          )
          else sb.append("")
        }
        else sb.append(new String(origInputChars, annots0.last.pos.end.offset, orig.pos.end.offset - annots0.last.pos.end.offset))

        s(sb.toString)
      case (Term.PartialFunction(cases0), Term.PartialFunction(cases1)) =>
        val sb = new StringBuilder
        if (cases0.head ne cases1.head) sb.append(
          new String(origInputChars, origPosStart, cases0.head.pos.start.offset - origPosStart) +
            (cases1.head)
        )
        else sb.append(new String(origInputChars, origPosStart, cases0.head.pos.end.offset - origPosStart))
        
        for (List(cases00, cases01) <- (cases0 zip cases1).grouped(2)) {
          if (cases01._1 ne cases01._2) sb.append(new String(origInputChars, cases00._1.pos.end.offset, cases01._1.pos.start.offset - cases00._1.pos.end.offset) + (cases01._2))
          else sb.append("")
        }

        if (cases0.length > 2) {
          if (cases0.last ne cases1.last) sb.append(
            new String(origInputChars, cases0(cases0.length - 2).pos.end.offset, cases0.last.pos.start.offset - cases0(cases0.length - 2).pos.end.offset) +
              (cases1.last) +
              new String(origInputChars, cases0.last.pos.end.offset, orig.pos.end.offset - cases0.last.pos.end.offset)
          )
          else sb.append("")
        }
        else sb.append(new String(origInputChars, cases0.last.pos.end.offset, orig.pos.end.offset - cases0.last.pos.end.offset))

        s(sb.toString)

      /* cover Pat cases here */
      case (Pat.Var.Term(name0), Pat.Var.Term(name1)) =>
        val sb = new StringBuilder
        if (name0 ne name1) {
          sb.append(
            new String(origInputChars, origPosStart, name0.pos.start.offset - origPosStart) +
              (name1) +
              new String(origInputChars, name0.pos.end.offset, orig.pos.end.offset - name0.pos.end.offset)
          )
        }
        else sb.append(orig.toString)

        s(sb.toString)
      case (Pat.Var.Type(name0), Pat.Var.Type(name1)) =>
        val sb = new StringBuilder
        if (name0 ne name1) {
          sb.append(
            new String(origInputChars, origPosStart, name0.pos.start.offset - origPosStart) +
              (name1) +
              new String(origInputChars, name0.pos.end.offset, orig.pos.end.offset - name0.pos.end.offset)
          )
        }
        else sb.append(orig.toString)

        s(sb.toString)
      case (Pat.Bind(lhs0, rhs0), Pat.Bind(lhs1, rhs1)) =>
        val sb = new StringBuilder
        if (lhs0 ne lhs1) {
          sb.append(
            new String(origInputChars, origPosStart, lhs0.pos.start.offset - origPosStart) +
              (lhs1)
          )
        }
        else sb.append(new String(origInputChars, origPosStart, lhs0.pos.end.offset - origPosStart))

        if (rhs0 ne rhs1) {
          sb.append(
            new String(origInputChars, lhs0.pos.end.offset, rhs0.pos.start.offset - lhs0.pos.end.offset) +
              (rhs1) +
              new String(origInputChars, rhs0.pos.end.offset, orig.pos.end.offset - rhs0.pos.end.offset)
          )
        }
        else sb.append(new String(origInputChars, lhs0.pos.end.offset, orig.pos.end.offset - lhs0.pos.end.offset))

        s(sb.toString)
      case (Pat.Alternative(lhs0, rhs0), Pat.Alternative(lhs1, rhs1)) =>
        val sb = new StringBuilder
        if (lhs0 ne lhs1) {
          sb.append(
            new String(origInputChars, origPosStart, lhs0.pos.start.offset - origPosStart) +
              (lhs1)
          )
        }
        else sb.append(new String(origInputChars, origPosStart, lhs0.pos.end.offset - origPosStart))

        if (rhs0 ne rhs1) {
          sb.append(
            new String(origInputChars, lhs0.pos.end.offset, rhs0.pos.start.offset - lhs0.pos.end.offset) +
              (rhs1) +
              new String(origInputChars, rhs0.pos.end.offset, orig.pos.end.offset - rhs0.pos.end.offset)
          )
        }
        else sb.append(new String(origInputChars, lhs0.pos.end.offset, orig.pos.end.offset - lhs0.pos.end.offset))

        s(sb.toString)
      case (Pat.Tuple(elements0), Pat.Tuple(elements1)) =>
        val sb = new StringBuilder
        if (elements0.head ne elements1.head) sb.append(
          new String(origInputChars, origPosStart, elements0.head.pos.start.offset - origPosStart) +
            (elements1.head)
        )
        else sb.append(new String(origInputChars, origPosStart, elements0.head.pos.end.offset - origPosStart))
        
        for (List(elements00, elements01) <- (elements0 zip elements1).grouped(2)) {
          if (elements01._1 ne elements01._2) sb.append(new String(origInputChars, elements00._1.pos.end.offset, elements01._1.pos.start.offset - elements00._1.pos.end.offset) + (elements01._2))
          else sb.append("")
        }

        if (elements0.length > 2) {
          if (elements0.last ne elements1.last) sb.append(
            new String(origInputChars, elements0(elements0.length - 2).pos.end.offset, elements0.last.pos.start.offset - elements0(elements0.length - 2).pos.end.offset) +
              (elements1.last) +
              new String(origInputChars, elements0.last.pos.end.offset, orig.pos.end.offset - elements0.last.pos.end.offset)
          )
          else sb.append("")
        }
        else sb.append(new String(origInputChars, elements0.last.pos.end.offset, orig.pos.end.offset - elements0.last.pos.end.offset))

        s(sb.toString)
      case (Pat.Typed(lhs0, rhs0), Pat.Typed(lhs1, rhs1)) =>
        val sb = new StringBuilder
        if (lhs0 ne lhs1) {
          sb.append(
            new String(origInputChars, origPosStart, lhs0.pos.start.offset - origPosStart) +
              (lhs1)
          )
        }
        else sb.append(new String(origInputChars, origPosStart, lhs0.pos.end.offset - origPosStart))

        if (rhs0 ne rhs1) {
          sb.append(
            new String(origInputChars, lhs0.pos.end.offset, rhs0.pos.start.offset - lhs0.pos.end.offset) +
              (rhs1) +
              new String(origInputChars, rhs0.pos.end.offset, orig.pos.end.offset - rhs0.pos.end.offset)
          )
        }
        else sb.append(new String(origInputChars, lhs0.pos.end.offset, orig.pos.end.offset - lhs0.pos.end.offset))

        s(sb.toString)
      /*
       more Pat cases to go here


       */
      case (Lit(a0), Lit(a1)) =>
        val sb = new StringBuilder

        if (a0 != a1) sb.append(a1)
        else sb.append(a0)

        s(sb.toString)

      /* Ctor cases */
      case (Ctor.Ref.Name(v0), Ctor.Ref.Name(v1)) =>
        val sb = new StringBuilder

        if (v0 ne v1) sb.append(v1)
        else sb.append(v0)

        s(sb.toString)
      case (Ctor.Ref.Select(qual0, name0), Ctor.Ref.Select(qual1, name1)) =>
        val sb = new StringBuilder
        if (qual0 ne qual1) {
          sb.append(
            new String(origInputChars, origPosStart, qual0.pos.start.offset - origPosStart) +
              (qual1)
          )
        }
        else sb.append(new String(origInputChars, origPosStart, qual0.pos.end.offset - origPosStart))

        if (name0 ne name1) {
          sb.append(
            new String(origInputChars, qual0.pos.end.offset, name0.pos.start.offset - qual0.pos.end.offset) +
              (name1) +
              new String(origInputChars, name0.pos.end.offset, orig.pos.end.offset - name0.pos.end.offset)
          )
        }
        else sb.append(new String(origInputChars, qual0.pos.end.offset, orig.pos.end.offset - qual0.pos.end.offset))

        s(sb.toString)
      case (Ctor.Ref.Project(qual0, name0), Ctor.Ref.Project(qual1, name1)) =>
        val sb = new StringBuilder
        if (qual0 ne qual1) {
          sb.append(
            new String(origInputChars, origPosStart, qual0.pos.start.offset - origPosStart) +
              (qual1)
          )
        }
        else sb.append(new String(origInputChars, origPosStart, qual0.pos.end.offset - origPosStart))

        if (name0 ne name1) {
          sb.append(
            new String(origInputChars, qual0.pos.end.offset, name0.pos.start.offset - qual0.pos.end.offset) +
              (name1) +
              new String(origInputChars, name0.pos.end.offset, orig.pos.end.offset - name0.pos.end.offset)
          )
        }
        else sb.append(new String(origInputChars, qual0.pos.end.offset, orig.pos.end.offset - qual0.pos.end.offset))

        s(sb.toString)
      case (Ctor.Ref.Function(name0), Ctor.Ref.Function(name1)) =>
        val sb = new StringBuilder
        if (name0 ne name1) {
          sb.append(
            new String(origInputChars, origPosStart, name0.pos.start.offset - origPosStart) +
              (name1) +
              new String(origInputChars, name0.pos.end.offset, orig.pos.end.offset - name0.pos.end.offset)
          )
        }
        else sb.append(orig.toString)

        s(sb.toString)

      /* Mod cases */
      case (Mod.Annot(body0), Mod.Annot(body1)) =>
        val sb = new StringBuilder
        if (body0 ne body1) {
          sb.append(
            new String(origInputChars, origPosStart, body0.pos.start.offset - origPosStart) +
              (body1) +
              new String(origInputChars, body0.pos.end.offset, orig.pos.end.offset - body0.pos.end.offset)
          )
        }
        else sb.append(orig.toString)

        s(sb.toString)

      /* Enumerator cases */
      case (Enumerator.Generator(pat0, rhs0), Enumerator.Generator(pat1, rhs1)) =>
        val sb = new StringBuilder
        if (pat0 ne pat1) {
          sb.append(
            new String(origInputChars, origPosStart, pat0.pos.start.offset - origPosStart) +
              (pat1)
          )
        }
        else sb.append(new String(origInputChars, origPosStart, pat0.pos.end.offset - origPosStart))

        if (rhs0 ne rhs1) {
          sb.append(
            new String(origInputChars, pat0.pos.end.offset, rhs0.pos.start.offset - pat0.pos.end.offset) +
              (rhs1) +
              new String(origInputChars, rhs0.pos.end.offset, orig.pos.end.offset - rhs0.pos.end.offset)
          )
        }
        else sb.append(new String(origInputChars, pat0.pos.end.offset, orig.pos.end.offset - pat0.pos.end.offset))

        s(sb.toString)
      case (Enumerator.Val(pat0, rhs0), Enumerator.Val(pat1, rhs1)) =>
        val sb = new StringBuilder
        if (pat0 ne pat1) {
          sb.append(
            new String(origInputChars, origPosStart, pat0.pos.start.offset - origPosStart) +
              (pat1)
          )
        }
        else sb.append(new String(origInputChars, origPosStart, pat0.pos.end.offset - origPosStart))

        if (rhs0 ne rhs1) {
          sb.append(
            new String(origInputChars, pat0.pos.end.offset, rhs0.pos.start.offset - pat0.pos.end.offset) +
              (rhs1) +
              new String(origInputChars, rhs0.pos.end.offset, orig.pos.end.offset - rhs0.pos.end.offset)
          )
        }
        else sb.append(new String(origInputChars, pat0.pos.end.offset, orig.pos.end.offset - pat0.pos.end.offset))

        s(sb.toString)
      case (Enumerator.Guard(cond0), Enumerator.Guard(cond1)) =>
        val sb = new StringBuilder
        if (cond0 ne cond1) {
          sb.append(
            new String(origInputChars, origPosStart, cond0.pos.start.offset - origPosStart) +
              (cond1) +
              new String(origInputChars, cond0.pos.end.offset, orig.pos.end.offset - cond0.pos.end.offset)
          )
        }
        else sb.append(orig.toString)

        s(sb.toString)

      /* Importee cases */
      case (Importee.Name(v0), Importee.Name(v1)) =>
        val sb = new StringBuilder
        if (v0 ne v1) {
          sb.append(
            new String(origInputChars, origPosStart, v0.pos.start.offset - origPosStart) +
              (v1) +
              new String(origInputChars, v0.pos.end.offset, orig.pos.end.offset - v0.pos.end.offset)
          )
        }
        else sb.append(orig.toString)

        s(sb.toString)

      /*
      case (Importee.Rename(from0), Importee.Rename(from1)) =>
        val sb = new StringBuilder
        if (from0 ne from1) {
          sb.append(
            new String(origInputChars, origPosStart, from0.pos.start.offset - origPosStart) +
              (from1) +
              new String(origInputChars, from0.pos.end.offset, orig.pos.end.offset - from0.pos.end.offset)
          )
        }
        else sb.append(orig.toString)

        s(sb.toString)
      
      case (Importee.Unimport(name0), Importee.Unimport(name1)) =>
        val sb = new StringBuilder
        if (name0 ne name1) {
          sb.append(
            new String(origInputChars, origPosStart, name0.pos.start.offset - origPosStart) +
              (name1) +
              new String(origInputChars, name0.pos.end.offset, orig.pos.end.offset - name0.pos.end.offset)
          )
        }
        else sb.append(orig.toString)

        s(sb.toString)
       */

      /* Source case */
      case (Source(stats0), Source(stats1)) =>
        val sb = new StringBuilder
        if (stats0.head ne stats1.head) sb.append(
          new String(origInputChars, origPosStart, stats0.head.pos.start.offset - origPosStart) +
            (stats1.head).toString
        )
        else sb.append(new String(origInputChars, origPosStart, stats0.head.pos.end.offset - origPosStart))

        for (List(stats00, stats01) <- (stats0 zip stats1).grouped(2)) {
          if (stats01._1 ne stats01._2) sb.append(new String(origInputChars, stats00._1.pos.end.offset, stats01._1.pos.start.offset - stats00._1.pos.end.offset) + (stats01._2).toString)
          else sb.append("")
        }

        if (stats0.length > 2) {
          if (stats0.last ne stats1.last) sb.append(
            new String(origInputChars, stats0(stats0.length - 2).pos.end.offset, stats0.last.pos.start.offset - stats0(stats0.length - 2).pos.end.offset) +
              (stats1.last).toString +
              new String(origInputChars, stats0.last.pos.end.offset, orig.pos.end.offset - stats0.last.pos.end.offset)
          )
          else sb.append("")
        }
        else sb.append(new String(origInputChars, stats0.last.pos.end.offset, orig.pos.end.offset - stats0.last.pos.end.offset))

        s(sb.toString)
      case _ => s("$hole")
    }
  }
}
