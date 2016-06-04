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
      case (Term.Function(params0, body0), Term.Function(params1, body1)) =>
        val sb = new StringBuilder
        if (params0.length == 0) {
          if (body0 ne body1) {
            sb.append(
              new String(origInputChars, origPosStart, body0.pos.start.offset - origPosStart) +
                (body1) +
                new String(origInputChars, body0.pos.end.offset, orig.pos.end.offset - body0.pos.end.offset)
            )
          }
          else sb.append(orig.toString)

          s(sb.toString)
        } else {
          if (params0.head ne params1.head) sb.append(
            new String(origInputChars, origPosStart, params0.head.pos.start.offset - origPosStart) +
              (params1.head)
          )
          else sb.append(new String(origInputChars, origPosStart, params0.head.pos.end.offset - origPosStart))
          
          for (List(params00, params01) <- (params0 zip params1).grouped(2)) {
            if (params01._1 ne params01._2) sb.append(new String(origInputChars, params00._1.pos.end.offset, params01._1.pos.start.offset - params00._1.pos.end.offset) + (params01._2))
            else sb.append("")
          }

          if (params0.length > 2) {
            if (params0.last ne params1.last) sb.append(
              new String(origInputChars, params0(params0.length - 2).pos.end.offset, params0.last.pos.start.offset - params0(params0.length - 2).pos.end.offset) +
                (params1.last)
            )
            else sb.append("")
          }
          else sb.append("") //sb.append(new String(origInputChars, params0.last.pos.end.offset, orig.pos.end.offset - params0.last.pos.end.offset))

          if (body0 ne body1) {
            sb.append(
              new String(origInputChars, params0.last.pos.end.offset, body0.pos.start.offset - params0.last.pos.end.offset) +
                (body1) +
                new String(origInputChars, body0.pos.end.offset, orig.pos.end.offset - body0.pos.end.offset)
            )
          }
          else sb.append(orig.toString)
        }
        s(sb.toString)
      case (Term.ApplyInfix(lhs0, op0, targs0, args0), Term.ApplyInfix(lhs1, op1, targs1, args1)) =>
        val sb = new StringBuilder
        if (lhs0 ne lhs1) {
          sb.append(
            new String(origInputChars, origPosStart, lhs0.pos.start.offset - origPosStart) +
              (lhs1)
          )
        }
        else sb.append("") // sb.append(new String(origInputChars, origPosStart, lhs0.pos.end.offset - origPosStart))

        if (op0 ne op1) {
          sb.append(
            new String(origInputChars, lhs0.pos.end.offset, op0.pos.start.offset - lhs0.pos.end.offset) +
              (op1)
              // new String(origInputChars, op0.pos.end.offset, orig.pos.end.offset - op0.pos.end.offset)
          )
        }
        else sb.append("")//sb.append(new String(origInputChars, lhs0.pos.end.offset, orig.pos.end.offset - lhs0.pos.end.offset))

        // seq cases
        if (targs0.length == 0) sb.append("") // remove this in the end (no-op)
        else {
          
          if (targs0.head ne targs1.head) sb.append(
            new String(origInputChars, op0.pos.end.offset, targs0.head.pos.start.offset - op0.pos.end.offset) +
              (targs1.head)
          )
          else sb.append(new String(origInputChars, op0.pos.end.offset, targs0.head.pos.end.offset - op0.pos.end.offset))
          
          for (List(targs00, targs01) <- (targs0 zip targs1).grouped(2)) {
            if (targs01._1 ne targs01._2) sb.append(new String(origInputChars, targs00._1.pos.end.offset, targs01._1.pos.start.offset - targs00._1.pos.end.offset) + (targs01._2))
            else sb.append("")
          }

          if (targs0.length > 2) {
            if (targs0.last ne targs1.last) sb.append(
              new String(origInputChars, targs0(targs0.length - 2).pos.end.offset, targs0.last.pos.start.offset - targs0(targs0.length - 2).pos.end.offset) +
                (targs1.last)
                // new String(origInputChars, targs0.last.pos.end.offset, orig.pos.end.offset - targs0.last.pos.end.offset)
            )
            else sb.append("")
          }
          else sb.append(new String(origInputChars, targs0.last.pos.end.offset, orig.pos.end.offset - targs0.last.pos.end.offset))
        }

        if (args0.length == 0) sb.append("")
        else {
          
          if (args0.head ne args1.head) sb.append(
            //new String(origInputChars, targs0.last.pos.end.offset, args0.head.pos.start.offset - targs0.last.pos.end.offset) +
            new String(origInputChars, op0.pos.end.offset, args0.head.pos.start.offset - op0.pos.end.offset) + 
              (args1.head)
          )
          else sb.append(new String(origInputChars, targs0.last.pos.end.offset, args0.head.pos.end.offset - targs0.last.pos.end.offset))
          
          for (List(args00, args01) <- (args0 zip args1).grouped(2)) {
            if (args01._1 ne args01._2) sb.append(new String(origInputChars, args00._1.pos.end.offset, args01._1.pos.start.offset - args00._1.pos.end.offset) + (args01._2))
            else sb.append("")
          }

          if (args0.length > 2) {
            if (args0.last ne args1.last) sb.append(
              new String(origInputChars, args0(args0.length - 2).pos.end.offset, args0.last.pos.start.offset - args0(args0.length - 2).pos.end.offset) +
                (args1.last) +
                new String(origInputChars, args0.last.pos.end.offset, orig.pos.end.offset - args0.last.pos.end.offset)
            )
            else sb.append("")
          }
          else sb.append(new String(origInputChars, args0.last.pos.end.offset, orig.pos.end.offset - args0.last.pos.end.offset))
        }
        s(sb.toString)

      // handle Term.Param
      case (Term.For(enums0, body0), Term.For(enums1, body1)) =>
        val sb = new StringBuilder
        if (enums0.head ne enums1.head) sb.append(
          new String(origInputChars, origPosStart, enums0.head.pos.start.offset - origPosStart) +
            (enums1.head)
        )
        else sb.append(new String(origInputChars, origPosStart, enums0.head.pos.end.offset - origPosStart))
        
        for (List(enums00, enums01) <- (enums0 zip enums1).grouped(2)) {
          if (enums01._1 ne enums01._2) sb.append(new String(origInputChars, enums00._1.pos.end.offset, enums01._1.pos.start.offset - enums00._1.pos.end.offset) + (enums01._2))
          else sb.append("")
        }

        if (enums0.length > 2) {
          if (enums0.last ne enums1.last) sb.append(
            new String(origInputChars, enums0(enums0.length - 2).pos.end.offset, enums0.last.pos.start.offset - enums0(enums0.length - 2).pos.end.offset) +
              (enums1.last)
          )
          else sb.append("")
        }
        else sb.append("") //sb.append(new String(origInputChars, enums0.last.pos.end.offset, orig.pos.end.offset - enums0.last.pos.end.offset))

        if (body0 ne body1) {
          sb.append(
            new String(origInputChars, enums0.last.pos.end.offset, body0.pos.start.offset - enums0.last.pos.end.offset) +
              (body1) +
              new String(origInputChars, body0.pos.end.offset, orig.pos.end.offset - body0.pos.end.offset)
          )
        }
        else sb.append(orig.toString)
        
        s(sb.toString)
      case (Term.ForYield(enums0, body0), Term.ForYield(enums1, body1)) =>
        val sb = new StringBuilder
        if (enums0.head ne enums1.head) sb.append(
          new String(origInputChars, origPosStart, enums0.head.pos.start.offset - origPosStart) +
            (enums1.head)
        )
        else sb.append(new String(origInputChars, origPosStart, enums0.head.pos.end.offset - origPosStart))
        
        for (List(enums00, enums01) <- (enums0 zip enums1).grouped(2)) {
          if (enums01._1 ne enums01._2) sb.append(new String(origInputChars, enums00._1.pos.end.offset, enums01._1.pos.start.offset - enums00._1.pos.end.offset) + (enums01._2))
          else sb.append("")
        }

        if (enums0.length > 2) {
          if (enums0.last ne enums1.last) sb.append(
            new String(origInputChars, enums0(enums0.length - 2).pos.end.offset, enums0.last.pos.start.offset - enums0(enums0.length - 2).pos.end.offset) +
              (enums1.last)
          )
          else sb.append("")
        }
        else sb.append("") //sb.append(new String(origInputChars, enums0.last.pos.end.offset, orig.pos.end.offset - enums0.last.pos.end.offset))

        if (body0 ne body1) {
          sb.append(
            new String(origInputChars, enums0.last.pos.end.offset, body0.pos.start.offset - enums0.last.pos.end.offset) +
              (body1) +
              new String(origInputChars, body0.pos.end.offset, orig.pos.end.offset - body0.pos.end.offset)
          )
        }
        else sb.append(orig.toString)
        
        s(sb.toString)
      case (Term.Param(mods0, name0, decltpe0, default0), Term.Param(mods1, name1, decltpe1, default1)) =>
        val sb = new StringBuilder
        if (mods0.length == 0) {
          if (name0 ne name1) {
            sb.append(
              new String(origInputChars, origPosStart, name0.pos.start.offset - origPosStart) +
                (name1) +
                new String(origInputChars, name0.pos.end.offset, orig.pos.end.offset - name0.pos.end.offset)
            )
          }
          else sb.append(orig.toString)

          s(sb.toString)
        } else {
          if (mods0.head ne mods1.head) sb.append(
            new String(origInputChars, origPosStart, mods0.head.pos.start.offset - origPosStart) +
              (mods1.head)
          )
          else sb.append(new String(origInputChars, origPosStart, mods0.head.pos.end.offset - origPosStart))
          
          for (List(mods00, mods01) <- (mods0 zip mods1).grouped(2)) {
            if (mods01._1 ne mods01._2) sb.append(new String(origInputChars, mods00._1.pos.end.offset, mods01._1.pos.start.offset - mods00._1.pos.end.offset) + (mods01._2))
            else sb.append("")
          }

          if (mods0.length > 2) {
            if (mods0.last ne mods1.last) sb.append(
              new String(origInputChars, mods0(mods0.length - 2).pos.end.offset, mods0.last.pos.start.offset - mods0(mods0.length - 2).pos.end.offset) +
                (mods1.last)
            )
            else sb.append("")
          }
          else sb.append("") //sb.append(new String(origInputChars, mods0.last.pos.end.offset, orig.pos.end.offset - mods0.last.pos.end.offset))

          /* Handle Option cases as well */

          if (name0 ne name1) {
            sb.append(
              new String(origInputChars, mods0.last.pos.end.offset, name0.pos.start.offset - mods0.last.pos.end.offset) +
                (name1) +
                new String(origInputChars, name0.pos.end.offset, orig.pos.end.offset - name0.pos.end.offset)
            )
          }
          else sb.append(orig.toString)
        }
        s(sb.toString)
      case (Term.TryWithTerm(expr0, catchp0, finallyp0), Term.TryWithTerm(expr1, catchp1, finallyp1)) =>
        /* handle option cases for finallyp */
        val sb = new StringBuilder
        if (expr0 ne expr1) {
          sb.append(
            new String(origInputChars, origPosStart, expr0.pos.start.offset - origPosStart) +
              (expr1)
          )
        }
        else sb.append(new String(origInputChars, origPosStart, expr0.pos.end.offset - origPosStart))

        if (catchp0 ne catchp1) {
          sb.append(
            new String(origInputChars, expr0.pos.end.offset, catchp0.pos.start.offset - expr0.pos.end.offset) +
              (catchp1) +
              new String(origInputChars, catchp0.pos.end.offset, orig.pos.end.offset - catchp0.pos.end.offset)
          )
        }
        else sb.append(new String(origInputChars, expr0.pos.end.offset, orig.pos.end.offset - expr0.pos.end.offset))

        s(sb.toString)
      case (Term.TryWithCases(expr0, catchp0, finallyp0) , Term.TryWithCases(expr1, catchp1, finallyp1)) =>
        /* handle Option cases */
        val sb = new StringBuilder
        if (expr0 ne expr1) {
          sb.append(
            new String(origInputChars, origPosStart, expr0.pos.start.offset - origPosStart) +
              (expr1).toString
          )
        }
        else sb.append(new String(origInputChars, origPosStart, expr0.pos.end.offset - origPosStart))

        /* handle Seq[Arg] */
        if (catchp0.head ne catchp1.head) sb.append(
          new String(origInputChars, expr0.pos.end.offset, catchp0.head.pos.start.offset - expr0.pos.end.offset) +
            (catchp1.head).toString
        )
        else sb.append(new String(origInputChars, origPosStart, catchp0.head.pos.end.offset - origPosStart))

        for (List(catchp00, catchp01) <- (catchp0 zip catchp1).grouped(2)) {
          if (catchp01._1 ne catchp01._2) sb.append(new String(origInputChars, catchp00._1.pos.end.offset, catchp01._1.pos.start.offset - catchp00._1.pos.end.offset) + (catchp01._2).toString)
          else sb.append("")
        }

        if (catchp0.length > 2) {
          if (catchp0.last ne catchp1.last) sb.append(
            new String(origInputChars, catchp0(catchp0.length - 2).pos.end.offset, catchp0.last.pos.start.offset - catchp0(catchp0.length - 2).pos.end.offset) +
              (catchp1.last).toString +
              new String(origInputChars, catchp0.last.pos.end.offset, orig.pos.end.offset - catchp0.last.pos.end.offset)
          )
          else sb.append("")
        }
        else sb.append(new String(origInputChars, catchp0.last.pos.end.offset, orig.pos.end.offset - catchp0.last.pos.end.offset))

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
      case (Pat.Type.Project(qual0, name0), Pat.Type.Project(qual1, name1)) =>
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
      case (Pat.Type.Apply(tpe0, args0), Pat.Type.Apply(tpe1, args1)) =>
        val sb = new StringBuilder
        if (tpe0 ne tpe1) {
          sb.append(
            new String(origInputChars, origPosStart, tpe0.pos.start.offset - origPosStart) +
              (tpe1).toString
          )
        }
        else sb.append(new String(origInputChars, origPosStart, tpe0.pos.end.offset - origPosStart))

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
      case (Pat.Type.ApplyInfix(lhs0, op0, rhs0), Pat.Type.ApplyInfix(lhs1, op1, rhs1)) =>
        val sb = new StringBuilder
        if (lhs0 ne lhs1) {
          sb.append(
            new String(origInputChars, origPosStart, lhs0.pos.start.offset - origPosStart) +
              (lhs1)
          )
        }
        else sb.append(new String(origInputChars, origPosStart, lhs0.pos.end.offset - origPosStart))

        if (op0 ne op1) {
          sb.append(
            new String(origInputChars, lhs0.pos.end.offset, op0.pos.start.offset - lhs0.pos.end.offset) +
              (op1)
          )
        }
        else sb.append(new String(origInputChars, lhs0.pos.end.offset, op0.pos.end.offset - lhs0.pos.end.offset))

        if (rhs0 ne rhs1) {
          sb.append(
            new String(origInputChars, op0.pos.end.offset, rhs0.pos.start.offset - op0.pos.end.offset) +
              (rhs1)
          )
        }
        else sb.append(new String(origInputChars, op0.pos.end.offset, orig.pos.end.offset - op0.pos.end.offset))
        s(sb.toString)
      case (Pat.Type.Function(params0, res0), Pat.Type.Function(params1, res1)) =>
        val sb = new StringBuilder
        if (params0.length == 0) {
          if (res0 ne res1) {
            sb.append(
              new String(origInputChars, origPosStart, res0.pos.start.offset - origPosStart) +
                (res1) +
                new String(origInputChars, res0.pos.end.offset, orig.pos.end.offset - res0.pos.end.offset)
            )
          }
          else sb.append(orig.toString)

          s(sb.toString)
        } else {
          if (params0.head ne params1.head) sb.append(
            new String(origInputChars, origPosStart, params0.head.pos.start.offset - origPosStart) +
              (params1.head)
          )
          else sb.append(new String(origInputChars, origPosStart, params0.head.pos.end.offset - origPosStart))
          
          for (List(params00, params01) <- (params0 zip params1).grouped(2)) {
            if (params01._1 ne params01._2) sb.append(new String(origInputChars, params00._1.pos.end.offset, params01._1.pos.start.offset - params00._1.pos.end.offset) + (params01._2))
            else sb.append("")
          }

          if (params0.length > 2) {
            if (params0.last ne params1.last) sb.append(
              new String(origInputChars, params0(params0.length - 2).pos.end.offset, params0.last.pos.start.offset - params0(params0.length - 2).pos.end.offset) +
                (params1.last)
            )
            else sb.append("")
          }
          else sb.append("") //sb.append(new String(origInputChars, params0.last.pos.end.offset, orig.pos.end.offset - params0.last.pos.end.offset))

          if (res0 ne res1) {
            sb.append(
              new String(origInputChars, params0.last.pos.end.offset, res0.pos.start.offset - params0.last.pos.end.offset) +
                (res1) +
                new String(origInputChars, res0.pos.end.offset, orig.pos.end.offset - res0.pos.end.offset)
            )
          }
          else sb.append(orig.toString)
        }
        s(sb.toString)
      case (Pat.Type.Tuple(elements0), Pat.Type.Tuple(elements1)) =>
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
      case (Pat.Type.Existential(tpe0, quants0), Pat.Type.Existential(tpe1, quants1)) =>
        val sb = new StringBuilder
        if (tpe0 ne tpe1) {
          sb.append(
            new String(origInputChars, origPosStart, tpe0.pos.start.offset - origPosStart) +
              (tpe1).toString
          )
        }
        else sb.append(new String(origInputChars, origPosStart, tpe0.pos.end.offset - origPosStart))

        /* handle Seq[Arg] */
        if (quants0.head ne quants1.head) sb.append(
          new String(origInputChars, origPosStart, quants0.head.pos.start.offset - origPosStart) +
            (quants1.head).toString
        )
        else sb.append(new String(origInputChars, origPosStart, quants0.head.pos.end.offset - origPosStart))

        for (List(quants00, quants01) <- (quants0 zip quants1).grouped(2)) {
          if (quants01._1 ne quants01._2) sb.append(new String(origInputChars, quants00._1.pos.end.offset, quants01._1.pos.start.offset - quants00._1.pos.end.offset) + (quants01._2).toString)
          else sb.append("")
        }

        if (quants0.length > 2) {
          if (quants0.last ne quants1.last) sb.append(
            new String(origInputChars, quants0(quants0.length - 2).pos.end.offset, quants0.last.pos.start.offset - quants0(quants0.length - 2).pos.end.offset) +
              (quants1.last).toString +
              new String(origInputChars, quants0.last.pos.end.offset, orig.pos.end.offset - quants0.last.pos.end.offset)
          )
          else sb.append("")
        }
        else sb.append(new String(origInputChars, quants0.last.pos.end.offset, orig.pos.end.offset - quants0.last.pos.end.offset))

        s(sb.toString)
      case (Pat.Type.Annotate(tpe0, annots0), Pat.Type.Annotate(tpe1, annots1)) =>
        val sb = new StringBuilder
        if (tpe0 ne tpe1) {
          sb.append(
            new String(origInputChars, origPosStart, tpe0.pos.start.offset - origPosStart) +
              (tpe1).toString
          )
        }
        else sb.append(new String(origInputChars, origPosStart, tpe0.pos.end.offset - origPosStart))

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

      /* TYPE CASES */
      case (Type.Name(v0), Type.Name(v1)) =>
        val sb = new StringBuilder

        if (v0 ne v1) sb.append(v1)
        else sb.append(v0)

        s(sb.toString)
      case (Type.Select(qual0, name0), Type.Select(qual1, name1)) =>
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
      case (Type.Project(qual0, name0), Type.Project(qual1, name1)) =>
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
      case (Type.Singleton(ref0), Type.Singleton(ref1)) =>
        val sb = new StringBuilder
        if (ref0 ne ref1) {
          sb.append(
            new String(origInputChars, origPosStart, ref0.pos.start.offset - origPosStart) +
              (ref1) +
              new String(origInputChars, ref0.pos.end.offset, orig.pos.end.offset - ref0.pos.end.offset)
          )
        }
        else sb.append(orig.toString)

        s(sb.toString)
      case (Type.Apply(tpe0, args0), Type.Apply(tpe1, args1)) =>
        val sb = new StringBuilder
        if (tpe0 ne tpe1) {
          sb.append(
            new String(origInputChars, origPosStart, tpe0.pos.start.offset - origPosStart) +
              (tpe1).toString
          )
        }
        else sb.append(new String(origInputChars, origPosStart, tpe0.pos.end.offset - origPosStart))

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
      case (Type.Function(params0, res0), Type.Function(params1, res1)) =>
        val sb = new StringBuilder
        if (params0.length == 0) {
          if (res0 ne res1) {
            sb.append(
              new String(origInputChars, origPosStart, res0.pos.start.offset - origPosStart) +
                (res1) +
                new String(origInputChars, res0.pos.end.offset, orig.pos.end.offset - res0.pos.end.offset)
            )
          }
          else sb.append(orig.toString)

          s(sb.toString)
        } else {
          if (params0.head ne params1.head) sb.append(
            new String(origInputChars, origPosStart, params0.head.pos.start.offset - origPosStart) +
              (params1.head)
          )
          else sb.append(new String(origInputChars, origPosStart, params0.head.pos.end.offset - origPosStart))
          
          for (List(params00, params01) <- (params0 zip params1).grouped(2)) {
            if (params01._1 ne params01._2) sb.append(new String(origInputChars, params00._1.pos.end.offset, params01._1.pos.start.offset - params00._1.pos.end.offset) + (params01._2))
            else sb.append("")
          }

          if (params0.length > 2) {
            if (params0.last ne params1.last) sb.append(
              new String(origInputChars, params0(params0.length - 2).pos.end.offset, params0.last.pos.start.offset - params0(params0.length - 2).pos.end.offset) +
                (params1.last)
            )
            else sb.append("")
          }
          else sb.append("") //sb.append(new String(origInputChars, params0.last.pos.end.offset, orig.pos.end.offset - params0.last.pos.end.offset))

          if (res0 ne res1) {
            sb.append(
              new String(origInputChars, params0.last.pos.end.offset, res0.pos.start.offset - params0.last.pos.end.offset) +
                (res1) +
                new String(origInputChars, res0.pos.end.offset, orig.pos.end.offset - res0.pos.end.offset)
            )
          }
          else sb.append(orig.toString)
        }
        s(sb.toString)
      case (Type.Existential(tpe0, quants0), Type.Existential(tpe1, quants1)) =>
        val sb = new StringBuilder
        if (tpe0 ne tpe1) {
          sb.append(
            new String(origInputChars, origPosStart, tpe0.pos.start.offset - origPosStart) +
              (tpe1).toString
          )
        }
        else sb.append(new String(origInputChars, origPosStart, tpe0.pos.end.offset - origPosStart))

        /* handle Seq[Arg] */
        if (quants0.head ne quants1.head) sb.append(
          new String(origInputChars, origPosStart, quants0.head.pos.start.offset - origPosStart) +
            (quants1.head).toString
        )
        else sb.append(new String(origInputChars, origPosStart, quants0.head.pos.end.offset - origPosStart))

        for (List(quants00, quants01) <- (quants0 zip quants1).grouped(2)) {
          if (quants01._1 ne quants01._2) sb.append(new String(origInputChars, quants00._1.pos.end.offset, quants01._1.pos.start.offset - quants00._1.pos.end.offset) + (quants01._2).toString)
          else sb.append("")
        }

        if (quants0.length > 2) {
          if (quants0.last ne quants1.last) sb.append(
            new String(origInputChars, quants0(quants0.length - 2).pos.end.offset, quants0.last.pos.start.offset - quants0(quants0.length - 2).pos.end.offset) +
              (quants1.last).toString +
              new String(origInputChars, quants0.last.pos.end.offset, orig.pos.end.offset - quants0.last.pos.end.offset)
          )
          else sb.append("")
        }
        else sb.append(new String(origInputChars, quants0.last.pos.end.offset, orig.pos.end.offset - quants0.last.pos.end.offset))

        s(sb.toString)
      case (Type.Annotate(tpe0, annots0), Type.Annotate(tpe1, annots1)) =>
        val sb = new StringBuilder
        if (tpe0 ne tpe1) {
          sb.append(
            new String(origInputChars, origPosStart, tpe0.pos.start.offset - origPosStart) +
              (tpe1).toString
          )
        }
        else sb.append(new String(origInputChars, origPosStart, tpe0.pos.end.offset - origPosStart))

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
      case (Type.ApplyInfix(lhs0, op0, rhs0), Type.ApplyInfix(lhs1, op1, rhs1)) =>
        val sb = new StringBuilder
        if (lhs0 ne lhs1) {
          sb.append(
            new String(origInputChars, origPosStart, lhs0.pos.start.offset - origPosStart) +
              (lhs1)
          )
        }
        else sb.append(new String(origInputChars, origPosStart, lhs0.pos.end.offset - origPosStart))

        if (op0 ne op1) {
          sb.append(
            new String(origInputChars, lhs0.pos.end.offset, op0.pos.start.offset - lhs0.pos.end.offset) +
              (op1)
          )
        }
        else sb.append(new String(origInputChars, lhs0.pos.end.offset, op0.pos.end.offset - lhs0.pos.end.offset))

        if (rhs0 ne rhs1) {
          sb.append(
            new String(origInputChars, op0.pos.end.offset, rhs0.pos.start.offset - op0.pos.end.offset) +
              (rhs1)
          )
        }
        else sb.append(new String(origInputChars, op0.pos.end.offset, orig.pos.end.offset - op0.pos.end.offset))
        s(sb.toString)
      case (Type.Tuple(elements0), Type.Tuple(elements1)) =>
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
      case (Type.Arg.ByName(tpe0), Type.Arg.ByName(tpe1)) =>
        val sb = new StringBuilder
        if (tpe0 ne tpe1) {
          sb.append(
            new String(origInputChars, origPosStart, tpe0.pos.start.offset - origPosStart) +
              (tpe1) +
              new String(origInputChars, tpe0.pos.end.offset, orig.pos.end.offset - tpe0.pos.end.offset)
          )
        }
        else sb.append(orig.toString)

        s(sb.toString)
      case (Type.Arg.Repeated(tpe0), Type.Arg.Repeated(tpe1)) =>
        val sb = new StringBuilder
        if (tpe0 ne tpe1) {
          sb.append(
            new String(origInputChars, origPosStart, tpe0.pos.start.offset - origPosStart) +
              (tpe1) +
              new String(origInputChars, tpe0.pos.end.offset, orig.pos.end.offset - tpe0.pos.end.offset)
          )
        }
        else sb.append(orig.toString)

        s(sb.toString)

      /* DECL cases */
      case (Decl.Val(mods0, pats0, decltpe0), Decl.Val(mods1, pats1, decltpe1)) =>
        val sb = new StringBuilder
        if (mods0.length == 0) {
          /* check other fields */
          sb.append("")

        } else {          
          if (mods0.head ne mods1.head) sb.append(
            new String(origInputChars, origPosStart, mods0.head.pos.start.offset - origPosStart) +
              (mods1.head)
          )
          else sb.append(new String(origInputChars, origPosStart, mods0.head.pos.end.offset - origPosStart))
          
          for (List(mods00, mods01) <- (mods0 zip mods1).grouped(2)) {
            if (mods01._1 ne mods01._2) sb.append(new String(origInputChars, mods00._1.pos.end.offset, mods01._1.pos.start.offset - mods00._1.pos.end.offset) + (mods01._2))
            else sb.append("")
          }

          if (mods0.length > 2) {
            if (mods0.last ne mods1.last) sb.append(
              new String(origInputChars, mods0(mods0.length - 2).pos.end.offset, mods0.last.pos.start.offset - mods0(mods0.length - 2).pos.end.offset) +
                (mods1.last)
                // new String(origInputChars, mods0.last.pos.end.offset, orig.pos.end.offset - mods0.last.pos.end.offset)
            )
            else sb.append("")
          }
          else sb.append("") // sb.append(new String(origInputChars, mods0.last.pos.end.offset, orig.pos.end.offset - mods0.last.pos.end.offset))

          if (pats0.head ne pats1.head) sb.append(
            new String(origInputChars, mods0.last.pos.end.offset, pats0.head.pos.start.offset - mods0.last.pos.end.offset) +
              (pats1.head)
          )
          else sb.append(new String(origInputChars, mods0.last.pos.end.offset, pats0.head.pos.end.offset - mods0.last.pos.end.offset))
          
          for (List(pats00, pats01) <- (pats0 zip pats1).grouped(2)) {
            if (pats01._1 ne pats01._2) sb.append(new String(origInputChars, pats00._1.pos.end.offset, pats01._1.pos.start.offset - pats00._1.pos.end.offset) + (pats01._2))
            else sb.append("")
          }

          if (pats0.length > 2) {
            if (pats0.last ne pats1.last) sb.append(
              new String(origInputChars, pats0(pats0.length - 2).pos.end.offset, pats0.last.pos.start.offset - pats0(pats0.length - 2).pos.end.offset) +
                (pats1.last)
                // new String(origInputChars, pats0.last.pos.end.offset, orig.pos.end.offset - pats0.last.pos.end.offset)
            )
            else sb.append("")
          }
          else sb.append("") // sb.append(new String(origInputChars, pats0.last.pos.end.offset, orig.pos.end.offset - pats0.last.pos.end.offset))

          if (decltpe0 ne decltpe1) {
            sb.append(
              new String(origInputChars, pats0.last.pos.end.offset, decltpe0.pos.start.offset - pats0.last.pos.end.offset) +
                (decltpe1) +
                new String(origInputChars, decltpe0.pos.end.offset, orig.pos.end.offset - decltpe0.pos.end.offset)
            )
          }
          else sb.append(orig.toString)
        }
        s(sb.toString)
      case (Decl.Var(mods0, pats0, decltpe0), Decl.Var(mods1, pats1, decltpe1)) =>
        val sb = new StringBuilder
        if (mods0.length == 0) {
          /* check other fields */
          sb.append("")

        } else {          
          if (mods0.head ne mods1.head) sb.append(
            new String(origInputChars, origPosStart, mods0.head.pos.start.offset - origPosStart) +
              (mods1.head)
          )
          else sb.append(new String(origInputChars, origPosStart, mods0.head.pos.end.offset - origPosStart))
          
          for (List(mods00, mods01) <- (mods0 zip mods1).grouped(2)) {
            if (mods01._1 ne mods01._2) sb.append(new String(origInputChars, mods00._1.pos.end.offset, mods01._1.pos.start.offset - mods00._1.pos.end.offset) + (mods01._2))
            else sb.append("")
          }

          if (mods0.length > 2) {
            if (mods0.last ne mods1.last) sb.append(
              new String(origInputChars, mods0(mods0.length - 2).pos.end.offset, mods0.last.pos.start.offset - mods0(mods0.length - 2).pos.end.offset) +
                (mods1.last)
                // new String(origInputChars, mods0.last.pos.end.offset, orig.pos.end.offset - mods0.last.pos.end.offset)
            )
            else sb.append("")
          }
          else sb.append("") // sb.append(new String(origInputChars, mods0.last.pos.end.offset, orig.pos.end.offset - mods0.last.pos.end.offset))

          if (pats0.head ne pats1.head) sb.append(
            new String(origInputChars, mods0.last.pos.end.offset, pats0.head.pos.start.offset - mods0.last.pos.end.offset) +
              (pats1.head)
          )
          else sb.append(new String(origInputChars, mods0.last.pos.end.offset, pats0.head.pos.end.offset - mods0.last.pos.end.offset))
          
          for (List(pats00, pats01) <- (pats0 zip pats1).grouped(2)) {
            if (pats01._1 ne pats01._2) sb.append(new String(origInputChars, pats00._1.pos.end.offset, pats01._1.pos.start.offset - pats00._1.pos.end.offset) + (pats01._2))
            else sb.append("")
          }

          if (pats0.length > 2) {
            if (pats0.last ne pats1.last) sb.append(
              new String(origInputChars, pats0(pats0.length - 2).pos.end.offset, pats0.last.pos.start.offset - pats0(pats0.length - 2).pos.end.offset) +
                (pats1.last)
                // new String(origInputChars, pats0.last.pos.end.offset, orig.pos.end.offset - pats0.last.pos.end.offset)
            )
            else sb.append("")
          }
          else sb.append("") // sb.append(new String(origInputChars, pats0.last.pos.end.offset, orig.pos.end.offset - pats0.last.pos.end.offset))

          if (decltpe0 ne decltpe1) {
            sb.append(
              new String(origInputChars, pats0.last.pos.end.offset, decltpe0.pos.start.offset - pats0.last.pos.end.offset) +
                (decltpe1) +
                new String(origInputChars, decltpe0.pos.end.offset, orig.pos.end.offset - decltpe0.pos.end.offset)
            )
          }
          else sb.append(orig.toString)
        }
        s(sb.toString)
      case (Decl.Def(mods0, name0, tparams0, paramss0, decltpe0), Decl.Def(mods1, name1, tparams1, paramss1, decltpe1)) =>
        val sb = new StringBuilder
        if (mods0.length == 0) {
          /* check other fields */
          sb.append("")

        } else {          
          if (mods0.head ne mods1.head) sb.append(
            new String(origInputChars, origPosStart, mods0.head.pos.start.offset - origPosStart) +
              (mods1.head)
          )
          else sb.append(new String(origInputChars, origPosStart, mods0.head.pos.end.offset - origPosStart))
          
          for (List(mods00, mods01) <- (mods0 zip mods1).grouped(2)) {
            if (mods01._1 ne mods01._2) sb.append(new String(origInputChars, mods00._1.pos.end.offset, mods01._1.pos.start.offset - mods00._1.pos.end.offset) + (mods01._2))
            else sb.append("")
          }

          if (mods0.length > 2) {
            if (mods0.last ne mods1.last) sb.append(
              new String(origInputChars, mods0(mods0.length - 2).pos.end.offset, mods0.last.pos.start.offset - mods0(mods0.length - 2).pos.end.offset) +
                (mods1.last)
                // new String(origInputChars, mods0.last.pos.end.offset, orig.pos.end.offset - mods0.last.pos.end.offset)
            )
            else sb.append("")
          }
          else sb.append("") // sb.append(new String(origInputChars, mods0.last.pos.end.offset, orig.pos.end.offset - mods0.last.pos.end.offset))
        }

        if (name0 ne name1) sb.append(
          new String(origInputChars, mods0.last.pos.end.offset, name0.pos.start.offset - mods0.last.pos.end.offset) +
            (name1)
        )
        else sb.append("")

        if (tparams0.length == 0) {
          /* check other fields */
          sb.append("")
        } else {
          if (tparams0.head ne tparams1.head) sb.append(
            new String(origInputChars, name0.pos.end.offset, tparams0.head.pos.start.offset - name0.pos.end.offset) +
              (tparams1.head)
          )
          else sb.append(new String(origInputChars, origPosStart, tparams0.head.pos.end.offset - origPosStart))
          
          for (List(tparams00, tparams01) <- (tparams0 zip tparams1).grouped(2)) {
            if (tparams01._1 ne tparams01._2) sb.append(new String(origInputChars, tparams00._1.pos.end.offset, tparams01._1.pos.start.offset - tparams00._1.pos.end.offset) + (tparams01._2))
            else sb.append("")
          }

          if (tparams0.length > 2) {
            if (tparams0.last ne tparams1.last) sb.append(
              new String(origInputChars, tparams0(tparams0.length - 2).pos.end.offset, tparams0.last.pos.start.offset - tparams0(tparams0.length - 2).pos.end.offset) +
                (tparams1.last)
                // new String(origInputChars, tparams0.last.pos.end.offset, orig.pos.end.offset - tparams0.last.pos.end.offset)
            )
            else sb.append("")
          }
          else sb.append("") // sb.append(new String(origInputChars, tparams0.last.pos.end.offset, orig.pos.end.offset - tparams0.last.pos.end.offset))
        }

        if (paramss0.length == 0) {
          /* check other fields */
          sb.append("")
        } else {
          if (paramss0.head ne paramss1.head) sb.append(
            new String(origInputChars, tparams0.last.pos.end.offset, paramss0.head.head.pos.start.offset - tparams0.last.pos.end.offset) +
              (paramss1.head)
          )
          else sb.append(new String(origInputChars, origPosStart, paramss0.head.head.pos.end.offset - origPosStart))
          
          for (List(paramss00, paramss01) <- (paramss0 zip paramss1).grouped(2)) {
            if (paramss01._1 ne paramss01._2) sb.append(new String(origInputChars, paramss00._1.last.pos.end.offset, paramss01._1.head.pos.start.offset - paramss00._1.last.pos.end.offset) + (paramss01._2))
            else sb.append("")
          }

          if (paramss0.length > 2) {
            if (paramss0.last ne paramss1.last) sb.append(
              new String(origInputChars, paramss0(paramss0.length - 2).last.pos.end.offset, paramss0.last.last.pos.start.offset - paramss0(paramss0.length - 2).last.pos.end.offset) +
                (paramss1.last)
                // new String(origInputChars, paramss0.last.pos.end.offset, orig.pos.end.offset - paramss0.last.pos.end.offset)
            )
            else sb.append("")
          }
          else sb.append("") // sb.append(new String(origInputChars, paramss0.last.pos.end.offset, orig.pos.end.offset - paramss0.last.pos.end.offset))
        }

        if (decltpe0 ne decltpe1) sb.append(
          new String(origInputChars, paramss0.last.last.pos.end.offset, decltpe0.pos.start.offset - paramss0.last.last.pos.end.offset) +
            (decltpe1) +
            new String(origInputChars, decltpe0.pos.end.offset, orig.pos.end.offset - decltpe0.pos.end.offset)
        )
        else sb.append("")

        s(sb.toString)
      case (Decl.Type(mods0, name0, tparams0, bounds0), Decl.Type(mods1, name1, tparams1, bounds1)) =>
        val sb = new StringBuilder
        if (mods0.length == 0) {
          /* check other fields */
          sb.append("")

        } else {          
          if (mods0.head ne mods1.head) sb.append(
            new String(origInputChars, origPosStart, mods0.head.pos.start.offset - origPosStart) +
              (mods1.head)
          )
          else sb.append(new String(origInputChars, origPosStart, mods0.head.pos.end.offset - origPosStart))
          
          for (List(mods00, mods01) <- (mods0 zip mods1).grouped(2)) {
            if (mods01._1 ne mods01._2) sb.append(new String(origInputChars, mods00._1.pos.end.offset, mods01._1.pos.start.offset - mods00._1.pos.end.offset) + (mods01._2))
            else sb.append("")
          }

          if (mods0.length > 2) {
            if (mods0.last ne mods1.last) sb.append(
              new String(origInputChars, mods0(mods0.length - 2).pos.end.offset, mods0.last.pos.start.offset - mods0(mods0.length - 2).pos.end.offset) +
                (mods1.last)
                // new String(origInputChars, mods0.last.pos.end.offset, orig.pos.end.offset - mods0.last.pos.end.offset)
            )
            else sb.append("")
          }
          else sb.append("") // sb.append(new String(origInputChars, mods0.last.pos.end.offset, orig.pos.end.offset - mods0.last.pos.end.offset))
        }

        if (name0 ne name1) sb.append(
          new String(origInputChars, mods0.last.pos.end.offset, name0.pos.start.offset - mods0.last.pos.end.offset) +
            (name1)
        )
        else sb.append("")

        if (tparams0.length == 0) {
          /* check other fields */
          sb.append("")
        } else {
          if (tparams0.head ne tparams1.head) sb.append(
            new String(origInputChars, name0.pos.end.offset, tparams0.head.pos.start.offset - name0.pos.end.offset) +
              (tparams1.head)
          )
          else sb.append(new String(origInputChars, origPosStart, tparams0.head.pos.end.offset - origPosStart))
          
          for (List(tparams00, tparams01) <- (tparams0 zip tparams1).grouped(2)) {
            if (tparams01._1 ne tparams01._2) sb.append(new String(origInputChars, tparams00._1.pos.end.offset, tparams01._1.pos.start.offset - tparams00._1.pos.end.offset) + (tparams01._2))
            else sb.append("")
          }

          if (tparams0.length > 2) {
            if (tparams0.last ne tparams1.last) sb.append(
              new String(origInputChars, tparams0(tparams0.length - 2).pos.end.offset, tparams0.last.pos.start.offset - tparams0(tparams0.length - 2).pos.end.offset) +
                (tparams1.last)
                // new String(origInputChars, tparams0.last.pos.end.offset, orig.pos.end.offset - tparams0.last.pos.end.offset)
            )
            else sb.append("")
          }
          else sb.append("") // sb.append(new String(origInputChars, tparams0.last.pos.end.offset, orig.pos.end.offset - tparams0.last.pos.end.offset))
        }

        if (bounds0 ne bounds1) sb.append(
          new String(origInputChars, tparams0.last.pos.end.offset, bounds0.pos.start.offset - tparams0.last.pos.end.offset) +
            (bounds1) +
            new String(origInputChars, bounds0.pos.end.offset, orig.pos.end.offset - bounds0.pos.end.offset)
        )
        else sb.append("")

        s(sb.toString)
      case (Defn.Val(mods0, pats0, decltpe0, rhs0), Defn.Val(mods1, pats1, decltpe1, rhs1)) =>
        val sb = new StringBuilder
        
        if (mods0.length == 0) {
          /* check other fields */
          sb.append("")
          

        } else { 
          
          if (mods0.head ne mods1.head) sb.append(
            new String(origInputChars, origPosStart, mods0.head.pos.start.offset - origPosStart) +
              (mods1.head)
          )
          else sb.append(new String(origInputChars, origPosStart, mods0.head.pos.end.offset - origPosStart))
          
          for (List(mods00, mods01) <- (mods0 zip mods1).grouped(2)) {
            if (mods01._1 ne mods01._2) sb.append(new String(origInputChars, mods00._1.pos.end.offset, mods01._1.pos.start.offset - mods00._1.pos.end.offset) + (mods01._2))
            else sb.append("")
          }

          if (mods0.length > 2) {
            if (mods0.last ne mods1.last) sb.append(
              new String(origInputChars, mods0(mods0.length - 2).pos.end.offset, mods0.last.pos.start.offset - mods0(mods0.length - 2).pos.end.offset) +
                (mods1.last)
                // new String(origInputChars, mods0.last.pos.end.offset, orig.pos.end.offset - mods0.last.pos.end.offset)
            )
            else sb.append("")
          }
          else sb.append("") // sb.append(new String(origInputChars, mods0.last.pos.end.offset, orig.pos.end.offset - mods0.last.pos.end.offset))

          if (pats0.head ne pats1.head) sb.append(
            new String(origInputChars, mods0.last.pos.end.offset, pats0.head.pos.start.offset - mods0.last.pos.end.offset) +
              (pats1.head)
          )
          else sb.append(new String(origInputChars, mods0.last.pos.end.offset, pats0.head.pos.end.offset - mods0.last.pos.end.offset))
          
          for (List(pats00, pats01) <- (pats0 zip pats1).grouped(2)) {
            if (pats01._1 ne pats01._2) sb.append(new String(origInputChars, pats00._1.pos.end.offset, pats01._1.pos.start.offset - pats00._1.pos.end.offset) + (pats01._2))
            else sb.append("")
          }

          if (pats0.length > 2) {
            if (pats0.last ne pats1.last) sb.append(
              new String(origInputChars, pats0(pats0.length - 2).pos.end.offset, pats0.last.pos.start.offset - pats0(pats0.length - 2).pos.end.offset) +
                (pats1.last)
                // new String(origInputChars, pats0.last.pos.end.offset, orig.pos.end.offset - pats0.last.pos.end.offset)
            )
            else sb.append("")
          }
          else sb.append("") // sb.append(new String(origInputChars, pats0.last.pos.end.offset, orig.pos.end.offset - pats0.last.pos.end.offset))

          /*
           if (decltpe0 ne decltpe1) {
           sb.append(
           new String(origInputChars, pats0.last.pos.end.offset, decltpe0.pos.start.offset - pats0.last.pos.end.offset) +
           (decltpe1)                
           )
           }
           else sb.append(orig.toString)
           */

          if (rhs0 ne rhs1) {
            sb.append(
              new String(origInputChars, pats0.last.pos.end.offset, rhs0.pos.start.offset - pats0.last.pos.end.offset) +
                (rhs1) +
                new String(origInputChars, rhs0.pos.end.offset, orig.pos.end.offset - rhs0.pos.end.offset)
            )
          }
          else sb.append(orig.toString)
          // new String(origInputChars, decltpe0.pos.end.offset, orig.pos.end.offset - decltpe0.pos.end.offset)
        }  

        s(sb.toString)
      case (Defn.Def(mods0, name0, tparams0, paramss0, decltpe0, body0), Defn.Def(mods1, name1, tparams1, paramss1, decltpe1, body1)) =>
        val sb = new StringBuilder
        if (mods0.length == 0) {
          /* check other fields */
          sb.append("")

        } else {          
          if (mods0.head ne mods1.head) sb.append(
            new String(origInputChars, origPosStart, mods0.head.pos.start.offset - origPosStart) +
              (mods1.head)
          )
          else sb.append(new String(origInputChars, origPosStart, mods0.head.pos.end.offset - origPosStart))
          
          for (List(mods00, mods01) <- (mods0 zip mods1).grouped(2)) {
            if (mods01._1 ne mods01._2) sb.append(new String(origInputChars, mods00._1.pos.end.offset, mods01._1.pos.start.offset - mods00._1.pos.end.offset) + (mods01._2))
            else sb.append("")
          }

          if (mods0.length > 2) {
            if (mods0.last ne mods1.last) sb.append(
              new String(origInputChars, mods0(mods0.length - 2).pos.end.offset, mods0.last.pos.start.offset - mods0(mods0.length - 2).pos.end.offset) +
                (mods1.last)
                // new String(origInputChars, mods0.last.pos.end.offset, orig.pos.end.offset - mods0.last.pos.end.offset)
            )
            else sb.append("")
          }
          else sb.append("") // sb.append(new String(origInputChars, mods0.last.pos.end.offset, orig.pos.end.offset - mods0.last.pos.end.offset))
        }

        if (name0 ne name1) sb.append(
          new String(origInputChars, mods0.last.pos.end.offset, name0.pos.start.offset - mods0.last.pos.end.offset) +
            (name1)
        )
        else sb.append("")

        if (tparams0.length == 0) {
          /* check other fields */
          sb.append("")
        } else {
          if (tparams0.head ne tparams1.head) sb.append(
            new String(origInputChars, name0.pos.end.offset, tparams0.head.pos.start.offset - name0.pos.end.offset) +
              (tparams1.head)
          )
          else sb.append(new String(origInputChars, origPosStart, tparams0.head.pos.end.offset - origPosStart))
          
          for (List(tparams00, tparams01) <- (tparams0 zip tparams1).grouped(2)) {
            if (tparams01._1 ne tparams01._2) sb.append(new String(origInputChars, tparams00._1.pos.end.offset, tparams01._1.pos.start.offset - tparams00._1.pos.end.offset) + (tparams01._2))
            else sb.append("")
          }

          if (tparams0.length > 2) {
            if (tparams0.last ne tparams1.last) sb.append(
              new String(origInputChars, tparams0(tparams0.length - 2).pos.end.offset, tparams0.last.pos.start.offset - tparams0(tparams0.length - 2).pos.end.offset) +
                (tparams1.last)
                // new String(origInputChars, tparams0.last.pos.end.offset, orig.pos.end.offset - tparams0.last.pos.end.offset)
            )
            else sb.append("")
          }
          else sb.append("") // sb.append(new String(origInputChars, tparams0.last.pos.end.offset, orig.pos.end.offset - tparams0.last.pos.end.offset))
        }

        if (paramss0.length == 0) {
          /* check other fields */
          sb.append("")
        } else {
          if (paramss0.head ne paramss1.head) sb.append(
            new String(origInputChars, tparams0.last.pos.end.offset, paramss0.head.head.pos.start.offset - tparams0.last.pos.end.offset) +
              (paramss1.head)
          )
          else sb.append(new String(origInputChars, origPosStart, paramss0.head.head.pos.end.offset - origPosStart))
          
          for (List(paramss00, paramss01) <- (paramss0 zip paramss1).grouped(2)) {
            if (paramss01._1 ne paramss01._2) sb.append(new String(origInputChars, paramss00._1.last.pos.end.offset, paramss01._1.head.pos.start.offset - paramss00._1.last.pos.end.offset) + (paramss01._2))
            else sb.append("")
          }

          if (paramss0.length > 2) {
            if (paramss0.last ne paramss1.last) sb.append(
              new String(origInputChars, paramss0(paramss0.length - 2).last.pos.end.offset, paramss0.last.last.pos.start.offset - paramss0(paramss0.length - 2).last.pos.end.offset) +
                (paramss1.last)
                // new String(origInputChars, paramss0.last.pos.end.offset, orig.pos.end.offset - paramss0.last.pos.end.offset)
            )
            else sb.append("")
          }
          else sb.append("") // sb.append(new String(origInputChars, paramss0.last.pos.end.offset, orig.pos.end.offset - paramss0.last.pos.end.offset))
        }

        if (body0 ne body1) sb.append(
          new String(origInputChars, paramss0.last.last.pos.end.offset, body0.pos.start.offset - paramss0.last.last.pos.end.offset) +
            (body1) +
            new String(origInputChars, body0.pos.end.offset, orig.pos.end.offset - body0.pos.end.offset)
        )
        else sb.append("")

        s(sb.toString)
      case (Defn.Type(mods0, name0, tparams0, body0), Defn.Type(mods1, name1, tparams1, body1)) =>
        val sb = new StringBuilder
        if (mods0.length == 0) {
          /* check other fields */
          sb.append("")

        } else {          
          if (mods0.head ne mods1.head) sb.append(
            new String(origInputChars, origPosStart, mods0.head.pos.start.offset - origPosStart) +
              (mods1.head)
          )
          else sb.append(new String(origInputChars, origPosStart, mods0.head.pos.end.offset - origPosStart))
          
          for (List(mods00, mods01) <- (mods0 zip mods1).grouped(2)) {
            if (mods01._1 ne mods01._2) sb.append(new String(origInputChars, mods00._1.pos.end.offset, mods01._1.pos.start.offset - mods00._1.pos.end.offset) + (mods01._2))
            else sb.append("")
          }

          if (mods0.length > 2) {
            if (mods0.last ne mods1.last) sb.append(
              new String(origInputChars, mods0(mods0.length - 2).pos.end.offset, mods0.last.pos.start.offset - mods0(mods0.length - 2).pos.end.offset) +
                (mods1.last)
                // new String(origInputChars, mods0.last.pos.end.offset, orig.pos.end.offset - mods0.last.pos.end.offset)
            )
            else sb.append("")
          }
          else sb.append("") // sb.append(new String(origInputChars, mods0.last.pos.end.offset, orig.pos.end.offset - mods0.last.pos.end.offset))
        }

        if (name0 ne name1) sb.append(
          new String(origInputChars, mods0.last.pos.end.offset, name0.pos.start.offset - mods0.last.pos.end.offset) +
            (name1)
        )
        else sb.append("")

        if (tparams0.length == 0) {
          /* check other fields */
          sb.append("")
        } else {
          if (tparams0.head ne tparams1.head) sb.append(
            new String(origInputChars, name0.pos.end.offset, tparams0.head.pos.start.offset - name0.pos.end.offset) +
              (tparams1.head)
          )
          else sb.append(new String(origInputChars, origPosStart, tparams0.head.pos.end.offset - origPosStart))
          
          for (List(tparams00, tparams01) <- (tparams0 zip tparams1).grouped(2)) {
            if (tparams01._1 ne tparams01._2) sb.append(new String(origInputChars, tparams00._1.pos.end.offset, tparams01._1.pos.start.offset - tparams00._1.pos.end.offset) + (tparams01._2))
            else sb.append("")
          }

          if (tparams0.length > 2) {
            if (tparams0.last ne tparams1.last) sb.append(
              new String(origInputChars, tparams0(tparams0.length - 2).pos.end.offset, tparams0.last.pos.start.offset - tparams0(tparams0.length - 2).pos.end.offset) +
                (tparams1.last)
                // new String(origInputChars, tparams0.last.pos.end.offset, orig.pos.end.offset - tparams0.last.pos.end.offset)
            )
            else sb.append("")
          }
          else sb.append("") // sb.append(new String(origInputChars, tparams0.last.pos.end.offset, orig.pos.end.offset - tparams0.last.pos.end.offset))
        }

        if (body0 ne body1) sb.append(
          new String(origInputChars, tparams0.last.pos.end.offset, body0.pos.start.offset - tparams0.last.pos.end.offset) +
            (body1) +
            new String(origInputChars, body0.pos.end.offset, orig.pos.end.offset - body0.pos.end.offset)
        )
        else sb.append("")

        s(sb.toString)
      case (Defn.Class(mods0, name0, tparams0, ctor0, templ0), Defn.Class(mods1, name1, tparams1, ctor1, templ1)) =>
        val sb = new StringBuilder
        if (mods0.length == 0) {
          /* check other fields */
          sb.append("")

        } else {          
          if (mods0.head ne mods1.head) sb.append(
            new String(origInputChars, origPosStart, mods0.head.pos.start.offset - origPosStart) +
              (mods1.head)
          )
          else sb.append(new String(origInputChars, origPosStart, mods0.head.pos.end.offset - origPosStart))
          
          for (List(mods00, mods01) <- (mods0 zip mods1).grouped(2)) {
            if (mods01._1 ne mods01._2) sb.append(new String(origInputChars, mods00._1.pos.end.offset, mods01._1.pos.start.offset - mods00._1.pos.end.offset) + (mods01._2))
            else sb.append("")
          }

          if (mods0.length > 2) {
            if (mods0.last ne mods1.last) sb.append(
              new String(origInputChars, mods0(mods0.length - 2).pos.end.offset, mods0.last.pos.start.offset - mods0(mods0.length - 2).pos.end.offset) +
                (mods1.last)
                // new String(origInputChars, mods0.last.pos.end.offset, orig.pos.end.offset - mods0.last.pos.end.offset)
            )
            else sb.append("")
          }
          else sb.append("") // sb.append(new String(origInputChars, mods0.last.pos.end.offset, orig.pos.end.offset - mods0.last.pos.end.offset))
        }

        if (name0 ne name1) sb.append(
          new String(origInputChars, mods0.last.pos.end.offset, name0.pos.start.offset - mods0.last.pos.end.offset) +
            (name1)
        )
        else sb.append("")

        if (tparams0.length == 0) {
          /* check other fields */
          sb.append("")
        } else {
          if (tparams0.head ne tparams1.head) sb.append(
            new String(origInputChars, name0.pos.end.offset, tparams0.head.pos.start.offset - name0.pos.end.offset) +
              (tparams1.head)
          )
          else sb.append(new String(origInputChars, origPosStart, tparams0.head.pos.end.offset - origPosStart))
          
          for (List(tparams00, tparams01) <- (tparams0 zip tparams1).grouped(2)) {
            if (tparams01._1 ne tparams01._2) sb.append(new String(origInputChars, tparams00._1.pos.end.offset, tparams01._1.pos.start.offset - tparams00._1.pos.end.offset) + (tparams01._2))
            else sb.append("")
          }

          if (tparams0.length > 2) {
            if (tparams0.last ne tparams1.last) sb.append(
              new String(origInputChars, tparams0(tparams0.length - 2).pos.end.offset, tparams0.last.pos.start.offset - tparams0(tparams0.length - 2).pos.end.offset) +
                (tparams1.last)
                // new String(origInputChars, tparams0.last.pos.end.offset, orig.pos.end.offset - tparams0.last.pos.end.offset)
            )
            else sb.append("")
          }
          else sb.append("") // sb.append(new String(origInputChars, tparams0.last.pos.end.offset, orig.pos.end.offset - tparams0.last.pos.end.offset))
        }

        if (ctor0 ne ctor1) sb.append(
          new String(origInputChars, tparams0.last.pos.end.offset, ctor0.pos.start.offset - tparams0.last.pos.end.offset) +
            (ctor1)
            // new String(origInputChars, ctor0.pos.end.offset, orig.pos.end.offset - ctor0.pos.end.offset)
        )
        else sb.append("")

        if (templ0 ne templ1) sb.append(
          new String(origInputChars, ctor0.pos.end.offset, templ0.pos.start.offset - ctor0.pos.end.offset) +
            (templ1)
            // new String(origInputChars, templ0.pos.end.offset, orig.pos.end.offset - templ0.pos.end.offset)
        )
        else sb.append("")

        s(sb.toString)

      case (Defn.Trait(mods0, name0, tparams0, ctor0, templ0), Defn.Trait(mods1, name1, tparams1, ctor1, templ1)) =>
        val sb = new StringBuilder
        if (mods0.length == 0) {
          /* check other fields */
          sb.append("")

        } else {
          if (mods0.head ne mods1.head) sb.append(
            new String(origInputChars, origPosStart, mods0.head.pos.start.offset - origPosStart) +
              (mods1.head)
          )
          else sb.append(new String(origInputChars, origPosStart, mods0.head.pos.end.offset - origPosStart))
          
          for (List(mods00, mods01) <- (mods0 zip mods1).grouped(2)) {
            if (mods01._1 ne mods01._2) sb.append(new String(origInputChars, mods00._1.pos.end.offset, mods01._1.pos.start.offset - mods00._1.pos.end.offset) + (mods01._2))
            else sb.append("")
          }

          if (mods0.length > 2) {
            if (mods0.last ne mods1.last) sb.append(
              new String(origInputChars, mods0(mods0.length - 2).pos.end.offset, mods0.last.pos.start.offset - mods0(mods0.length - 2).pos.end.offset) +
                (mods1.last)
                // new String(origInputChars, mods0.last.pos.end.offset, orig.pos.end.offset - mods0.last.pos.end.offset)
            )
            else sb.append("")
          }
          else sb.append("") // sb.append(new String(origInputChars, mods0.last.pos.end.offset, orig.pos.end.offset - mods0.last.pos.end.offset))
        }

        if (name0 ne name1) sb.append(
          new String(origInputChars, mods0.last.pos.end.offset, name0.pos.start.offset - mods0.last.pos.end.offset) +
            (name1)
        )
        else sb.append("")

        if (tparams0.length == 0) {
          /* check other fields */
          sb.append("")
        } else {
          if (tparams0.head ne tparams1.head) sb.append(
            new String(origInputChars, name0.pos.end.offset, tparams0.head.pos.start.offset - name0.pos.end.offset) +
              (tparams1.head)
          )
          else sb.append(new String(origInputChars, origPosStart, tparams0.head.pos.end.offset - origPosStart))
          
          for (List(tparams00, tparams01) <- (tparams0 zip tparams1).grouped(2)) {
            if (tparams01._1 ne tparams01._2) sb.append(new String(origInputChars, tparams00._1.pos.end.offset, tparams01._1.pos.start.offset - tparams00._1.pos.end.offset) + (tparams01._2))
            else sb.append("")
          }

          if (tparams0.length > 2) {
            if (tparams0.last ne tparams1.last) sb.append(
              new String(origInputChars, tparams0(tparams0.length - 2).pos.end.offset, tparams0.last.pos.start.offset - tparams0(tparams0.length - 2).pos.end.offset) +
                (tparams1.last)
                // new String(origInputChars, tparams0.last.pos.end.offset, orig.pos.end.offset - tparams0.last.pos.end.offset)
            )
            else sb.append("")
          }
          else sb.append("") // sb.append(new String(origInputChars, tparams0.last.pos.end.offset, orig.pos.end.offset - tparams0.last.pos.end.offset))
        }

        if (ctor0 ne ctor1) sb.append(
          new String(origInputChars, tparams0.last.pos.end.offset, ctor0.pos.start.offset - tparams0.last.pos.end.offset) +
            (ctor1)
            // new String(origInputChars, ctor0.pos.end.offset, orig.pos.end.offset - ctor0.pos.end.offset)
        )
        else sb.append("")

        if (templ0 ne templ1) sb.append(
          new String(origInputChars, ctor0.pos.end.offset, templ0.pos.start.offset - ctor0.pos.end.offset) +
            (templ1)
            // new String(origInputChars, templ0.pos.end.offset, orig.pos.end.offset - templ0.pos.end.offset)
        )
        else sb.append("")

        s(sb.toString)
      case (Defn.Object(mods0, name0, templ0), Defn.Object(mods1, name1, templ1)) =>
        val sb = new StringBuilder
        if (mods0.length == 0) {
          /* check other fields */
          sb.append("")

        } else {
          if (mods0.head ne mods1.head) sb.append(
            new String(origInputChars, origPosStart, mods0.head.pos.start.offset - origPosStart) +
              (mods1.head)
          )
          else sb.append(new String(origInputChars, origPosStart, mods0.head.pos.end.offset - origPosStart))
          
          for (List(mods00, mods01) <- (mods0 zip mods1).grouped(2)) {
            if (mods01._1 ne mods01._2) sb.append(new String(origInputChars, mods00._1.pos.end.offset, mods01._1.pos.start.offset - mods00._1.pos.end.offset) + (mods01._2))
            else sb.append("")
          }

          if (mods0.length > 2) {
            if (mods0.last ne mods1.last) sb.append(
              new String(origInputChars, mods0(mods0.length - 2).pos.end.offset, mods0.last.pos.start.offset - mods0(mods0.length - 2).pos.end.offset) +
                (mods1.last)
                // new String(origInputChars, mods0.last.pos.end.offset, orig.pos.end.offset - mods0.last.pos.end.offset)
            )
            else sb.append("")
          }
          else sb.append("") // sb.append(new String(origInputChars, mods0.last.pos.end.offset, orig.pos.end.offset - mods0.last.pos.end.offset))
        }

        if (name0 ne name1) sb.append(
          new String(origInputChars, mods0.last.pos.end.offset, name0.pos.start.offset - mods0.last.pos.end.offset) +
            (name1)
        )
        else sb.append("")

        if (templ0 ne templ1) sb.append(
          new String(origInputChars, name0.pos.end.offset, templ0.pos.start.offset - name0.pos.end.offset) +
            (templ1)
            // new String(origInputChars, templ0.pos.end.offset, orig.pos.end.offset - templ0.pos.end.offset)
        )
        else sb.append("")

        s(sb.toString)
      case _ => s("$hole")
    }
  }
}
