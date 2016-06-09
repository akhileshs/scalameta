package scala.meta
package internal
package prettyprinters

import scala.meta.prettyprinters._
import Show.{ sequence => s }

object TransformedTreeSyntax {
  def printTransformedTree[T <: Tree](orig: T, transformed: T): Show.Result = {
    val origInputChars = orig.pos.input.chars
    val origPosStart = orig.pos.start.offset
    val sb = new StringBuilder
    var pos = origPosStart
    def appendResultTree(sb: StringBuilder, t1: Tree, t2: Tree): Unit = {
      // this is hack. I don't know how to handle the Lit() case yet.
      t1 match {
        case Lit(a0) =>
          if (a0.toString eq "()") {
            appendRemainder(sb)
          }
          else sb.appendAll(origInputChars, pos, t1.pos.start.offset - pos)
        case _ =>
          sb.appendAll(origInputChars, pos, t1.pos.start.offset - pos)
      }
      // sb.appendAll(origInputChars, pos, t1.pos.start.offset - pos)
      (t1, t2) match {
        case (Term.Name(a0), Term.Name(a1)) =>
          sb.append(a1)
        case (Type.Name(a0), Type.Name(a1)) =>
          sb.append(a1)
        case (Lit(a0), Lit(a1)) =>
          if (a0.toString eq "()") { } // not sure how to deal with this case.
          else sb.append(a1)
        case _ =>
          sb.append(t2)
      }
      pos = t1.pos.end.offset     
    }     

    def appendResultSeqTree(sb: StringBuilder, t1: Seq[Any], t2: Seq[Any]): Unit = {
      if (t1.isEmpty) {}
      else {
        for ((x0, x1) <- (t1 zip t2)) {
          (x0, x1) match {
            case (y0: Tree, y1: Tree) => appendResultTree(sb, y0, y1)
            case (y0: Seq[_], y1: Seq[_]) => appendResultSeqTree(sb, y0, y1)
            case (y0: Option[_], y1: Option[_]) => appendResultOptionTree(sb, y0, y1)
            case _ => {}
          }
        }
      }
    }

    def appendResultOptionTree(sb: StringBuilder, t1: Option[Any], t2: Option[Any]): Unit = {
      (t1, t2) match {
        case (Some(x0), Some(x1)) =>
          (x0, x1) match {
            case (y0: Tree, y1: Tree) => appendResultTree(sb, y0, y1)
            case (y0: Seq[_], y1: Seq[_]) => appendResultSeqTree(sb, y0, y1)
            case (y0: Option[_], y1: Option[_]) => appendResultOptionTree(sb, y0, y1)
            case _ => {}
          }
        case _ => {}
      }
    }
     
    def appendRemainder(sb: StringBuilder): Unit = {      
      if (orig.pos.end.offset - pos <= 0) {}       
      else sb.appendAll(origInputChars, pos, orig.pos.end.offset - pos)      
    }           

    val l1 = orig.productIterator.toList
    val l2 = transformed.productIterator.toList

    (l1 zip l2) foreach {
      /* put weird cases here first */      
      case (Ctor.Primary(mods0, _, paramss0), Ctor.Primary(mods1, _, paramss1)) =>
        appendResultSeqTree(sb, mods0, mods1)
        appendResultSeqTree(sb, paramss0, paramss1)      
      case (x: Tree, y: Tree) =>
        appendResultTree(sb, x, y)                
      case (x: Seq[_], y: Seq[_])  =>
        appendResultSeqTree(sb, x, y)
      case (x: Option[_], y: Option[_]) =>
        appendResultOptionTree(sb, x, y)      
      case _ =>
        {}
    }   
    appendRemainder(sb)    
    s(sb.toString)    
  }
}
