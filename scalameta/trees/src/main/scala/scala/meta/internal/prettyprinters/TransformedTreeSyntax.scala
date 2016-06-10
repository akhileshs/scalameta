package scala.meta
package internal
package prettyprinters

import scala.meta.prettyprinters._
import Show.{ sequence => s }

object TransformedTreeSyntax {
  def printTransformedTree[T <: Tree](orig: T, transformed: T): Show.Result = {
    val origInputChars = orig.pos.input.chars
    val origPosStart = orig.pos.start.offset
    val origPosEnd = orig.pos.end.offset
    val sb = new StringBuilder
    var pos = origPosStart
    def appendResultTree(sb: StringBuilder, t1: Tree, t2: Tree): Unit = {       
      sb.appendAll(origInputChars, pos, t1.pos.start.offset - pos)
      (t1, t2) match {
        case (Name.Indeterminate(a0), Name.Indeterminate(a1)) =>
          sb.append(a1)
        case (Term.Name(a0), Term.Name(a1)) =>
          sb.append(a1)
        case (Type.Name(a0), Type.Name(a1)) =>
          sb.append(a1)         
        case (Lit(a0), Lit(a1)) =>                    
          sb.append(a1)
        case _ =>
          sb.append(t2)
      }
      pos = t1.pos.end.offset     // pos only gets updated once over here
    }     

    def appendResultSeqTree(sb: StringBuilder, t1: Seq[Any], t2: Seq[Any]): Unit = {
      if (t1.isEmpty) {}
      else {
        for ((x0, x1) <- (t1 zip t2)) {
          (x0, x1) match {
            case (y0: Tree, y1: Tree) => appendResultTree(sb, y0, y1)
            case (y0: Seq[_], y1: Seq[_]) => appendResultSeqTree(sb, y0, y1)
            case (y0: Option[_], y1: Option[_]) => appendResultSeqTree(sb, y0.toList, y1.toList)
            case _ => {}
          }
        }
      }
    }    
     
    def appendRemainder(sb: StringBuilder): Unit = sb.appendAll(origInputChars, pos, origPosEnd - pos)    

    val l1 = orig.productIterator.toList
    val l2 = transformed.productIterator.toList

    (l1 zip l2) foreach {
      /* put weird cases here first */
      case (Lit(()), Lit(())) => {}        
      case (Ctor.Primary(mods0, _, paramss0), Ctor.Primary(mods1, _, paramss1)) =>
        appendResultSeqTree(sb, mods0, mods1)
        appendResultSeqTree(sb, paramss0, paramss1)      
      case (x: Tree, y: Tree) =>
        appendResultTree(sb, x, y)                
      case (x: Seq[_], y: Seq[_])  =>
        appendResultSeqTree(sb, x, y)
      case (x: Option[_], y: Option[_]) =>
        appendResultSeqTree(sb, x.toList, y.toList)      
      case _ => {}
    }
           
    appendRemainder(sb)    
    s(sb.toString)    
  }
}
