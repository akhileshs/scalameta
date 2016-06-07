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
    def appendResultTree(sb: StringBuilder, t1: Tree, t2: Tree) = {
      sb.appendAll(origInputChars, pos, t1.pos.start.offset - pos)      
      sb.append(t2)      
      pos = t1.pos.end.offset      
    }    

    def appendResultSeqTree(sb: StringBuilder, t1: Seq[Tree], t2: Seq[Tree]) = {
      if (t1.isEmpty) sb
      else {        
        for ((x0, x1) <- (t1 zip t2)) {          
          appendResultTree(sb, x0, x1)         
        }        
        appendRemainder(sb)        
      }      
    }   
     
    def appendRemainder(sb: StringBuilder) = {
      sb.appendAll(origInputChars, pos, orig.pos.end.offset - pos)      
    }       

    val l1 = orig.productIterator.toList
    val l2 = transformed.productIterator.toList
    
    (l1 zip l2) foreach {      
      case (x: Tree, y: Tree) =>
        appendResultTree(sb, x, y)
        /*
      case (x: Seq[Tree], y: Seq[Tree]) =>
        appendResultSeqTree(sb, x, y)
         */
      case _ =>
        "handle other cases here"
    }   
    
    (orig, transformed) match {
      case (Term.Name(a0), Term.Name(a1)) =>
        if (a0 ne a1) sb.append(a1)
        else sb.append(a0)
      case (Lit(a0), Lit(a1)) =>
        if (a0 != a1) sb.append(a1)
        else sb.append(a0)
      case _ => "handle other cases here"
    }    

    s(sb.toString)    
  }
}
