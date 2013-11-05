package scala.scalajs.sbtplugin

import com.google.javascript.jscomp
import jscomp._
import com.google.javascript.rhino._

import scala.collection.JavaConversions.iterableAsScalaIterable

class ScalaJSClosurePass(compiler: AbstractCompiler) extends CompilerPass {
  import ScalaJSClosurePass._

  def process(externs: Node, root: Node): Unit = {
    if (root ne null)
      processRoot(root)
  }

  def processRoot(root: Node): Unit = {
    for (node <- root.children)
      processScript(node)
  }

  def processScript(script: Node): Unit = {
    script transformChildren {
      case node @ Block(_*) =>
        processScript(node)
        Seq(node)

      case exprStat @ ExprStmt(
          call @ Call(QualifiedName("ScalaJS.extend"),
              extendee, extender @ ObjectLit(keys @ _*))) =>
        for {
          key @ ObjectLitStringKey(value) <- keys
          if !key.isQuotedString
        } yield {
          val prop = Node.newString(key.getString) withInfo key
          val getProp = GetProp(extendee.cloneTree(), prop) withInfo key
          value.detachFromParent()
          ExprStmt(Assign(getProp, value) withInfo key) withInfo key
        }
    }
  }
}

object ScalaJSClosurePass {
  implicit class NodeOps(val self: Node) extends AnyVal {
    def withInfo(source: Node): Node = {
      self.copyInformationFrom(source)
    }

    def transformChildren(f: PartialFunction[Node, Seq[Node]]): Unit = {
      var prevNode: Node = null
      var nextNode = self.getFirstChild
      while (nextNode ne null) {
        val node = nextNode
        nextNode = node.getNext

        f.applyOrElse(node, (_: Node) => Seq(node)) match {
          case Seq(`node`) =>
            prevNode = node

          case Seq() =>
            node.detachFromParent()
            // do not change `prevNode`

          case newNodes =>
            node.detachFromParent()
            for (newNode <- newNodes if newNode.getParent ne null)
              newNode.detachFromParent()
            for (newNode <- newNodes.reverse)
              if (prevNode eq null) self.addChildToFront(newNode)
              else self.addChildAfter(newNode, prevNode)
            prevNode = newNodes.last
        }
      }
    }
  }

  class NodeExtractor(token: Int) {
    def apply(children: Node*): Node = {
      new Node(token, children.toArray)
    }

    def unapplySeq(node: Node): Option[Seq[Node]] = {
      if (node.getType == token) Some(node.children.toSeq)
      else None
    }
  }

  val Block = new NodeExtractor(Token.BLOCK)
  val Call = new NodeExtractor(Token.CALL)
  val Assign = new NodeExtractor(Token.ASSIGN)
  val ExprStmt = new NodeExtractor(Token.EXPR_RESULT)
  val GetProp = new NodeExtractor(Token.GETPROP)
  val ObjectLit = new NodeExtractor(Token.OBJECTLIT)
  val ObjectLitStringKey = new NodeExtractor(Token.STRING_KEY)

  object QualifiedName {
    def unapply(node: Node): Option[String] = {
      Option(node.getQualifiedName)
    }
  }
}
