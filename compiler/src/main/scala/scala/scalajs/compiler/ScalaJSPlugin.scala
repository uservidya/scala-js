/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.compiler

import scala.tools.nsc._
import scala.tools.nsc.plugins.{
  Plugin => NscPlugin, PluginComponent => NscPluginComponent
}
import scala.collection.{ mutable, immutable }

/** Main entry point for the Scala.js compiler plugin
 *
 *  @author Sébastien Doeraene
 */
class ScalaJSPlugin(val global: Global) extends NscPlugin {
  import global._

  val name = "scalajs"
  val description = "Compile to JavaScript"
  val components = List[NscPluginComponent](PluginComponent)

  /** Addons for JavaScript platform */
  object jsAddons extends {
    val global: ScalaJSPlugin.this.global.type = ScalaJSPlugin.this.global
  } with JSGlobalAddons

  object PluginComponent extends {
    val global: ScalaJSPlugin.this.global.type = ScalaJSPlugin.this.global
    val jsAddons: ScalaJSPlugin.this.jsAddons.type = ScalaJSPlugin.this.jsAddons
    override val runsAfter = List("mixin")
    override val runsBefore = List("delambdafy", "cleanup", "terminal")
  } with GenJSCode
}
