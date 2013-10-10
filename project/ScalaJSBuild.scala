import sbt._
import Keys._

import ch.epfl.lamp.sbtscalajs._
import ScalaJSPlugin._
import ScalaJSKeys._
import SourceMapCat.catJSFilesAndTheirSourceMaps

object ScalaJSBuild extends Build {

  val scalajsScalaVersion = "2.11.0-M5"

  val commonSettings = Defaults.defaultSettings ++ Seq(
      organization := "ch.epfl.lamp",
      version := "0.1-SNAPSHOT",

      normalizedName ~= {
        _.replace("scala.js", "scalajs").replace("scala-js", "scalajs")
      }
  )

  val defaultSettings = commonSettings ++ Seq(
      scalaVersion := scalajsScalaVersion,
      scalacOptions ++= Seq(
          "-deprecation",
          "-unchecked",
          "-feature",
          "-encoding", "utf8"
      )
  )

  val myScalaJSSettings = scalaJSAbstractSettings ++ Seq(
      autoCompilerPlugins := true
  )

  // Used when compiling the compiler, adding it to scalacOptions does not help
  scala.util.Properties.setProp("scalac.patmat.analysisBudget", "1024")

  lazy val root: Project = Project(
      id = "scalajs",
      base = file("."),
      settings = defaultSettings ++ Seq(
          name := "Scala.js",
          publishArtifact in Compile := false,

          clean := clean.dependsOn(
              // compiler, library and sbt-plugin are aggregated
              clean in corejslib, clean in javalib, clean in scalalib,
              clean in libraryAux, clean in examples,
              clean in exampleHelloWorld, clean in exampleReversi).value
      )
  ).aggregate(
      compiler, plugin, library
  )

  lazy val compiler: Project = Project(
      id = "scalajs-compiler",
      base = file("compiler"),
      settings = defaultSettings ++ Seq(
          name := "Scala.js compiler",
          libraryDependencies ++= Seq(
              "org.scala-lang" % "scala-compiler" % scalajsScalaVersion,
              "org.scala-lang" % "scala-reflect" % scalajsScalaVersion
          ),
          mainClass := Some("scala.tools.nsc.scalajs.Main"),
          exportJars := true
      )
  )

  lazy val plugin: Project = Project(
      id = "scalajs-sbt-plugin",
      base = file("sbt-plugin"),
      settings = commonSettings ++ Seq(
          name := "Scala.js sbt plugin",
          sbtPlugin := true,
          scalaBinaryVersion :=
            CrossVersion.binaryScalaVersion(scalaVersion.value),
          libraryDependencies ++= Seq(
              "com.google.javascript" % "closure-compiler" % "v20130603",
              "org.mozilla" % "rhino" % "1.7R4"
          )
      )
  )

  lazy val corejslib: Project = Project(
      id = "scalajs-corejslib",
      base = file("corejslib"),
      settings = defaultSettings ++ Seq(
          name := "Scala.js core JS runtime",
          publishArtifact in Compile := false,

          packageJS in Compile := {
            val s = streams.value
            val targetDir = (target in Compile).value

            // hard-coded because order matters!
            val fileNames =
              Seq("scalajsenv.js", "javalangObject.js",
                  "javalangString.js", "DummyParents.js")

            val allJSFiles = fileNames map (baseDirectory.value / _)
            val output = targetDir / ("scalajs-corejslib.js")

            FileFunction.cached(s.cacheDirectory / "package-js",
                FilesInfo.lastModified, FilesInfo.exists) { dependencies =>
              targetDir.mkdir()
              catJSFilesAndTheirSourceMaps(allJSFiles, output)
              Set(output)
            } (allJSFiles.toSet)

            Seq(output)
          }
      )
  )

  lazy val javalib: Project = Project(
      id = "scalajs-javalib",
      base = file("javalib"),
      settings = defaultSettings ++ myScalaJSSettings ++ Seq(
          name := "Java library for Scala.js",
          publishArtifact in Compile := false,
          scalacOptions += "-Yskip:cleanup,icode,jvm"
      ) ++ (
          scalaJSExternalCompileSettings
      )
  ).dependsOn(compiler % "plugin", library)

  lazy val scalalib: Project = Project(
      id = "scalajs-scalalib",
      base = file("scalalib"),
      settings = defaultSettings ++ myScalaJSSettings ++ Seq(
          name := "Scala library for Scala.js",
          publishArtifact in Compile := false,

          // The Scala lib is full of warnings we don't want to see
          scalacOptions ~= (_.filterNot(
              Set("-deprecation", "-unchecked", "-feature") contains _)),

          // Do not generate .class files
          scalacOptions += "-Yskip:cleanup,icode,jvm",

          // Exclude files that are overridden in library
          excludeFilter in (Compile, unmanagedSources) ~= { superFilter =>
            superFilter || new SimpleFileFilter({ f =>
              val path = f.getPath.replace(java.io.File.separator, "/")
              (path.endsWith("/scala/package.scala")
                  || path.endsWith("/scala/App.scala")
                  || path.endsWith("/scala/Console.scala")
                  || path.endsWith("/scala/compat/Platform.scala")
                  || path.endsWith("/scala/runtime/BoxesRunTime.scala"))
            })
          },

          // Continuation plugin
          autoCompilerPlugins := true,
          libraryDependencies += compilerPlugin(
              "org.scala-lang.plugins" % "continuations" % scalaVersion.value),
          scalacOptions += "-P:continuations:enable"
      ) ++ (
          scalaJSExternalCompileSettings
      )
  ).dependsOn(compiler % "plugin")

  lazy val libraryAux: Project = Project(
      id = "scalajs-library-aux",
      base = file("library-aux"),
      settings = defaultSettings ++ myScalaJSSettings ++ Seq(
          name := "Scala.js aux library",
          publishArtifact in Compile := false,
          scalacOptions += "-Yskip:cleanup,icode,jvm"
      ) ++ (
          scalaJSExternalCompileSettings
      )
  ).dependsOn(compiler % "plugin")

  lazy val library: Project = Project(
      id = "scalajs-library",
      base = file("library"),
      settings = defaultSettings ++ myScalaJSSettings ++ Seq(
          name := "Scala.js library"
      ) ++ (
          scalaJSExternalCompileSettings
      ) ++ inConfig(Compile)(Seq(
          /* Add the .js and .js.map files from other lib projects
           * (but not .jstype files)
           */
          mappings in packageBin ++= {
            val allProducts = (
                (products in javalib).value ++
                (products in scalalib).value ++
                (products in libraryAux).value)
            val filter = ("*.js": NameFilter) | "*.js.map"
            allProducts.flatMap(dir => (dir ** filter) x relativeTo(dir))
          },

          // Add the core JS library
          mappings in packageBin +=
            (packageJS in corejslib).value.head -> "scalajs-corejslib.js"
      ))
  ).dependsOn(compiler % "plugin")

  // Examples

  lazy val examples: Project = Project(
      id = "examples",
      base = file("examples"),
      settings = defaultSettings ++ Seq(
          name := "Scala.js examples"
      )
  ).aggregate(exampleHelloWorld, exampleReversi)

  lazy val exampleSettings = defaultSettings ++ myScalaJSSettings ++ Seq(
      autoCompilerPlugins := true,

      /* Add the library classpath this way to escape the dependency between
       * tasks. This avoids to recompile the library every time we make a
       * change in the compiler, and we want to test it on an example.
       */
      unmanagedClasspath in Compile += {
        val libraryJar = (artifactPath in (library, Compile, packageBin)).value
        Attributed.blank(libraryJar)
      },

      // Add the startup.js file of this example project
      unmanagedSources in (Compile, packageJS) +=
        baseDirectory.value / "startup.js"
  )

  lazy val exampleHelloWorld = Project(
      id = "helloworld",
      base = file("examples") / "helloworld",
      settings = exampleSettings ++ Seq(
          name := "Hello World - Scala.js example",
          moduleName := "helloworld"
      )
  ).dependsOn(compiler % "plugin")

  lazy val exampleReversi = Project(
      id = "reversi",
      base = file("examples") / "reversi",
      settings = exampleSettings ++ Seq(
          name := "Reversi - Scala.js example",
          moduleName := "reversi"
      )
  ).dependsOn(compiler % "plugin")

  // Testing

  lazy val partest: Project = Project(
      id = "scalajs-partest",
      base = file("partest"),
      settings = defaultSettings ++ Seq(
          name := "Partest for Scala.js",
          moduleName := "scalajs-partest",

          resolvers += Resolver.typesafeIvyRepo("releases"),

          libraryDependencies ++= Seq(
              "org.scala-sbt" % "sbt" % "0.13.0",
              "org.scala-lang.modules" % "scala-partest_2.11.0-M4" % "1.0-RC2",
              "org.mozilla" % "rhino" % "1.7R4"
          ),

          sources in Compile += (
              (baseDirectory in plugin).value /
              "src/main/scala/ch/epfl/lamp/sbtscalajs/RhinoBasedRun.scala")
      )
  ).dependsOn(compiler)

  lazy val testSuite: Project = Project(
      id = "scalajs-testsuite",
      base = file("test"),
      settings = defaultSettings ++ Seq(
          name := "Scala.js test suite",

          /* Add an extracted version of scalajs-library.jar on the classpath.
           * The runner will need it, as it cannot cope with .js files in .jar.
           */
          dependencyClasspath in Test += {
            val s = streams.value

            val taskCacheDir = s.cacheDirectory / "extract-scalajs-library"
            val extractDir = taskCacheDir / "scalajs-library"

            val libraryJar =
              (artifactPath in (library, Compile, packageBin)).value

            val cachedExtractJar = FileFunction.cached(taskCacheDir / "cache-info",
                FilesInfo.lastModified, FilesInfo.exists) { dependencies =>

              val usefulFilesFilter = ("*.js": NameFilter) | ("*.js.map")
              s.log.info("Extracting %s ..." format libraryJar)
              if (extractDir.exists)
                IO.delete(extractDir)
              IO.createDirectory(extractDir)
              IO.unzip(libraryJar, extractDir, filter = usefulFilesFilter,
                  preserveLastModified = true)
              (extractDir ** usefulFilesFilter).get.toSet
            }

            cachedExtractJar(Set(libraryJar))

            Attributed.blank(extractDir)
          },

          fork in Test := true,
          javaOptions in Test += "-Xmx1G",

          testFrameworks +=
            new TestFramework("scala.tools.partest.scalajs.Framework"),

          definedTests in Test +=
            new sbt.TestDefinition(
                "partest",
                // marker fingerprint since there are no test classes
                // to be discovered by sbt:
                new sbt.testing.AnnotatedFingerprint {
                  def isModule = true
                  def annotationName = "partest"
                },
                true,
                Array()
            )
      )
  ).dependsOn(partest % "test")
}
