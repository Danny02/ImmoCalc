import org.scalajs.core.tools.linker.ModuleInitializer

turbo := true

scalaVersion := "2.12.9"

enablePlugins(WorkbenchPlugin)
workbenchStartMode := WorkbenchStartModes.OnCompile

enablePlugins(ScalaJSPlugin)
scalaJSModuleInitializers += ModuleInitializer.mainMethodWithArgs("de.dheinrich.VisApp", "main")

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom"  % "0.9.7",
  "com.lihaoyi"  %%% "scalatags"    % "0.7.0",
  "com.lihaoyi"  %%% "upickle"      % "0.7.5",
  "com.lihaoyi"  %%% "scalarx"      % "0.4.0",
  "com.timushev" %%% "scalatags-rx" % "0.4.0"
)
