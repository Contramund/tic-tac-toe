addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.9.15")
addSbtPlugin("org.scalameta" %% "sbt-scalafmt" % "2.4.5")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.8")
addSbtPlugin("org.typelevel" % "sbt-tpolecat" % "0.5.0")
addCompilerPlugin(("org.scalamacros" % "paradise" % "2.1.1").cross(CrossVersion.full))
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
addCompilerPlugin(("org.typelevel" % "kind-projector" % "0.13.2").cross(CrossVersion.full))
