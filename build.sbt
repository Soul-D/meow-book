name := "cats-book"

val scalazVersion = "7.2.17"
val catsVersion = "1.0.0-MF"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-laws" % catsVersion,
  "org.typelevel" %% "cats-effect" % "0.5",
  "org.typelevel" %% "discipline" % "0.8",
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-concurrent" % scalazVersion
)

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-Xfatal-warnings",
  "-Ypartial-unification",
  "-language:higherKinds"
)

