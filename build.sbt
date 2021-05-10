/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

lazy val silicon = project
  .in(file("silicon"))

lazy val inference = project
  .in(file("."))
  .dependsOn(silicon)
  .settings(
    // general settings
    name := "inference",
    version := "0",

    // dependencies
    libraryDependencies += "org.rogach" %% "scallop" % "4.0.2",

    // jvm options
    fork := true,
    run / javaOptions += "-Xss128m",
  )
