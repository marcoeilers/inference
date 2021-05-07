# Inference

This inference is work in progress. A description will be added once the work is more complete.

## Dependencies

 * Java 11 or newer (64 bit).
 * Although the inference can be compiled in other ways, the build instructions below assume that a recent version of [sbt](https://www.scala-sbt.org/) is installed.
 * An executable of the [Z3](https://github.com/Z3Prover/z3/releases) SMT solver.

## Build Instructions

 * Clone this repository as well as the [Silver](https://github.com/viperproject/silver) and [Silicon](https://github.com/viperproject/silicon) repositories into the same parent directory.
 * Switch to the Silicon directory and create a symbolic link to the Silver directory:
    * On Linux or Mac, run `ln -s ../silver silver`.
    * On Windows, run `mklink /D silver ..\silver`.
 * Switch to the inference directory and create symbolic links to the Silver and the Silicon repositories.
 * Compile by running `sbt compile`.
