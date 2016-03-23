// RUN: %target-run-simple-swift
// REQUIRES: executable_test

func setAlgebraOps<S : SetAlgebra>(s: S) {}

setAlgebraOps(Set<Int>())
