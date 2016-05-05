// RUN: %target-run-simple-swift
// REQUIRES: executable_test

func setAlgebraOps<S : SetAlgebra>(_ s: S) {}

setAlgebraOps(Set<Int>())
