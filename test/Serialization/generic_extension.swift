// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/generic_extension_1.swift
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-sil -I %t %s | %FileCheck %s

// We have to perform IRGen to actually check that the generic substitutions
// are being used.

import generic_extension_1

["a", "b", "c"].wobble()

// CHECK: sil @_T0Sa19generic_extension_1E6wobble{{[_0-9a-zA-Z]*}}F : $@convention(method) <τ_0_0> (@guaranteed Array<τ_0_0>) -> @out Optional<τ_0_0>

func useP<T: P>(_ t: T) -> Int { return t.property }

func testUseP(c: ConformsToP) {
  _ = useP(c)
}
