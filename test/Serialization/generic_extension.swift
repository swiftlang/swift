// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/generic_extension_1.swift
// RUN: %target-swift-frontend -emit-sil -I %t %s | FileCheck %s

// We have to perform IRGen to actually check that the generic substitutions
// are being used.

import generic_extension_1

["a", "b", "c"].wobble()

// CHECK: sil @_TFE19generic_extension_1Sa6wobbleU__fGSaQ__FT_GSqQ__ : $@cc(method) @thin <τ_0_0> (@out Optional<τ_0_0>, @guaranteed Array<τ_0_0>) -> ()
