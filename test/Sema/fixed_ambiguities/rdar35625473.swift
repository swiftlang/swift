// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

// CHECK: function_ref @_T0s10CollectionPsE9dropFirst11SubSequenceQzSiF : $@convention(method) <τ_0_0 where τ_0_0 : Collection> (Int, @in_guaranteed τ_0_0) -> @out τ_0_0.SubSequence
_ = [1, 2, 3].dropFirst(1).dropFirst(1)
