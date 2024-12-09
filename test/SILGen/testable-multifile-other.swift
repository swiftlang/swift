
// This test is paired with testable-multifile.swift.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/TestableMultifileHelper.swift -enable-testing -o %t

// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -I %t %s %S/testable-multifile.swift -module-name main | %FileCheck %s
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -I %t %S/testable-multifile.swift %s -module-name main | %FileCheck %s
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -I %t -primary-file %s %S/testable-multifile.swift -module-name main | %FileCheck %s

// Just make sure we don't crash later on.
// RUN: %target-swift-emit-ir -I %t -primary-file %s %S/testable-multifile.swift -module-name main -o /dev/null
// RUN: %target-swift-emit-ir -I %t -O -primary-file %s %S/testable-multifile.swift -module-name main -o /dev/null

@testable import TestableMultifileHelper

func use<F: Fooable>(_ f: F) { f.foo() }
func test(internalFoo: FooImpl, publicFoo: PublicFooImpl) {
  use(internalFoo)
  use(publicFoo)

  internalFoo.foo()
  publicFoo.foo()
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4test11internalFoo06publicD0yAA0D4ImplV_AA06PublicdF0VtF
// CHECK: [[USE_1:%.+]] = function_ref @$s4main3useyyxAA7FooableRzlF
// CHECK: = apply [[USE_1]]<FooImpl>({{%.+}}) : $@convention(thin) <τ_0_0 where τ_0_0 : Fooable> (@in_guaranteed τ_0_0) -> ()
// CHECK: [[USE_2:%.+]] = function_ref @$s4main3useyyxAA7FooableRzlF
// CHECK: = apply [[USE_2]]<PublicFooImpl>({{%.+}}) : $@convention(thin) <τ_0_0 where τ_0_0 : Fooable> (@in_guaranteed τ_0_0) -> ()
// CHECK: [[IMPL_1:%.+]] = function_ref @$s23TestableMultifileHelper13HasDefaultFooPAAE3fooyyF
// CHECK: = apply [[IMPL_1]]<FooImpl>({{%.+}}) : $@convention(method) <τ_0_0 where τ_0_0 : HasDefaultFoo> (@in_guaranteed τ_0_0) -> ()
// CHECK: [[IMPL_2:%.+]] = function_ref @$s23TestableMultifileHelper13HasDefaultFooPAAE3fooyyF
// CHECK: = apply [[IMPL_2]]<PublicFooImpl>({{%.+}}) : $@convention(method) <τ_0_0 where τ_0_0 : HasDefaultFoo> (@in_guaranteed τ_0_0) -> ()
// CHECK: } // end sil function '$s4main4test11internalFoo06publicD0yAA0D4ImplV_AA06PublicdF0VtF'

func test(internalSub: Sub, publicSub: PublicSub) {
  internalSub.foo()
  publicSub.foo()
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4test11internalSub06publicD0yAA0D0C_AA06PublicD0CtF
// CHECK: bb0([[ARG0:%.*]] : @guaranteed $Sub, [[ARG1:%.*]] : @guaranteed $PublicSub):
// CHECK: = class_method [[ARG0]] : $Sub, #Sub.foo :
// CHECK: = class_method [[ARG1]] : $PublicSub, #PublicSub.foo :
// CHECK: } // end sil function '$s4main4test11internalSub06publicD0yAA0D0C_AA06PublicD0CtF'

