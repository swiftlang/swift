// This test is paired with testable-multifile.swift.

// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -emit-module %S/Inputs/TestableMultifileHelper.swift -enable-testing -o %t

// RUN: %target-swift-frontend -emit-silgen -I %t %s %S/testable-multifile.swift -module-name main | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen -I %t %S/testable-multifile.swift %s -module-name main | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen -I %t -primary-file %s %S/testable-multifile.swift -module-name main | %FileCheck %s

// Just make sure we don't crash later on.
// RUN: %target-swift-frontend -emit-ir -I %t -primary-file %s %S/testable-multifile.swift -module-name main -o /dev/null
// RUN: %target-swift-frontend -emit-ir -I %t -O -primary-file %s %S/testable-multifile.swift -module-name main -o /dev/null

func use<F: Fooable>(_ f: F) { f.foo() }
func test(internalFoo: FooImpl, publicFoo: PublicFooImpl) {
  use(internalFoo)
  use(publicFoo)

  internalFoo.foo()
  publicFoo.foo()
}

// CHECK-LABEL: sil hidden @_T04main4testyAA7FooImplV08internalC0_AA06PubliccD0V06publicC0tF
// CHECK: [[USE_1:%.+]] = function_ref @_T04main3useyxAA7FooableRzlF
// CHECK: = apply [[USE_1]]<FooImpl>({{%.+}}) : $@convention(thin) <τ_0_0 where τ_0_0 : Fooable> (@in τ_0_0) -> ()
// CHECK: [[USE_2:%.+]] = function_ref @_T04main3useyxAA7FooableRzlF
// CHECK: = apply [[USE_2]]<PublicFooImpl>({{%.+}}) : $@convention(thin) <τ_0_0 where τ_0_0 : Fooable> (@in τ_0_0) -> ()
// CHECK: [[IMPL_1:%.+]] = function_ref @_T023TestableMultifileHelper13HasDefaultFooPAAE3fooyyF
// CHECK: = apply [[IMPL_1]]<FooImpl>({{%.+}}) : $@convention(method) <τ_0_0 where τ_0_0 : HasDefaultFoo> (@in_guaranteed τ_0_0) -> ()
// CHECK: [[IMPL_2:%.+]] = function_ref @_T023TestableMultifileHelper13HasDefaultFooPAAE3fooyyF
// CHECK: = apply [[IMPL_2]]<PublicFooImpl>({{%.+}}) : $@convention(method) <τ_0_0 where τ_0_0 : HasDefaultFoo> (@in_guaranteed τ_0_0) -> ()
// CHECK: } // end sil function '_T04main4testyAA7FooImplV08internalC0_AA06PubliccD0V06publicC0tF'

func test(internalSub: Sub, publicSub: PublicSub) {
  internalSub.foo()
  publicSub.foo()
}

// CHECK-LABEL: sil hidden @_T04main4testyAA3SubC08internalC0_AA06PublicC0C06publicC0tF
// CHECK: bb0([[ARG0:%.*]] : $Sub, [[ARG1:%.*]] : $PublicSub):
// CHECK: [[BORROWED_ARG0:%.*]] = begin_borrow [[ARG0]]
// CHECK: = class_method [[BORROWED_ARG0]] : $Sub, #Sub.foo!1
// CHECK: [[BORROWED_ARG1:%.*]] = begin_borrow [[ARG1]]
// CHECK: = class_method [[BORROWED_ARG1]] : $PublicSub, #PublicSub.foo!1
// CHECK: } // end sil function '_T04main4testyAA3SubC08internalC0_AA06PublicC0C06publicC0tF'

