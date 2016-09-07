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

// CHECK-LABEL: sil hidden @_TF4main4testFT11internalFooVS_7FooImpl9publicFooVS_13PublicFooImpl_T_
// CHECK: [[USE_1:%.+]] = function_ref @_TF4main3useuRxS_7FooablerFxT_
// CHECK: = apply [[USE_1]]<FooImpl>({{%.+}}) : $@convention(thin) <τ_0_0 where τ_0_0 : Fooable> (@in τ_0_0) -> ()
// CHECK: [[USE_2:%.+]] = function_ref @_TF4main3useuRxS_7FooablerFxT_
// CHECK: = apply [[USE_2]]<PublicFooImpl>({{%.+}}) : $@convention(thin) <τ_0_0 where τ_0_0 : Fooable> (@in τ_0_0) -> ()
// CHECK: [[IMPL_1:%.+]] = function_ref @_TFE23TestableMultifileHelperPS_13HasDefaultFoo3foofT_T_
// CHECK: = apply [[IMPL_1]]<FooImpl>({{%.+}}) : $@convention(method) <τ_0_0 where τ_0_0 : HasDefaultFoo> (@in_guaranteed τ_0_0) -> ()
// CHECK: [[IMPL_2:%.+]] = function_ref @_TFE23TestableMultifileHelperPS_13HasDefaultFoo3foofT_T_
// CHECK: = apply [[IMPL_2]]<PublicFooImpl>({{%.+}}) : $@convention(method) <τ_0_0 where τ_0_0 : HasDefaultFoo> (@in_guaranteed τ_0_0) -> ()
// CHECK: {{^}$}}

func test(internalSub: Sub, publicSub: PublicSub) {
  internalSub.foo()
  publicSub.foo()
}

// CHECK-LABEL: sil hidden @_TF4main4testFT11internalSubCS_3Sub9publicSubCS_9PublicSub_T_
// CHECK: = class_method %0 : $Sub, #Sub.foo!1
// CHECK: = class_method %1 : $PublicSub, #PublicSub.foo!1
// CHECK: {{^}$}}

