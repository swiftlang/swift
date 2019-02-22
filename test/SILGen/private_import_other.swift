// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -module-name Mod -emit-module -enable-private-imports -enable-sil-ownership -swift-version 5 -o %t %S/Inputs/private_import_module.swift
// RUN: %target-swift-emit-silgen -enable-sil-ownership -I %t -primary-file %s %S/private_import.swift -module-name main -swift-version 5 | %FileCheck %s
// RUN: %target-swift-emit-silgen -enable-sil-ownership -I %t %s %S/private_import.swift -module-name main -swift-version 5 | %FileCheck %s
// RUN: %target-swift-emit-silgen -enable-sil-ownership -I %t %S/private_import.swift %s -module-name main -swift-version 5 | %FileCheck %s
// RUN: %target-swift-emit-ir -enable-sil-ownership -I %t -primary-file %s %S/private_import.swift -module-name main -o /dev/null
// RUN: %target-swift-emit-ir -enable-sil-ownership -I %t -O -primary-file %s %S/private_import.swift -module-name main -o /dev/null

@_private(sourceFile: "private_import_module.swift") import Mod

func use<F: Fooable>(_ f: F) { f.foo() }
func test(internalFoo: FooImpl, publicFoo: PublicFooImpl) {
  use(internalFoo)
  use(publicFoo)

  internalFoo.foo()
  publicFoo.foo()
}

// CHECK-LABEL: sil hidden @$s4main4test11internalFoo06publicD0yAA0D4ImplV_AA06PublicdF0VtF
// CHECK: [[USE_1:%.+]] = function_ref @$s4main3useyyxAA7FooableRzlF
// CHECK: = apply [[USE_1]]<FooImpl>({{%.+}}) : $@convention(thin) <τ_0_0 where τ_0_0 : Fooable> (@in_guaranteed τ_0_0) -> ()
// CHECK: [[USE_2:%.+]] = function_ref @$s4main3useyyxAA7FooableRzlF
// CHECK: = apply [[USE_2]]<PublicFooImpl>({{%.+}}) : $@convention(thin) <τ_0_0 where τ_0_0 : Fooable> (@in_guaranteed τ_0_0) -> ()
// CHECK: [[IMPL_1:%.+]] = function_ref @$s3Mod13HasDefaultFoo33_{{.*}}PAAE3fooyyF
// CHECK: = apply [[IMPL_1]]<FooImpl>({{%.+}}) : $@convention(method) <τ_0_0 where τ_0_0 : HasDefaultFoo> (@in_guaranteed τ_0_0) -> ()
// CHECK: [[IMPL_2:%.+]] = function_ref @$s3Mod13HasDefaultFoo33_{{.*}}PAAE3fooyyF
// CHECK: = apply [[IMPL_2]]<PublicFooImpl>({{%.+}}) : $@convention(method) <τ_0_0 where τ_0_0 : HasDefaultFoo> (@in_guaranteed τ_0_0) -> ()
// CHECK: } // end sil function '$s4main4test11internalFoo06publicD0yAA0D4ImplV_AA06PublicdF0VtF'

func test(internalSub: Sub, publicSub: PublicSub) {
  internalSub.foo()
  publicSub.foo()
}

// CHECK-LABEL: sil hidden @$s4main4test11internalSub06publicD0yAA0D0C_AA06PublicD0CtF
// CHECK: bb0([[ARG0:%.*]] : @guaranteed $Sub, [[ARG1:%.*]] : @guaranteed $PublicSub):
// CHECK: = class_method [[ARG0]] : $Sub, #Sub.foo!1
// CHECK: = class_method [[ARG1]] : $PublicSub, #PublicSub.foo!1
// CHECK: } // end sil function '$s4main4test11internalSub06publicD0yAA0D0C_AA06PublicD0CtF'


