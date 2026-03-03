// RUN: %empty-directory(%t)

// RUN: %target-build-swift %s \
// RUN: -module-name=Lib -package-name Pkg \
// RUN: -parse-as-library -emit-module -emit-module-path %t/Lib.swiftmodule -I%t \
// RUN: -Xfrontend -experimental-package-cmo -Xfrontend -experimental-allow-non-resilient-access \
// RUN: -O -wmo -enable-library-evolution

// RUN: %target-sil-opt -sil-print-types %t/Lib.swiftmodule -sil-verify-all -o %t/Lib.sil
// RUN: %FileCheck %s --check-prefix=CHECK < %t/Lib.sil

// REQUIRES: swift_in_compiler

public struct Something {
  public init() {}

  public func f() -> Int {
    return 7
  }
}


@usableFromInline
func use(_ c: () -> Int) { }

// Don't crash on this example
@inlinable
public func reproduce(_ e: Something) {
  use {
    return e.f()
  }
}

// use(_:)
// CHECK: sil [serialized_for_package] [canonical] [ossa] @$s3Lib3useyySiyXEF : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> Int) -> () {
// CHECK:   bb0(%0 : @guaranteed $@noescape @callee_guaranteed () -> Int):
// CHECK:       [[RESULT:%.*]] = tuple ()
// CHECK:       return [[RESULT]] : $()
// CHECK:     } // end sil function '$s3Lib3useyySiyXEF'

// closure #1 in reproduce(_:)
// CHECK: sil shared [serialized] [canonical] [ossa] @$s3Lib9reproduceyyAA9SomethingVFSiyXEfU_ : $@convention(thin) (@in_guaranteed Something) -> Int {
  // function_ref Something.f()
// CHECK: bb0(%0 : @closureCapture $*Something):
// CHECK: [[REF:%.*]] = function_ref @$s3Lib9SomethingV1fSiyF : $@convention(method) (@in_guaranteed Something) -> Int
// CHECK: [[VAL:%.*]] = apply [[REF]](%0) : $@convention(method) (@in_guaranteed Something) -> Int
// CHECK: return [[VAL]] : $Int
// CHECK: } // end sil function '$s3Lib9reproduceyyAA9SomethingVFSiyXEfU_'

// Something.f()
// CHECK: sil [serialized_for_package] [canonical] [ossa] @$s3Lib9SomethingV1fSiyF : $@convention(method) (@in_guaranteed Something) -> Int {

// Something.init()
// CHECK: sil [serialized_for_package] [canonical] [ossa] @$s3Lib9SomethingVACycfC : $@convention(method) (@thin Something.Type) -> @out Something {

// reproduce(_:)
// CHECK: sil [serialized] [canonical] [ossa] @$s3Lib9reproduceyyAA9SomethingVF : $@convention(thin) (@in_guaranteed Something) -> () {
// CHECK:   bb0(%0 : $*Something):
// CHECK:     [[CLOSURE_PTR:%.*]] = function_ref @$s3Lib9reproduceyyAA9SomethingVFSiyXEfU_ : $@convention(thin) (@in_guaranteed Something) -> Int
// CHECK:     [[CLOSURE_W_ARG:%.*]] = partial_apply [callee_guaranteed] [on_stack] [[CLOSURE_PTR]](%0) : $@convention(thin) (@in_guaranteed Something) -> Int
// CHECK:     [[MARKED:%.*]] = mark_dependence [nonescaping] [[CLOSURE_W_ARG]] : $@noescape @callee_guaranteed () -> Int on %0 : $*Something
// CHECK:     [[USE_REF:%.*]] = function_ref @$s3Lib3useyySiyXEF : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> Int) -> ()
// CHECK:     [[RESULT:%.*]] = apply [[USE_REF]]([[MARKED]]) : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> Int) -> ()

