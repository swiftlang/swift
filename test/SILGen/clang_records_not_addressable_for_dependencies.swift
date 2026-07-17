// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values -import-objc-header %S/Inputs/clang_records_not_addressable_for_dependencies.h -enable-experimental-feature Lifetimes %s
// RUN: %target-swift-emit-silgen -import-objc-header %S/Inputs/clang_records_not_addressable_for_dependencies.h -enable-experimental-feature Lifetimes %s | %FileCheck %s

// REQUIRES: swift_feature_Lifetimes

struct Dependent: ~Escapable {
	@_lifetime(immortal)
	init() { fatalError() }
}

// Foreign (C/C++) record types keep their natural foreign calling convention
// and are passed directly, rather than being forced indirect via addressable
// for dependencies.

// CHECK-LABEL: sil {{.*}} @${{.*}}12dependenceOn7cStruct{{.*}} : $@convention(thin) (CStruct) -> @lifetime(borrow 0) @owned Dependent
@_lifetime(cStruct)
func dependenceOn(cStruct: CStruct) -> Dependent { fatalError() }

// CHECK-LABEL: sil {{.*}} @${{.*}}12dependenceOn6cUnion{{.*}} : $@convention(thin) (CUnion) -> @lifetime(borrow 0) @owned Dependent
@_lifetime(cUnion)
func dependenceOn(cUnion: CUnion) -> Dependent { fatalError() }
