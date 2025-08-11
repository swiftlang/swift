// RUN: %target-swift-emit-silgen -import-objc-header %S/Inputs/clang_records_addressable_for_dependencies.h -enable-experimental-feature Lifetimes %s | %FileCheck %s

// REQUIRES: swift_feature_Lifetimes

struct Dependent: ~Escapable {
	@_lifetime(immortal)
	init() { fatalError() }
}

// CHECK-LABEL: sil {{.*}} @${{.*}}12dependenceOn7cStruct{{.*}} : $@convention(thin) (@in_guaranteed CStruct)
@_lifetime(cStruct)
func dependenceOn(cStruct: CStruct) -> Dependent { fatalError() }

// CHECK-LABEL: sil {{.*}} @${{.*}}12dependenceOn6cUnion{{.*}} : $@convention(thin) (@in_guaranteed CUnion)
@_lifetime(cUnion)
func dependenceOn(cUnion: CUnion) -> Dependent { fatalError() }
