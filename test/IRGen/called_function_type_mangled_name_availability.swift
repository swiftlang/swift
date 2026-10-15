// RUN: %target-swift-frontend -emit-ir %s -target %target-swift-6.4-abi-triple -enable-experimental-feature CalledAttribute | %FileCheck %s --check-prefix=OLD
// RUN: %target-swift-frontend -emit-ir %s -target %target-future-triple -enable-experimental-feature CalledAttribute | %FileCheck %s --check-prefix=NEW

// REQUIRES: swift_feature_CalledAttribute
// REQUIRES: OS=macosx || OS=maccatalyst || OS=ios || OS=tvos || OS=watchos || OS=xros

// Runtimes before Swift 6.5 can't demangle function types with execution
// semantics, so when the deployment target includes such a runtime, metadata
// for them must not be instantiated from a mangled name.

struct G<T: ~Copyable>: ~Copyable {}

@inline(never)
func use<T: ~Copyable>(_: T.Type) {}

public func test() {
  use(G<@called(atMostOnce) () -> Void>.self)
}

// OLD-NOT: __swift_instantiateConcreteTypeFromMangledName{{.*}}XOo
// OLD: call swiftcc %swift.metadata_response @"$s{{.*}}1GVyyyXOoGMa"

// NEW: call ptr @__swift_instantiateConcreteTypeFromMangledName{{.*}}(ptr @"$s{{.*}}1GVyyyXOoGM{{.*}}"
