// RUN: %target-run-simple-swift(-enable-experimental-feature CalledAttribute -enable-experimental-feature NondeinitableTypes -Xfrontend -disable-concrete-type-metadata-mangled-name-accessors) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_CalledAttribute
// REQUIRES: swift_feature_NondeinitableTypes

// The metadata that the compiler emits for a `@called` function type is the
// same metadata that the runtime builds from its mangled name.

func metadata<T: ~Copyable & ~Deinitable>(_: T.Type) -> UnsafeRawPointer {
  unsafeBitCast(T.self, to: UnsafeRawPointer.self)
}

func metadata(named name: String) -> UnsafeRawPointer {
  unsafeBitCast(_typeByName(name)!, to: UnsafeRawPointer.self)
}

let atMostOnce = metadata((@called(atMostOnce) () -> Void).self)
let exactlyOnce = metadata((@called(exactlyOnce) () -> Void).self)

// CHECK: true
print(atMostOnce == metadata(named: "yyXOo"))
// CHECK-NEXT: true
print(exactlyOnce == metadata(named: "yyXO"))
// CHECK-NEXT: true
print(atMostOnce != exactlyOnce)
// CHECK-NEXT: true
print(atMostOnce != metadata(named: "yyc"))
