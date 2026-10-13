// RUN: %target-run-simple-swift(-target %target-future-triple -enable-experimental-feature CalledAttribute) | %FileCheck %s
// RUN: %target-run-simple-swift(-target %target-future-triple -enable-experimental-feature CalledAttribute -Xfrontend -disable-concrete-type-metadata-mangled-name-accessors) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_CalledAttribute

// Only the Swift 6.5 runtime can demangle function types with execution
// semantics.
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// The metadata that the compiler emits for a function type with execution
// semantics is the same metadata that the runtime builds from its mangled name.

func metadata(named name: String) -> UnsafeRawPointer {
  unsafeBitCast(_typeByName(name)!, to: UnsafeRawPointer.self)
}

let atMostOnce = unsafeBitCast((@called(atMostOnce) () -> Void).self,
                               to: UnsafeRawPointer.self)
let exactlyOnce = unsafeBitCast((@called(exactlyOnce) () -> Void).self,
                                to: UnsafeRawPointer.self)

// CHECK: true
print(atMostOnce == metadata(named: "yyXOo"))
// CHECK-NEXT: true
print(exactlyOnce == metadata(named: "yyXO"))
// CHECK-NEXT: true
print(atMostOnce != exactlyOnce)
// CHECK-NEXT: true
print(atMostOnce != metadata(named: "yyc"))

// The runtime also names the metadata with the right execution semantics, and
// the name maps back to the same metadata.

func describe(_ metadata: UnsafeRawPointer) -> String {
  let type = unsafeBitCast(metadata, to: Any.Type.self)
  let name = _mangledTypeName(type)!
  let roundTrips = _typeByName(name).map {
    unsafeBitCast($0, to: UnsafeRawPointer.self) == metadata
  } ?? false
  return "\(String(reflecting: type)) \(name) \(roundTrips)"
}

// CHECK-NEXT: @called(atMostOnce) () -> () yyXOo true
print(describe(atMostOnce))
// CHECK-NEXT: @called(exactlyOnce) () -> () yyXO true
print(describe(exactlyOnce))
// CHECK-NEXT: @called(atMostOnce) (Swift.Int) throws -> Swift.String SSSiKXOo true
print(describe(unsafeBitCast((@called(atMostOnce) (Int) throws -> String).self,
                             to: UnsafeRawPointer.self)))
