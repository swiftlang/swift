// RUN: %target-run-simple-swift(-target %target-future-triple -enable-experimental-feature OnceFunctions) | %FileCheck %s
// RUN: %target-run-simple-swift(-target %target-future-triple -enable-experimental-feature OnceFunctions -Xfrontend -disable-concrete-type-metadata-mangled-name-accessors) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_OnceFunctions

// Only the Swift 6.5 runtime can demangle function types with execution
// semantics.
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// The metadata that the compiler emits for a function type with execution
// semantics is the same metadata that the runtime builds from its mangled name.

func metadata(named name: String) -> UnsafeRawPointer {
  unsafeBitCast(_typeByName(name)!, to: UnsafeRawPointer.self)
}

let atMostOnce = unsafeBitCast((@callable(once) () -> Void).self,
                               to: UnsafeRawPointer.self)
let exactlyOnce = unsafeBitCast((@once () -> Void).self,
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

// CHECK-NEXT: @callable(once) () -> () yyXOo true
print(describe(atMostOnce))
// CHECK-NEXT: @once () -> () yyXO true
print(describe(exactlyOnce))
// CHECK-NEXT: @callable(once) (Swift.Int) throws -> Swift.String SSSiKXOo true
print(describe(unsafeBitCast((@callable(once) (Int) throws -> String).self,
                             to: UnsafeRawPointer.self)))
