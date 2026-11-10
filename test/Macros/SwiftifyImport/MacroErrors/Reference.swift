// REQUIRES: swift_swift_parser

// RUN: %target-typecheck-verify-swift %s -swift-version 5 -module-name main -disable-availability-checking -plugin-path %swift-plugin-dir

// expected-error@+1{{reference requires a lifetimeDependence on the same value}}
@_SwiftifyImport(.reference(pointer: .return))
func noLifetime(_ a: inout CInt) -> UnsafeMutablePointer<CInt> {
  unsafe UnsafeMutablePointer(bitPattern: 8)!
}

// expected-error@+1{{reference is currently only supported for the return value}}
@_SwiftifyImport(.reference(pointer: .param(1)), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .borrow))
func onParam(_ a: UnsafeMutablePointer<CInt>) -> UnsafeMutablePointer<CInt> {
  a
}

// expected-error@+1{{expected non-optional Unsafe[Mutable]Pointer<T> type for reference, got 'UnsafeMutablePointer<CInt>?'}}
@_SwiftifyImport(.reference(pointer: .return), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .borrow))
func optionalReturn(_ a: inout CInt) -> UnsafeMutablePointer<CInt>? {
  nil
}

// expected-error@+1{{reference requires a return value}}
@_SwiftifyImport(.reference(pointer: .return), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .borrow))
func noReturn(_ a: inout CInt) {
}
