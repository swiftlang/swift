// RUN: %target-swift-frontend -typecheck -I %S/Inputs/ %s -verify

// Test that a C struct annotated with swift_attr("import_pointer_as_opaque")
// causes pointer-to-struct to be imported as OpaquePointer, even though the
// struct definition is complete.

import OpaquePointerImport

// Pointers to the annotated struct come through as OpaquePointer.
func testAnnotatedViaAttr(h: OpaqueHandle) {
  // OpaqueHandle is a typedef for OpaqueHandleImpl*, which should be OpaquePointer.
  let _: OpaquePointer = h
  let _: OpaquePointer? = createOpaqueHandle()
  consumeOpaqueHandle(h)

  // Direct struct pointer (not through the typedef) should also be OpaquePointer.
  let raw = getOpaqueHandleRaw()
  let _: OpaquePointer? = raw
}

// Pointers to the unannotated struct still come through as UnsafeMutablePointer<PlainStruct>.
func testUnannotated(s: UnsafeMutablePointer<PlainStruct>) {
  let _: UnsafeMutablePointer<PlainStruct>? = createPlainStruct()
  consumePlainStruct(s)
}
