// RUN: %target-swift-frontend -typecheck -I %S/Inputs/ %s -verify

// Test that SwiftImportAs: opaque_pointer in an .apinotes file causes
// pointer-to-struct to be imported as OpaquePointer. This requires the
// corresponding Clang APINotes change in apple/llvm-project to translate
// the YAML key into swift_attr("import_pointer_as_opaque") on the record.

import OpaquePointerImportAPINotes

func testAnnotatedViaAPINotes(f: FileHandle) {
  let _: OpaquePointer = f
  let _: OpaquePointer? = openFileHandle()
  closeFileHandle(f)

  let raw = getRawFileImpl()
  let _: OpaquePointer? = raw
}
