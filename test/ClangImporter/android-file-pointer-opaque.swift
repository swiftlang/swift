// RUN: %target-swift-frontend -typecheck -target %target-triple23 %s
// RUN: %target-swift-frontend -typecheck -target %target-triple24 %s

// REQUIRES: OS=linux-android || OS=linux-androideabi

// On Android API 23 and below, bionic's stdio.h exposes FILE as the complete struct.
// Without an APINotes annotation, FILE* would import as
// UnsafeMutablePointer<FILE> rather than OpaquePointer, breaking source
// compatibility with code written against API 24+ where FILE is already opaque.
// The _stdio.apinotes file annotates __sFILE with SwiftImportAs: opaque_pointer
// to normalise the import type to OpaquePointer across all API levels.

import Android

func verifyFILEImportsAsOpaquePointer(path: UnsafePointer<CChar>,
                                      mode: UnsafePointer<CChar>) {
  // fopen / fdopen return FILE* — must be OpaquePointer? on both API 23 and 24+.
  let fp: OpaquePointer? = fopen(path, mode)
  if let fp {
    fclose(fp)
  }

  let fp2: OpaquePointer? = fdopen(STDIN_FILENO, mode)
  _ = fp2
}
