// RUN: %target-swift-frontend -primary-file %s -parse-as-library -emit-sil \
// RUN:   -import-objc-header "%S/Inputs/verify_diagnostics_objc_header.h" \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -verify-additional-file "%S/Inputs/verify_diagnostics_objc_header.h" \
// RUN:   -sil-verify-all \
// RUN:   -module-name test

// REQUIRES: objc_interop

func test() {
  let _ = TakeNoEscapeBlock { }
}
