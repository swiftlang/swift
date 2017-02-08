// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s

// REQUIRES: OS=macosx

import ctypes

func testImportMacTypes() {
  // FIXME: this should work.  We cannot map Float80 to 'long double' because
  // 'long double' has size of 128 bits on SysV ABI, and 80 != 128.  'long
  // double' is currently not handled by the importer, so it cannot be
  // imported in normal way either.
  var t11_unqual : Float80 = Float80_test // expected-error {{use of unresolved identifier 'Float80_test'}}
  var t11_qual : ctypes.Float80 = 0.0 // expected-error {{no type named 'Float80' in module 'ctypes'}}
}

