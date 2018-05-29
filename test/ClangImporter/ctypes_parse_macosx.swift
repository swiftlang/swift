// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s

// REQUIRES: OS=macosx

import ctypes

func testImportMacTypes() {
  var _ : Float80 = Float80_test
  var _ : ctypes.Float80 = 0.0
}

