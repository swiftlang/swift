// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name CxxStdlibInterop -cxx-interoperability-mode=default -emit-module -emit-module-path %t/CxxStdlibInterop.swiftmodule
// RUN: %target-swift-symbolgraph-extract -module-name CxxStdlibInterop -cxx-interoperability-mode=default -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/CxxStdlibInterop.symbols.json

// REQUIRES: OS=linux-gnu

import CxxStdlib

public func makeString() -> std.string {
  return std.string()
}

// CHECK: "precise": "s:16CxxStdlibInterop10makeString
