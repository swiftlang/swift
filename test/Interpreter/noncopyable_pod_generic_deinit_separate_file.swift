// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(deinit_lib)) \
// RUN:     -emit-module -emit-module-path %t/deinit_lib.swiftmodule \
// RUN:     -module-name deinit_lib \
// RUN:     %S/Inputs/noncopyable_pod_generic_deinit_lib.swift
// RUN: %target-codesign %t/%target-library-name(deinit_lib)
// RUN: %target-build-swift -parse-as-library -I %t -L %t -ldeinit_lib %s -o %t/a.out %target-rpath(%t)
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out %t/%target-library-name(deinit_lib) | %FileCheck %s

// REQUIRES: executable_test

// Verifies that runtime metadata initialization does not optimize away
// the user-defined deinit for a ~Copyable struct which is determined to be POD
// based on its fields alone (rdar://166549159).

import deinit_lib

@main
struct App {
  static func main() {
    print("-- start")
    do {
      let _ = NCWrapper<SomePOD>(SomePOD())
    }
    print("-- done")
  }
}

// CHECK:      -- start
// CHECK-NEXT: Container.deinit fired
// CHECK-NEXT: -- done
