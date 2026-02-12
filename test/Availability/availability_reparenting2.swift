//////////////////
// Try first with an older macOS 10.15 library and client.

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-cpu-apple-macosx10.15 -parse-as-library -emit-library -enable-experimental-feature Reparenting \
// RUN:     -emit-module-path %t/Reparenting.swiftmodule -module-name Reparenting -enable-library-evolution %S/Inputs/Reparenting.swift -o %t/%target-library-name(Reparenting)
// RUN: %target-codesign %t/%target-library-name(Reparenting)
// RUN: %target-build-swift -target %target-cpu-apple-macosx10.15 -lReparenting -module-name main -I %t -L %t %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

//////////////////
// Now do it all again, but with a macOS 12 library as the target that includes the reparented conformance
// and test deployment targets both old and new

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-cpu-apple-macosx12 -parse-as-library -emit-library -enable-experimental-feature Reparenting \
// RUN:     -emit-module-path %t/Reparenting.swiftmodule -module-name Reparenting -enable-library-evolution %S/Inputs/Reparenting.swift -o %t/%target-library-name(Reparenting)
// RUN: %target-codesign %t/%target-library-name(Reparenting)

// RUN: %target-build-swift -target %target-cpu-apple-macosx10.15 -lReparenting -module-name main -I %t -L %t %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck --check-prefixes=CHECK,NEW-LIB %s

// RUN: %target-build-swift -target %target-cpu-apple-macosx12 -lReparenting -module-name main -I %t -L %t %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck --check-prefixes=CHECK,NEW-LIB,NEW-CLIENT %s

// REQUIRES: OS=macosx && (CPU=x86_64 || CPU=arm64)
// REQUIRES: executable_test
// UNSUPPORTED: remote_run || device_run

// REQUIRES: swift_feature_Reparenting

import Reparenting

struct MyStruct: Derived {
  typealias Thing = String
  func existing() -> String { return "MyStruct.existing" }
  func derived() -> String { return "MyStruct.derived" }
}

defer { main() }
func main() {
  print("start test")  // CHECK: start test
  let ms = MyStruct()
  print(ms.derived())  // CHECK: MyStruct.derived
  print(ms.existing()) // CHECK: MyStruct.existing

  if #available(macOS 12, *) {
    print(ms.new())    // NEW-CLIENT: defaulted Existing.new
  }

  // CHECK: libraryFunc start
  // NEW-LIB: defaulted Existing.new
  // CHECK: libraryFunc end
  libraryFunc(ms)
}
