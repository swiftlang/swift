// RUN: %empty-directory(%t)

// Build the library with the new protocol available.

// RUN: %target-build-swift -DIncludeNewProto -target %target-cpu-apple-macosx12 -parse-as-library -emit-library -enable-experimental-feature Reparenting -enable-experimental-feature SuppressedAssociatedTypesWithDefaults \
// RUN:     -emit-module-path %t/Reparenting.swiftmodule -module-name Reparenting -enable-library-evolution %S/Inputs/Reparenting.swift -o %t/%target-library-name(Reparenting)
// RUN: %target-codesign %t/%target-library-name(Reparenting)

// Build the client with an older deployment target + its custom witness for new() and run.

// RUN: %target-build-swift -DIncludeCustomWitness -target %target-cpu-apple-macosx10.15 -lReparenting -module-name main -I %t -L %t %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck --check-prefixes=CHECK,NEW-CLIENT-WITNESS %s

// Build the client with an older deployment target + the library-provided witness for new() and run.

// RUN: %target-build-swift -target %target-cpu-apple-macosx10.15 -lReparenting -module-name main -I %t -L %t %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck --check-prefixes=CHECK,NEW-LIB-WITNESS %s

///
// Key piece of the test:
// Replace the library dylib with one that never even knew about NewProto while using an older deployment target,
// then run WITHOUT rebuilding the client.
///

// RUN: %target-build-swift -target %target-cpu-apple-macosx10.15 -parse-as-library -emit-library -enable-experimental-feature Reparenting \
// RUN:     -emit-module-path %t/Reparenting.swiftmodule -module-name Reparenting -enable-library-evolution %S/Inputs/Reparenting.swift -o %t/%target-library-name(Reparenting)
// RUN: %target-codesign %t/%target-library-name(Reparenting)
// RUN: %target-run %t/a.out | %FileCheck --check-prefixes=CHECK,NEW-NO-WITNESS %s

// REQUIRES: OS=macosx && (CPU=x86_64 || CPU=arm64)
// REQUIRES: executable_test
// UNSUPPORTED: remote_run || device_run

// REQUIRES: swift_feature_Reparenting

import Reparenting

struct MyStruct: Derived {
  typealias Thing = String
  func existing() -> String { return "MyStruct.existing" }
  func derived() -> String { return "MyStruct.derived" }
#if IncludeCustomWitness
  func new() -> String { return "MyStruct.new" }
#endif
}

defer { main() }
func main() {
  print("start test")  // CHECK: start test
  let ms = MyStruct()
  libraryFunc(ms)
  // CHECK: libraryFunc start
  // CHECK: MyStruct.derived
  // CHECK: MyStruct.existing

  // NEW-LIB-WITNESS: defaulted Existing.new
  // NEW-CLIENT-WITNESS: MyStruct.new
  // NEW-NO-WITNESS-NOT: new

  // CHECK: libraryFunc end
}
