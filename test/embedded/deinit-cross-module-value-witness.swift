// The destroy value witness of a non-copyable type has to call the type's
// deinit. Outside embedded Swift, IRGen may leave that to the value witness of
// the module which declares the type. In embedded Swift there is no such
// module: every module emits its own copy of the value witnesses of the types
// it uses, so delegating to the value witness would make the destroy value
// witness call itself and loop forever.
//
// The imported value is only ever held in a box here, so nothing in the
// client's SIL destroys it - IRGen synthesizes the box's destructor, and that
// is the only place the deinit is needed.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -wmo -O %t/Lib.swift -module-name Lib -emit-module -emit-module-path %t/Lib.swiftmodule -c -o %t/Lib.o

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -wmo -Onone -I %t %t/main.swift -module-name main -emit-ir -o %t/main.ll
// RUN: %FileCheck --check-prefix=DEINIT %s < %t/main.ll
// RUN: %FileCheck --check-prefix=NO-VW %s < %t/main.ll

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -wmo -O -I %t %t/main.swift -module-name main -c -o %t/main.o
// RUN: %target-embedded-link %t/Lib.o %t/main.o -o %t/a.out %target-clang-resource-dir-opt
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

// The box destructor calls the imported deinit directly ...
// DEINIT: call {{.*}}@"$e3Lib7StorageVfD"

// ... and no destroy value witness which delegates to itself is emitted.
// NO-VW-NOT: $e3Lib7StorageVwxx

//--- Lib.swift
public var deinitCount = 0

public struct Storage: ~Copyable {
  var p: UnsafeMutableRawPointer?

  public init() {
    p = .allocate(byteCount: 8, alignment: 8)
  }

  public var isEmpty: Bool { p == nil }

  deinit {
    deinitCount += 1
    if let p { p.deallocate() }
  }
}

//--- main.swift
import Lib

var escaping: (() -> Bool)? = nil

// Capturing a non-copyable `let` in an escaping closure boxes it, and the box
// is the only thing that ever destroys the value.
@inline(never)
func useBoxedStorage() {
  let storage = Storage()
  escaping = { storage.isEmpty }
  print(escaping!() ? "empty" : "allocated")
  // CHECK: allocated
  escaping = nil
}

@main
struct Main {
  static func main() {
    useBoxedStorage()
    print("deinits: \(deinitCount)")
    // CHECK-NEXT: deinits: 1
  }
}
