// Members of '@usableFromInline' declarations that clients can reach have
// predictable symbols: a deinit is as usable as its type, and default argument
// generators are emitted into each client that uses them.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-ir -o %t/Lib.ll %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -emit-tbd-path %t/Lib.tbd -tbd-install_name Lib -validate-tbd-against-ir=all
// RUN: %FileCheck -check-prefix LIBRARY-IR %s < %t/Lib.ll
// RUN: %FileCheck -check-prefix TBD %s < %t/Lib.tbd
// RUN: %target-swift-frontend -emit-ir -o /dev/null %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -validate-tbd-against-ir=all -O

// RUN: %target-swift-frontend -c -emit-module -o %t/Lib.o %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -O
// RUN: %target-swift-frontend -c -I %t -o %t/Client.o %t/Client.swift -parse-as-library -module-name Client -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -O
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Lib.o %t/Client.o -o %t/Client
// RUN: %target-run %t/Client | %FileCheck -check-prefix OUTPUT %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: VENDOR=apple

//--- Lib.swift

// Clients that destroy a value call its deinit directly, whatever the
// optimization level.
// LIBRARY-IR-DAG: define {{(protected |dllexport )?}}swiftcc void @"$e3Lib8ResourceVfD"(
// TBD-DAG: "_$e3Lib8ResourceVfD"
@usableFromInline
struct Resource: ~Copyable {
  @usableFromInline var value: Int
  @usableFromInline init(value: Int) { self.value = value }
  deinit { print("deinit") }
}

// TBD-NOT: withDefault{{.*}}fA0_
@usableFromInline
func withDefault(_ x: Int, flag: Bool = true) -> Int { flag ? x : 0 }

public func generic<T>(_ t: T) -> Int {
  let r = Resource(value: MemoryLayout<T>.size)
  return withDefault(r.value)
}

//--- Client.swift
import Lib

@main
struct Main {
  static func main() {
    print(generic(Int64(1)))
  }
}

// OUTPUT: deinit
// OUTPUT-NEXT: 8
