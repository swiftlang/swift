// Before serialization, the optimizer doesn't inline a function with a unique
// definition into code that clients emit themselves, unless the function's
// body is serialized too. Otherwise, clients would keep using the inlined code
// after the function changes, and the function's private references would
// have to become public symbols, which the TBD file can't predict.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-ir -o /dev/null %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature Extern -enable-experimental-feature CodeGenerationModel=interface -emit-tbd-path %t/Lib.tbd -tbd-install_name Lib -validate-tbd-against-ir=all
// RUN: %target-swift-frontend -emit-ir -o %t/Lib-O.ll %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature Extern -enable-experimental-feature CodeGenerationModel=interface -emit-tbd-path %t/Lib-O.tbd -tbd-install_name Lib -validate-tbd-against-ir=all -O
// RUN: %FileCheck -check-prefix LIBRARY-IR %s < %t/Lib-O.ll

// RUN: %target-swift-frontend -emit-module -o %t/Lib.swiftmodule %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature Extern -enable-experimental-feature CodeGenerationModel=interface -O
// RUN: %target-sil-opt %t/Lib.swiftmodule -enable-experimental-feature Embedded -emit-sorted-sil | %FileCheck -check-prefix SERIALIZED %s

// RUN: %target-swift-frontend -c -emit-module -o %t/Lib.o %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature Extern -enable-experimental-feature CodeGenerationModel=interface -O
// RUN: %target-swift-frontend -c -I %t -o %t/Client.o %t/Client.swift -parse-as-library -module-name Client -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -O
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Lib.o %t/Client.o -o %t/Client
// RUN: %target-run %t/Client | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern
// REQUIRES: VENDOR=apple

//--- Lib.swift
@_extern(c, "getpid") func c_getpid() -> Int32

// The global's once-token and once-initializer stay internal, because its
// addressor isn't inlined into generic code.
// LIBRARY-IR-DAG: @"$e3Lib6globalSivp" = global
// SERIALIZED-NOT: @$e3Lib6global_WZ
// LIBRARY-IR-DAG: @"$e3Lib6global_Wz" = internal global
// LIBRARY-IR-DAG: define internal {{.*}}@"$e3Lib6global_WZ"(
@usableFromInline
var global: Int = Int(c_getpid() &* 0) &+ 10

// An @inlinable body is serialized, so it can be inlined.
@inlinable
var viaInlinable: Int {
  @inline(__always) get { global }
}

private func helper() -> Int { global &* 2 }

@usableFromInline @inline(__always)
func alwaysInline() -> Int { helper() &+ 1 }

// Even a body that only refers to what clients can use isn't inlined.
@usableFromInline @inline(__always)
func onlyPublicReferences() -> Int { MemoryLayout<Int>.size &* 3 }

// SERIALIZED-LABEL: sil [serialized] [export_implementation] {{.*}}@$e3Lib7genericySixlF :
// SERIALIZED-DAG: function_ref @$e3Lib12alwaysInlineSiyF
// SERIALIZED-DAG: function_ref @$e3Lib20onlyPublicReferencesSiyF
// SERIALIZED: } // end sil function '$e3Lib7genericySixlF'
public func generic<T>(_ t: T) -> Int {
  viaInlinable &+ alwaysInline() &+ onlyPublicReferences() &+
    MemoryLayout<T>.size
}

// The once-initializer of a global that clients emit doesn't get the body of
// a function with a unique definition.
@usableFromInline
func compute() -> Int { helper() &+ 2 }

@export(implementation)
public var implementationGlobal: Int = compute()

// A lazy property getter isn't inlined into generic code either.
@usableFromInline
final class Box {
  @usableFromInline
  lazy var value: Int = helper() &+ 3

  @usableFromInline
  init() {}
}

public func genericLazy<T>(_ t: T) -> Int {
  let box = Box()
  return box.value &+ box.value &+ MemoryLayout<T>.size
}

//--- Client.swift
import Lib

@main
struct Main {
  static func main() {
    print(generic(Int64(1)))
    print(implementationGlobal)
    print(genericLazy(Int64(1)))
  }
}

// CHECK: 63
// CHECK-NEXT: 22
// CHECK-NEXT: 54
