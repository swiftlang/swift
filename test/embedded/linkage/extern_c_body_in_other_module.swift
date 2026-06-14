// Verify that an `@_extern(c)` declaration in one module can be satisfied
// by a `@c`-annotated function (which has only a C entry point, no Swift
// entry point) defined in another module. The two SIL functions have
// different mangled names but share an asm name, and the C linker resolves
// the symbol at link time. This exercises the SIL Linker's fragile-ref
// check on a `consumerDouble` body-less forward declaration referenced
// from a serialized `@inlinable` function in another module.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -c -emit-module -o %t/Provider.o %t/Provider.swift -enable-experimental-feature Embedded -enable-experimental-feature CAttribute -parse-as-library
// RUN: %target-swift-frontend -c -I %t -emit-module -o %t/Consumer.o %t/Consumer.swift -enable-experimental-feature Embedded -enable-experimental-feature Extern -parse-as-library
// RUN: %target-swift-frontend -c -I %t -emit-module -o %t/Application.o %t/Application.swift -enable-experimental-feature Embedded -enable-experimental-feature Extern -parse-as-library
// RUN: %target-clang %target-clang-resource-dir-opt %t/Provider.o %t/Consumer.o %t/Application.o %target-embedded-posix-shim -o %t/Application
// RUN: %target-run %t/Application | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern
// REQUIRES: swift_feature_CAttribute

//--- Provider.swift

// `@c` (without the underscore) produces *only* a C entry point — the
// function body is the C-callable symbol "extern_c_provider_double".
@c(extern_c_provider_double)
public func provideDouble(_ x: Int) -> Int {
  return x * 2
}

//--- Consumer.swift

// Body-less `@_extern(c)` that points at the same C symbol as Provider.
@_extern(c, "extern_c_provider_double")
public func consumerDouble(_ x: Int) -> Int

// Inlinable so the call is exposed across the module boundary, forcing the
// SIL Linker to reason about the body-less `consumerDouble` forward
// declaration from any caller's translation unit.
@inlinable
public func callConsumerDouble() -> Int {
  return consumerDouble(21)
}

//--- Application.swift

import Consumer

@main
struct Main {
  static func main() {
    // CHECK: 42
    print(callConsumerDouble())
  }
}
