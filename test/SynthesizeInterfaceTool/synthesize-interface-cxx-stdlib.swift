// REQUIRES: target-same-as-host

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -module-name CxxStdlibInterface -cxx-interoperability-mode=default -o %t/CxxStdlibInterface.swiftmodule %s

// RUN: %target-swift-synthesize-interface -cxx-interoperability-mode=default -module-name CxxStdlibInterface -I %t -o - 2>&1 | %FileCheck %s --check-prefix=OK

// RUN: not %target-swift-synthesize-interface -module-name CxxStdlibInterface -I %t -o - 2>&1 | %FileCheck %s --check-prefix=NEEDS_INTEROP

public struct CxxStdlibInterfaceThing {
  public var value: Int = 0
  public func describe() -> String { "thing" }
}

// OK-NOT: but current compilation uses
// OK-NOT: unknown C++ stdlib
// OK:     public struct CxxStdlibInterfaceThing {
// OK-DAG:     public var value: Int
// OK-DAG:     public func describe() -> String
// OK-DAG: }

// NEEDS_INTEROP: error: module 'CxxStdlibInterface' was built with C++ interoperability enabled, but current compilation does not enable C++ interoperability
