// REQUIRES: target-same-as-host

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Output)
// RUN: %target-build-swift %s -module-name CxxInterop -emit-module -emit-module-path %t/ -cxx-interoperability-mode=default

// RUN: %target-swift-symbolgraph-extract -module-name CxxInterop -I %t -cxx-interoperability-mode=default -pretty-print -output-dir %t/Output 2>&1 | %FileCheck %s --check-prefix=STDLIB_OK --allow-empty
// RUN: %FileCheck %s --check-prefix=SYMBOLS --input-file %t/Output/CxxInterop.symbols.json

// RUN: not %target-swift-symbolgraph-extract -module-name CxxInterop -I %t -pretty-print -output-dir %t/Output 2>&1 | %FileCheck %s --check-prefix=NEEDS_INTEROP

public struct CxxInteropThing {
  public var value: Int = 0
  public func describe() -> String { "thing" }
}

// STDLIB_OK-NOT: was built with
// STDLIB_OK-NOT: unknown C++ stdlib

// SYMBOLS-DAG: "title": "CxxInteropThing"
// SYMBOLS-DAG: "title": "value"
// SYMBOLS-DAG: "title": "describe()"

// NEEDS_INTEROP: error: module 'CxxInterop' was built with C++ interoperability enabled, but current compilation does not enable C++ interoperability
