// UNSUPPORTED: OS=windows-msvc

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %empty-directory(%t/v1)
// RUN: %empty-directory(%t/v2)

// RUN: %target-swift-frontend -emit-module -module-name UsesCxx -o %t/v1/UsesCxx.swiftmodule %t/v1.swift -I %t/Inputs -cxx-interoperability-mode=default -Xcc -std=c++17
// RUN: %target-swift-frontend -emit-module -module-name UsesCxx -o %t/v2/UsesCxx.swiftmodule %t/v2.swift -I %t/Inputs -cxx-interoperability-mode=default -Xcc -std=c++17

// Without -cxx-interoperability-mode the module cannot be loaded.
// RUN: not %api-digester -dump-sdk -module UsesCxx -o %t/no-interop.json -module-cache-path %t/mcp -swift-only -abort-on-module-fail -avoid-tool-args -I %t/v1 -I %t/Inputs 2>&1 | %FileCheck --check-prefix=NO-INTEROP %s
// NO-INTEROP: error: module 'Counter' requires feature 'cplusplus'
// NO-INTEROP: Failed to load module: UsesCxx

// RUN: %api-digester -dump-sdk -module UsesCxx -o %t/v1.json -module-cache-path %t/mcp -swift-only -abort-on-module-fail -avoid-tool-args -I %t/v1 -I %t/Inputs -cxx-interoperability-mode=default -Xcc -std=c++17
// RUN: %FileCheck --input-file %t/v1.json %s
// CHECK:      "name": "makeCounter",
// CHECK:      "printedName": "Counter.Counter",
// CHECK-NEXT: "usr": "c:@S@Counter"
// CHECK:      "name": "origin",
// CHECK:      "printedName": "{{.*}}geo.Point",
// CHECK-NEXT: "usr": "c:@N@geo@S@Point"

// Comparing the same module against its own baseline reports nothing.
// RUN: %api-digester -diagnose-sdk -module UsesCxx -baseline-path %t/v1.json -module-cache-path %t/mcp -abort-on-module-fail -compiler-style-diags -I %t/v1 -I %t/Inputs -cxx-interoperability-mode=default -Xcc -std=c++17 2>&1 | %FileCheck --check-prefix=SAME --allow-empty %s
// SAME-NOT: breakage

// Changes involving C++ types are diagnosed.
// RUN: not %api-digester -diagnose-sdk -module UsesCxx -baseline-path %t/v1.json -module-cache-path %t/mcp -abort-on-module-fail -compiler-style-diags -I %t/v2 -I %t/Inputs -cxx-interoperability-mode=default -Xcc -std=c++17 2>&1 | %FileCheck --check-prefix=BREAK --implicit-check-not="API breakage" %s
// BREAK: error: API breakage: func origin() has return type change from {{.*}}geo.Point to {{.*}}geo.Colour
// BREAK: error: API breakage: func paint(_:) has been removed

// RUN: not %api-digester -dump-sdk -module UsesCxx -o %t/invalid.json -module-cache-path %t/mcp -I %t/v1 -I %t/Inputs -cxx-interoperability-mode=bogus 2>&1 | %FileCheck --check-prefix=INVALID %s
// INVALID: error: invalid value 'bogus' in '-cxx-interoperability-mode=bogus'
// INVALID-NOT: Failed to load module

//--- Inputs/module.modulemap
module Counter {
  header "Counter.h"
  requires cplusplus
  export *
}

//--- Inputs/Counter.h
#pragma once

struct Counter {
  int value = 0;
  int next() { return ++value; }
};

namespace geo {
struct Point {
  int x = 0;
  int y = 0;
};

enum class Colour { red, green };
} // namespace geo

//--- v1.swift
import Counter

public func makeCounter() -> Counter { Counter() }
public func origin() -> geo.Point { geo.Point() }
public func paint(_ c: geo.Colour) {}

//--- v2.swift
import Counter

public func makeCounter() -> Counter { Counter() }
public func origin() -> geo.Colour { .red }
