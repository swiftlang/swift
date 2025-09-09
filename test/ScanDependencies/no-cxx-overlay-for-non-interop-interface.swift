
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/deps)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -o %t/deps.json %t/clientWithInteropDep.swift -I %t/deps -cxx-interoperability-mode=default -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -verify
// RUN: cat %t/deps.json | %FileCheck %s -check-prefix=ENABLE-CHECK

// RUN: %target-swift-frontend -scan-dependencies -o %t/deps_no_interop_dep.json %t/clientNoInteropDep.swift -I %t/deps -cxx-interoperability-mode=default -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -verify
// RUN: cat %t/deps_no_interop_dep.json | %FileCheck %s -check-prefix=DISABLE-CHECK

// RUN: %target-swift-frontend -emit-module %t/BinaryDepNoInterop.swift -emit-module-path %t/deps/BinaryDepNoInterop.swiftmodule -module-name BinaryDepNoInterop -I %t/deps -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import
// RUN: %target-swift-frontend -scan-dependencies -o %t/deps_no_interop_binary_dep.json %t/clientNoInteropBinaryDep.swift -I %t/deps -cxx-interoperability-mode=default -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -verify
// RUN: cat %t/deps_no_interop_binary_dep.json | %FileCheck %s -check-prefix=DISABLE-BINARY-CHECK

// RUN: %target-swift-frontend -scan-dependencies -o %t/deps_darwin_dep.json %t/clientDarwin.swift -I %t/deps -cxx-interoperability-mode=default -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -verify
// RUN: cat %t/deps_darwin_dep.json | %FileCheck %s -check-prefix=DARWIN-CHECK

//--- deps/bar.h
void bar(void);

//--- deps/baz.h
#include "bar.h"
void baz(void);

//--- deps/module.modulemap
module std_Bar [system] {
  header "bar.h"
  export *
}
module normal {
  header "baz.h"
  export *
}

//--- deps/Foo.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name Foo -enable-library-evolution
import std_Bar
public struct Foo1 {}

//--- deps/FooNoInterop.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name FooNoInterop -enable-library-evolution
// swift-module-flags-ignorable: -formal-cxx-interoperability-mode=off
import std_Bar
public struct Foo2 {}

//--- deps/Darwin.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name Darwin -enable-library-evolution
import std_Bar
public struct Foo3 {}

//--- BinaryDepNoInterop.swift
import normal
public struct Foo6 {}

//--- clientWithInteropDep.swift
import Foo

//--- clientNoInteropDep.swift
import FooNoInterop

//--- clientNoInteropBinaryDep.swift
import BinaryDepNoInterop

//--- clientDarwin.swift
import Darwin

// Ensure that when the 'Foo' dependency was built with C++ interop enabled,
// it gets the C++ standard library overlay for its 'std_*' dependency
//
// 'Foo' as it appears in direct deps
// ENABLE-CHECK: "swift": "Foo"
// 'Foo' as it appears in source-import deps
// ENABLE-CHECK: "swift": "Foo"
// Actual dependency info node
// ENABLE-CHECK: "swift": "Foo"
// ENABLE-CHECK:      "directDependencies": [
// ENABLE-CHECK:        {
// ENABLE-CHECK:          "swift": "SwiftOnoneSupport"
// ENABLE-CHECK:        },
// ENABLE-CHECK:        {
// ENABLE-CHECK:          "swift": "CxxStdlib"
// ENABLE-CHECK:        },
// ENABLE-CHECK:        {
// ENABLE-CHECK:          "clang": "std_Bar"
// ENABLE-CHECK:        }
// ENABLE-CHECK:      ],

// Ensure that when the 'Foo' dependency was *not* built with C++ interop enabled,
// it does not get the C++ standard library overlay for its 'std_*' dependency
//
// 'Foo' as it appears in direct deps
// DISABLE-CHECK: "swift": "FooNoInterop"
// 'Foo' as it appears in source-import deps
// DISABLE-CHECK: "swift": "FooNoInterop"
// Actual dependency info node
// DISABLE-CHECK: "swift": "FooNoInterop"
// DISABLE-CHECK:      "directDependencies": [
// DISABLE-CHECK:        {
// DISABLE-CHECK:          "swift": "SwiftOnoneSupport"
// DISABLE-CHECK:        },
// DISABLE-CHECK:        {
// DISABLE-CHECK:          "clang": "std_Bar"
// DISABLE-CHECK:        }
// DISABLE-CHECK:      ],

// DISABLE-BINARY-CHECK: "modulePath": "{{.*}}{{/|\\}}BinaryDepNoInterop.swiftmodule"
// DISABLE-BINARY-CHECK:      "directDependencies": [
// DISABLE-BINARY-CHECK-NEXT:        {
// DISABLE-BINARY-CHECK-NEXT:          "swift": "Swift"
// DISABLE-BINARY-CHECK-NEXT:        },
// DISABLE-BINARY-CHECK-NEXT:        {
// DISABLE-BINARY-CHECK-NEXT:          "swift": "SwiftOnoneSupport"
// DISABLE-BINARY-CHECK-NEXT:        },
// DISABLE-BINARY-CHECK-NEXT:        {
// DISABLE-BINARY-CHECK-NEXT:          "clang": "normal"
// DISABLE-BINARY-CHECK-NEXT:        }
// DISABLE-BINARY-CHECK-NEXT:      ],

// DARWIN-CHECK: "modulePath": "{{.*}}{{/|\\}}Darwin-{{.*}}.swiftmodule"
// DARWIN-CHECK:      "directDependencies": [
// DARWIN-CHECK-NEXT:        {
// DARWIN-CHECK-NEXT:          "swift": "SwiftOnoneSupport"
// DARWIN-CHECK-NEXT:        },
// DARWIN-CHECK-NEXT:        {
// DARWIN-CHECK-NEXT:          "clang": "std_Bar"
// DARWIN-CHECK-NEXT:        }
// DARWIN-CHECK-NEXT:      ],
