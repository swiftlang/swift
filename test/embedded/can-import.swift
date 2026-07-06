// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s
// RUN: mkdir -p %t/embedded
// RUN: mkdir -p %t/desktop

// Build the module as both embedded and non-embedded

// RUN: %target-swift-frontend -emit-module -o %t/desktop/MyModule.swiftmodule %t/MyModule.swift -parse-as-library
// RUN: %target-swift-frontend -emit-module -o %t/embedded/MyModule.swiftmodule %t/MyModule.swift -parse-as-library -enable-experimental-feature Embedded

// Test for canImport of a non-embedded module into an embedded module
// RUN: %target-typecheck-verify-swift -I %t/desktop -enable-experimental-feature Embedded

// Test for canImport of an embedded module into a non-embedded module
// RUNx: %target-typecheck-verify-swift -I %t/embedded

// REQUIRES: swift_in_compiler
// REQUIRES: OS=macosx || OS=linux-gnu || OS=wasip1
// REQUIRES: swift_feature_Embedded

// BEGIN MyModule.swift

public func f() { }

// BEGIN Main.swift

// expected-warning@+1{{canImport() evaluated to false due to invalid swiftmodule:}}
#if canImport(MyModule)
#error("Shouldn't be able to see the module!!")
import MyModule
#endif
