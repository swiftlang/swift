// Constant globals should "work" even when used across files in non-WMO builds.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -parse-as-library
// RUN: %target-swift-frontend -emit-ir -I %t %t/Main.swift -verify

//--- MyModule.swift

public func foo() -> Int {
	return 42
}

//--- Main.swift

import MyModule

_const let constGlobal1: Int = foo()
// expected-error@-1 {{_const let should be initialized with a compile-time value}}
