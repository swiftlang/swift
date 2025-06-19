// Constant globals should "work" even when used across files in non-WMO builds.
// REQUIRES: swift_feature_CompileTimeValues
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -parse-as-library -enable-experimental-feature CompileTimeValues
// RUN: %target-swift-frontend -emit-ir -I %t %t/Main.swift -verify -enable-experimental-feature CompileTimeValues

//--- MyModule.swift

public func foo() -> Int {
	return 42
}

//--- Main.swift

import MyModule

@const let constGlobal1: Int = foo()
// expected-error@-1 {{not supported in a '@const' expression}}
