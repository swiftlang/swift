// Constant globals should "work" even when used across files in non-WMO builds.
// REQUIRES: swift_feature_CompileTimeValues
// REQUIRES: swift_feature_CompileTimeValuesPreview
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -parse-as-library -enable-experimental-feature CompileTimeValues -enable-experimental-feature CompileTimeValuesPreview
// RUN: %target-swift-frontend -emit-ir -I %t %t/Main.swift -verify -enable-experimental-feature CompileTimeValues -enable-experimental-feature CompileTimeValuesPreview

//--- MyModule.swift

public func foo() -> Int {
	return 42
}

//--- Main.swift

import MyModule

@const let constGlobal1: Int = foo()
// expected-error@-1 {{'@const' value should be initialized with a compile-time value}}
// expected-error@-2 {{global variable must be a compile-time constant}} // Remove this once we common out the diagnostics
