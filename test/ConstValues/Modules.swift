// Constant values should be able to call "const" functions from other modules

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -parse-as-library
// RUN: %target-swift-frontend -emit-ir -I %t %t/Main.swift

//--- MyModule.swift

_const
public func foo() -> Int {
	return 42
}

//--- Main.swift

import MyModule

_const let constGlobal1: Int = foo()
