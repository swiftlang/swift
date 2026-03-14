// Constant values should be able to call "const" functions from other modules
// REQUIRES: swift_feature_CompileTimeValues
// REQUIRES: swift_feature_CompileTimeValuesPreview
// REQUIRES: rdar146953110
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -parse-as-library -enable-experimental-feature CompileTimeValues -enable-experimental-feature CompileTimeValuesPreview
// RUN: %target-swift-frontend -emit-ir -I %t %t/Main.swift -enable-experimental-feature CompileTimeValues -enable-experimental-feature CompileTimeValuesPreview

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -parse-as-library -enable-experimental-feature CompileTimeValues
// RUN: %target-swift-frontend -emit-ir -I %t %t/Main.swift -enable-experimental-feature CompileTimeValues

//--- MyModule.swift

@const
public func foo() -> Int {
	return 42
}

//--- Main.swift

import MyModule

@const let constGlobal1: Int = foo()
