// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -enable-experimental-feature Extern -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -enable-experimental-feature Extern -c -I %t %t/Main.swift -enable-experimental-feature Embedded -o %t/a.o

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern

// BEGIN MyModule.swift

@_extern(c)
func some_c_api()

@_transparent
public func publicFuncInAModule() {
  internalFuncInAModule()
}

@usableFromInline
internal func internalFuncInAModule() {
  some_c_api()
}

// BEGIN Main.swift

import MyModule

@_extern(c)
func some_c_api()

some_c_api()
publicFuncInAModule()
