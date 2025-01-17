// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -enable-experimental-feature Extern -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -parse-as-library
// RUN: %target-swift-frontend -enable-experimental-feature Extern -c -I %t %t/Main.swift -o %t/a.o

// REQUIRES: swift_feature_Extern

// BEGIN MyModule.swift

@_extern(c)
@_alwaysEmitIntoClient
func some_c_api()

@_transparent
public func publicFuncInAModule() {
  some_c_api()
}

// BEGIN Main.swift

import MyModule

@_extern(c)
func some_c_api()

some_c_api()
publicFuncInAModule()
