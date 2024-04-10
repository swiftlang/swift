// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -parse-as-library
// RUN: %target-swift-frontend -c -I %t %t/Main.swift -o %t/a.o

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
