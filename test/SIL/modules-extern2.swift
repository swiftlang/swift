// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -parse-as-library
// RUN: %target-swift-frontend -c -I %t %t/Main.swift -o %t/a.o

// BEGIN MyModule.swift

@_silgen_name("some_forward_declared_api")
@_alwaysEmitIntoClient
func some_forward_declared_api()

@_transparent
public func publicFuncInAModule() {
  some_forward_declared_api()
}

// BEGIN Main.swift

import MyModule

@_silgen_name("some_forward_declared_api")
func some_forward_declared_api()

some_forward_declared_api()
publicFuncInAModule()
