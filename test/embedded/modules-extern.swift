// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -c -I %t %t/Main.swift -enable-experimental-feature Embedded -o %t/a.o

// REQUIRES: swift_in_compiler
// REQUIRES: OS=macosx || OS=linux-gnu

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
