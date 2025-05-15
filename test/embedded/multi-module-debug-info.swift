// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -c -I %t %t/Main.swift -enable-experimental-feature Embedded -verify -o /dev/null

// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

// BEGIN MyModule.swift

public struct MyError: Error {
}

@inline(never)
public func foo<T>(_ t: T) throws {
  throw MyError() // expected-error {{cannot use a value of protocol type 'any Error' in embedded Swift}}
}

@inline(never)
public func callit<T>(_ t: T) {
  do {
    try foo(t)
  } catch {
  }
}

// BEGIN Main.swift

import MyModule

public func testit() {
  callit(27) // expected-note {{generic specialization called here}}
}

