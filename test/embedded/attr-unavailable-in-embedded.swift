// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -parse-stdlib -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -typecheck -verify -I %t %t/Main.swift -parse-stdlib -enable-experimental-feature Embedded

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// BEGIN MyModule.swift

@_unavailableInEmbedded
public func unavailable() { }

// BEGIN Main.swift

import MyModule

func available() {
  unavailable() // expected-error {{'unavailable()' is unavailable: unavailable in embedded Swift}}
}

