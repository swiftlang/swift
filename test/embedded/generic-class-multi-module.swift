// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -enable-experimental-feature Extern -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -enable-experimental-feature Extern -c -I %t %t/Main.swift -enable-experimental-feature Embedded -o %t/a.o

// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern

// BEGIN MyModule.swift


// Check that the compiler can build this and doesn't crash.

public class SubClass<T> {
}

// BEGIN Main.swift

import MyModule

var dummy: [SubClass<Int>] = []
