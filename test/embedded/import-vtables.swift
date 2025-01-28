// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -c -I %t %t/Main.swift -enable-experimental-feature Embedded -o %t/a.o
// RUN: %target-clang -x c -c %S/Inputs/print.c -o %t/print.o
// RUN: %target-clang %t/a.o %t/print.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

// BEGIN MyModule.swift

public enum MyEnum {
    case aCase
}

open class BaseClass {
    open var type: MyEnum? { nil }
}

public final class DerivedClass: BaseClass {
    public override var type: MyEnum { .aCase }

    public override init() {
    }
}

open class BaseGenericClass<T> {
    open var type: MyEnum? { nil }
}

public final class DerivedGenericClass<T>: BaseGenericClass<T> {
    public override var type: MyEnum { .aCase }

    public override init() {
    }
}

public func createGenClass() -> BaseGenericClass<Bool> {
  return DerivedGenericClass<Bool>()
}

// BEGIN Main.swift

import MyModule

let dc = DerivedClass()
let dgc = DerivedGenericClass<Int>()
let gc = createGenClass()
print("OK!")

// CHECK: OK!

