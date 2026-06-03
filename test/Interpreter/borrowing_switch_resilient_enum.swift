// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(EnumLib)) -enable-library-evolution %S/Inputs/borrowing_switch_resilient_enum_lib.swift -emit-module -emit-module-path %t/EnumLib.swiftmodule -target %target-swift-5.1-abi-triple -module-name EnumLib
// RUN: %target-codesign %t/%target-library-name(EnumLib)

// RUN: %target-build-swift -target %target-swift-5.1-abi-triple %s -lEnumLib -I %t -L %t -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main

// RUN: %target-run %t/main %t/%target-library-name(EnumLib)

// REQUIRES: executable_test

import EnumLib

// CHECK: let's go
print("let's go")

func test1() {
        let v: MyEnum = .small("hello")
		// CHECK-NEXT: true
        print(v.isSmall)
		// CHECK-NEXT: hello
        print(v.smallValue as Any)
}
test1()

func test2() {
        let v: MyFrozenEnum = .small("hello")
		// CHECK-NEXT: true
        print(v.isSmall)
		// CHECK-NEXT: hello
        print(v.smallValue as Any)
}
test2()

func test3() {
        let v: MyEnum = .big(Payload(a: 17, b: nil, c: 38, d: nil))
		// CHECK-NEXT: false
        print(v.isSmall)
		// CHECK-NEXT: nil
        print(v.smallValue as Any)
}
test3()

func test4() {
        let v: MyGenericEnum<Payload> = .gen(Payload(a: 17, b: nil, c: 38, d: nil))
		// CHECK-NEXT: false
        print(v.isSmall)
		// CHECK-NEXT: nil
        print(v.smallValue as Any)
}
test4()

func test5() {
        let v: MyGenericEnum<Any> = .gen(Payload(a: 17, b: nil, c: 38, d: nil))
		// CHECK-NEXT: false
        print(v.isSmall)
		// CHECK-NEXT: nil
        print(v.smallValue as Any)
}
test5()

func test6() {
        let v: MyGenericEnum<Payload> = .small("butt")
		// CHECK-NEXT: true
        print(v.isSmall)
		// CHECK-NEXT: butt
        print(v.smallValue as Any)
}
test6()

func test7() {
        let v: MyGenericEnum<Any> = .small("butt")
		// CHECK-NEXT: true
        print(v.isSmall)
		// CHECK-NEXT: butt
        print(v.smallValue as Any)
}
test7()

// CHECK-NEXT: done
print("done")
