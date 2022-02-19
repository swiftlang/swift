// RUN: %target-swift-ide-test -print-module -module-to-print=ImplicitComputedProperties -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

//CHECK: struct VoidGetter {
//CHECK-NEXT:    init()
//CHECK-NEXT: mutating func getX()
//CHECK-NEXT: mutating func setX(_: Int32)
//CHECK-NEXT: }

//CHECK: struct VoidGetterNoName {
//CHECK-NEXT: init()
//CHECK-NEXT: mutating func set()
//CHECK-NEXT:}

//CHECK: struct IllegalIntReturnSetter {
//CHECK-NEXT: init()
//CHECK-NEXT: mutating func setX(_: Int32) -> Int32
//CHECK-NEXT: }

//CHECK: struct TwoParameterSetter {
//CHECK-NEXT: init()
//CHECK-NEXT: mutating func setX(_: Int32, _: Int32)
//CHECK-NEXT: }

//CHECK: struct NoNameSetter {
//CHECK-NEXT: init()
//CHECK-NEXT: mutating func set(_: Int32)
//CHECK-NEXT: }

//CHECK: struct NoNameVoidGetter {
//CHECK-NEXT: init()
//CHECK-NEXT: mutating func get()
//CHECK-NEXT: }

//CHECK: struct LongNameAllLower {
//CHECK-NEXT: init()
//CHECK-NEXT: var foo: Int32
//CHECK-NEXT: mutating func getfoo() -> Int32
//CHECK-NEXT: }

//CHECK: struct LongNameAllUpper {
//CHECK-NEXT: init()
//CHECK-NEXT: var foo: Int32
//CHECK-NEXT: mutating func getFOO() -> Int32
//CHECK-NEXT: }

//CHECK: struct LongNameMix {
//CHECK-NEXT: init()
//CHECK-NEXT: var foo: Int32
//CHECK-NEXT: mutating func GetFoo() -> Int32
//CHECK-NEXT: }

//CHECK: struct NoNameUpperGetter {
//CHECK-NEXT: init()
//CHECK-NEXT: mutating func Getter() -> Int32
//CHECK-NEXT: }

//CHECK: struct NotypeSetter {
//CHECK-NEXT: init()
//CHECK-NEXT: mutating func setX()
//CHECK-NEXT: }

//CHECK: struct IntGetterSetter {
//CHECK-NEXT: init()
//CHECK-NEXT: var x: Int32
//CHECK-NEXT: func getX() -> Int32
//CHECK-NEXT: mutating func setX(_: Int32)
//CHECK-NEXT: }
