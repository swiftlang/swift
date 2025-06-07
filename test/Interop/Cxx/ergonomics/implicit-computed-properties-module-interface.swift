// RUN: %target-swift-ide-test -print-module -module-to-print=ImplicitComputedProperties -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop -cxx-interop-getters-setters-as-properties | %FileCheck %s

// CHECK:      struct VoidGetter {
// CHECK-NOT:     var
// CHECK-NEXT:    init()
// CHECK-NEXT:    mutating func getX()
// CHECK-NEXT:    mutating func setX(_: Int32)
// CHECK-NEXT: }

// CHECK:      struct VoidSetterNoName {
// CHECK-NOT:     var
// CHECK-NEXT:    init()
// CHECK-NEXT:    mutating func set()
// CHECK-NEXT: }

// CHECK:      struct IllegalIntReturnSetter {
// CHECK-NOT:     var
// CHECK-NEXT:    init()
// CHECK-NEXT:    mutating func setX(_: Int32) -> Int32
// CHECK-NEXT: }

// CHECK:      struct TwoParameterSetter {
// CHECK-NOT:     var
// CHECK-NEXT:    init()
// CHECK-NEXT:    mutating func setX(_: Int32, _: Int32)
// CHECK-NEXT: }

// CHECK:      struct NoNameSetter {
// CHECK-NOT:     var
// CHECK-NEXT:    init()
// CHECK-NEXT:    mutating func set(_: Int32)
// CHECK-NEXT: }

// CHECK:      struct NoNameVoidGetter {
// CHECK-NOT:     var
// CHECK-NEXT:    init()
// CHECK-NEXT:    mutating func get()
// CHECK-NEXT: }

// CHECK:      struct LongNameAllLower {
// CHECK-NOT:     var
// CHECK-NEXT:    init()
// CHECK-NEXT:    init(value: Int32)
// CHECK-NEXT:    var foo: Int32
// CHECK-NEXT:    func getfoo() -> Int32
// CHECK-NEXT:    mutating func setfoo(_ v: Int32)
// CHECK-NEXT:    var value: Int32
// CHECK-NEXT: }

// CHECK:      struct LongNameAllUpper {
// CHECK-NEXT:     init()
// CHECK-NEXT:     init(value: Int32)
// CHECK-NEXT:     var foo: Int32
// CHECK-NEXT:     func getFOO() -> Int32
// CHECK-NEXT:     mutating func setFOO(_ v: Int32)
// CHECK-NEXT:     var value: Int32
// CHECK-NEXT: }

// CHECK:      struct UpperCaseMix {
// CHECK-NEXT:    init()
// CHECK-NEXT:    init(value: Int32)
// CHECK-NEXT:    var foo: Int32
// CHECK-NEXT:    func getFoo() -> Int32
// CHECK-NEXT:    mutating func SetFoo(_ v: Int32)
// CHECK-NEXT:    var value: Int32
// CHECK-NEXT: }

// CHECK:      struct UpperCaseGetterSetter {
// CHECK-NEXT:    init()
// CHECK-NEXT:    init(value: Int32)
// CHECK-NEXT:    var foo: Int32
// CHECK-NEXT:    func GetFoo() -> Int32
// CHECK-NEXT:    mutating func SetFoo(_ v: Int32)
// CHECK-NEXT:    var value: Int32
// CHECK-NEXT: }

// CHECK:      struct GetterOnly {
// CHECK-NEXT:    init()
// CHECK-NEXT:    var foo: Int32 { get }
// CHECK-NEXT:    func getFoo() -> Int32
// CHECK-NEXT: }

// CHECK:      struct NoNameUpperGetter {
// CHECK-NOT:     var
// CHECK-NEXT:    init()
// CHECK-NEXT:    mutating func Getter() -> Int32
// CHECK-NEXT: }

// CHECK:      struct NotypeSetter {
// CHECK-NEXT:    init()
// CHECK-NEXT:    mutating func setX()
// CHECK-NEXT: }

// CHECK:      struct IntGetterSetter {
// CHECK-NEXT:    init()
// CHECK-NEXT:    init(val: Int32)
// CHECK-NEXT:    var x: Int32
// CHECK-NEXT:    func getX() -> Int32
// CHECK-NEXT:    mutating func setX(_ v: Int32)
// CHECK-NEXT:    var val: Int32
// CHECK-NEXT: }

// CHECK:      struct IntGetterSetterSnakeCaseUpper {
// CHECK-NEXT:    init()
// CHECK-NEXT:    init(val: Int32)
// CHECK-NEXT:    func Get_X() -> Int32
// CHECK-NEXT:    mutating func Set_X(_ v: Int32)
// CHECK-NEXT:    var val: Int32
// CHECK-NEXT: }

// CHECK:      struct IntGetterSetterSnakeCase {
// CHECK-NEXT:    init()
// CHECK-NEXT:    init(val: Int32)
// CHECK-NEXT:    var x: Int32
// CHECK-NEXT:    func get_x() -> Int32
// CHECK-NEXT:    mutating func set_x(_ v: Int32)
// CHECK-NEXT:    var val: Int32
// CHECK-NEXT: }

// CHECK:      struct GetterHasArg {
// CHECK-NEXT:    init()
// CHECK-NEXT:    func getX(_ v: Int32) -> Int32
// CHECK-NEXT:    mutating func setX(_ v: Int32)
// CHECK-NEXT: }

// CHECK:      struct GetterSetterIsUpper {
// CHECK-NEXT:    init()
// CHECK-NEXT:    init(val: Int32)
// CHECK-NEXT:    var x: Int32
// CHECK-NEXT:    func GETX() -> Int32
// CHECK-NEXT:    mutating func SETX(_ v: Int32)
// CHECK-NEXT:    var val: Int32
// CHECK-NEXT: }

// CHECK:      struct HasXAndY {
// CHECK-NEXT:    init()
// CHECK-NEXT:    init(val: Int32)
// CHECK-NEXT:    var xAndY: Int32
// CHECK-NEXT:    func GetXAndY() -> Int32
// CHECK-NEXT:    mutating func SetXAndY(_ v: Int32)
// CHECK-NEXT:    var val: Int32
// CHECK-NEXT: }

// CHECK:      struct AllUpper {
// CHECK-NEXT:    init()
// CHECK-NEXT:    init(val: Int32)
// CHECK-NEXT:    var fooandbar: Int32
// CHECK-NEXT:    func GETFOOANDBAR() -> Int32
// CHECK-NEXT:    mutating func SETFOOANDBAR(_ v: Int32)
// CHECK-NEXT:    var val: Int32
// CHECK-NEXT: }

// CHECK:      struct BothUpper {
// CHECK-NEXT:    init()
// CHECK-NEXT:    init(val: Int32)
// CHECK-NEXT:    var fooAndBAR: Int32
// CHECK-NEXT:    func getFOOAndBAR() -> Int32
// CHECK-NEXT:    mutating func setFOOAndBAR(_ v: Int32)
// CHECK-NEXT:    var val: Int32
// CHECK-NEXT: }

// CHECK:      struct FirstUpper {
// CHECK-NEXT:    init()
// CHECK-NEXT:    init(val: Int32)
// CHECK-NEXT:    var fooAndBar: Int32
// CHECK-NEXT:    func getFOOAndBar() -> Int32
// CHECK-NEXT:    mutating func setFOOAndBar(_ v: Int32)
// CHECK-NEXT:    var val: Int32
// CHECK-NEXT: }

// CHECK:      struct NonConstGetter {
// CHECK-NEXT:    init()
// CHECK-NEXT:    init(val: Int32)
// CHECK-NEXT:    var x: Int32 { mutating get set }
// CHECK-NEXT:    mutating func getX() -> Int32
// CHECK-NEXT:    mutating func setX(_ v: Int32)
// CHECK-NEXT:    var val: Int32
// CHECK-NEXT: }

// FIXME: rdar91961524
// TODO-CHECK:      struct ConstSetter {
// TODO-CHECK-NEXT:    init()
// TODO-CHECK-NEXT:    init(val: Int32)
// TODO-CHECK-NEXT:    var x: Int32 { mutating get set }
// TODO-CHECK-NEXT:    mutating func getX() -> Int32
// TODO-CHECK-NEXT:    mutating func setX(_ v: Int32)
// TODO-CHECK-NEXT:    var val: Int32
// TODO-CHECK-NEXT: }

// CHECK:      struct MultipleArgsSetter {
// CHECK-NEXT:    init()
// CHECK-NEXT:    var x: Int32
// CHECK-NEXT:    func getX() -> Int32
// CHECK-NEXT:    mutating func setX(_ a: Int32, _ b: Int32)
// CHECK-NEXT: }

// CHECK:      struct NonTrivial {
// CHECK-NEXT:    init()
// CHECK-NEXT:    init(value: Int32)
// CHECK-NEXT:   var value: Int32
// CHECK-NEXT: }

// CHECK:      struct PtrGetterSetter {
// CHECK-NEXT:    init()
// CHECK-NEXT:    init(value: Int32)
// CHECK-NEXT:    var x: UnsafeMutablePointer<Int32>? { mutating get set }
// CHECK-NEXT:    mutating func getX() -> UnsafeMutablePointer<Int32>!
// CHECK-NEXT:    mutating func setX(_ v: UnsafeMutablePointer<Int32>!)
// CHECK-NEXT:    var value: Int32
// CHECK-NEXT: }

// CHECK:      struct RefGetterSetter {
// CHECK-NEXT:    init()
// CHECK-NEXT:    init(value: Int32)
// CHECK-NEXT:    mutating func getX() -> UnsafePointer<Int32>
// CHECK-NEXT:    mutating func setX(_ v: Int32)
// CHECK-NEXT:    var value: Int32
// CHECK-NEXT: }

// CHECK:      struct NonTrivialGetterSetter {
// CHECK-NEXT:    init()
// CHECK-NEXT:    init(value: NonTrivial)
// CHECK-NEXT:    var x: NonTrivial { mutating get set }
// CHECK-NEXT:    mutating func getX() -> NonTrivial
// CHECK-NEXT:    mutating func setX(_ v: NonTrivial)
// CHECK-NEXT:    var value: NonTrivial
// CHECK-NEXT: }

// CHECK:      struct DifferentTypes {
// CHECK-NEXT:    init()
// CHECK-NEXT:    init(value: NonTrivial)
// CHECK-NEXT:    mutating func getX() -> NonTrivial
// CHECK-NEXT:    mutating func setX(_ v: Int32)
// CHECK-NEXT:    var value: NonTrivial
// CHECK-NEXT: }

// CHECK:      struct UTF8Str {
// CHECK-NEXT:    init()
// CHECK-NEXT:    init(value: Int32)
// CHECK-NEXT:    var utf8Str: Int32
// CHECK-NEXT:    func getUTF8Str() -> Int32
// CHECK-NEXT:    mutating func setUTF8Str(_ v: Int32)
// CHECK-NEXT:    var value: Int32
// CHECK-NEXT: }


// CHECK:      struct MethodWithSameName {
// CHECK-NEXT:    init()
// CHECK-NOT:     var value: Int32
// CHECK-NEXT:    mutating func value() -> Int32
// CHECK-NEXT:    func getValue() -> Int32
// CHECK-NEXT:    mutating func setValue(_ i: Int32)
// CHECK-NEXT: }

// CHECK:      struct PropertyWithSameName {
// CHECK-NEXT:    init()
// CHECK-NEXT:    init(value: Int32)
// CHECK-NOT:     var value: Int32
// CHECK-NEXT:    func getValue() -> Int32
// CHECK-NEXT:    mutating func setValue(_ i: Int32)
// CHECK-NEXT:    var value: Int32
// CHECK-NEXT: }

// CHECK:      struct PrivatePropertyWithSameName {
// CHECK-NEXT:    init()
// CHECK-NEXT:    func getValue() -> Int32
// CHECK-NEXT:    mutating func setValue(_ i: Int32)
// CHECK-NEXT:    var value: Int32
// CHECK-NEXT: }

// CHECK:      struct SnakeCaseGetterSetter {
// CHECK-NEXT:    init()
// CHECK-NEXT:    init(value: Int32)
// CHECK-NEXT:    var foo: Int32
// CHECK-NEXT:    func get_foo() -> Int32
// CHECK-NEXT:    mutating func set_foo(_ v: Int32)
// CHECK-NEXT:    var value: Int32
// CHECK-NEXT: }

// CHECK:      struct SnakeCaseUTF8Str {
// CHECK-NEXT:    init()
// CHECK-NEXT:    init(value: Int32)
// CHECK-NEXT:    var utf8String: Int32
// CHECK-NEXT:    func get_utf8_string() -> Int32
// CHECK-NEXT:    mutating func set_utf8_string(_ v: Int32)
// CHECK-NEXT:    var value: Int32
// CHECK-NEXT: }

// CHECK:      struct SnakeCaseTrailing {
// CHECK-NEXT:    init()
// CHECK-NEXT:    init(value: Int32)
// CHECK-NEXT:    var x: Int32
// CHECK-NEXT:    func get_x_() -> Int32
// CHECK-NEXT:    mutating func set_x_(_ v: Int32)
// CHECK-NEXT:    var value: Int32
// CHECK-NEXT: }