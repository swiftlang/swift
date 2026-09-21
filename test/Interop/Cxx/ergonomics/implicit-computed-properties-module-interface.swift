// RUN: %target-swift-ide-test -print-module -module-to-print=ImplicitComputedProperties -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop -cxx-interop-getters-setters-as-properties | %FileCheck %s

// CHECK:      struct VoidGetter {
// CHECK-NOT:     var
// CHECK-NEXT:    init()
// CHECK-NEXT:    mutating func getX()
// CHECK-NEXT:    mutating func setX(_: CInt)
// CHECK-NEXT: }

// CHECK:      struct VoidSetterNoName {
// CHECK-NOT:     var
// CHECK-NEXT:    init()
// CHECK-NEXT:    mutating func set()
// CHECK-NEXT: }

// CHECK:      struct IllegalIntReturnSetter {
// CHECK-NOT:     var
// CHECK-NEXT:    init()
// CHECK-NEXT:    mutating func setX(_: CInt) -> CInt
// CHECK-NEXT: }

// CHECK:      struct TwoParameterSetter {
// CHECK-NOT:     var
// CHECK-NEXT:    init()
// CHECK-NEXT:    mutating func setX(_: CInt, _: CInt)
// CHECK-NEXT: }

// CHECK:      struct NoNameSetter {
// CHECK-NOT:     var
// CHECK-NEXT:    init()
// CHECK-NEXT:    mutating func set(_: CInt)
// CHECK-NEXT: }

// CHECK:      struct NoNameVoidGetter {
// CHECK-NOT:     var
// CHECK-NEXT:    init()
// CHECK-NEXT:    mutating func get()
// CHECK-NEXT: }

// CHECK:      struct LongNameAllLower {
// CHECK-NEXT:    init(value: CInt)
// CHECK-NEXT:    init()
// CHECK-NEXT:    var value: CInt
// CHECK-NEXT:    func getfoo() -> CInt
// CHECK-NEXT:    mutating func setfoo(_ v: CInt)
// CHECK-NEXT:    var foo: CInt
// CHECK-NEXT: }

// CHECK:      struct LongNameAllUpper {
// CHECK-NEXT:     init(value: CInt)
// CHECK-NEXT:     init()
// CHECK-NEXT:     var value: CInt
// CHECK-NEXT:     func getFOO() -> CInt
// CHECK-NEXT:     mutating func setFOO(_ v: CInt)
// CHECK-NEXT:     var foo: CInt
// CHECK-NEXT: }

// CHECK:      struct UpperCaseMix {
// CHECK-NEXT:    init(value: CInt)
// CHECK-NEXT:    init()
// CHECK-NEXT:    var value: CInt
// CHECK-NEXT:    func getFoo() -> CInt
// CHECK-NEXT:    mutating func SetFoo(_ v: CInt)
// CHECK-NEXT:    var foo: CInt
// CHECK-NEXT: }

// CHECK:      struct UpperCaseGetterSetter {
// CHECK-NEXT:    init(value: CInt)
// CHECK-NEXT:    init()
// CHECK-NEXT:    var value: CInt
// CHECK-NEXT:    func GetFoo() -> CInt
// CHECK-NEXT:    mutating func SetFoo(_ v: CInt)
// CHECK-NEXT:    var foo: CInt
// CHECK-NEXT: }

// CHECK:      struct GetterOnly {
// CHECK-NEXT:    init()
// CHECK-NEXT:    func getFoo() -> CInt
// CHECK-NEXT:    var foo: CInt { get }
// CHECK-NEXT: }

// CHECK:      struct NoNameUpperGetter {
// CHECK-NOT:     var
// CHECK-NEXT:    init()
// CHECK-NEXT:    mutating func Getter() -> CInt
// CHECK-NEXT: }

// CHECK:      struct NotypeSetter {
// CHECK-NEXT:    init()
// CHECK-NEXT:    mutating func setX()
// CHECK-NEXT: }

// CHECK:      struct IntGetterSetter {
// CHECK-NEXT:    init(val: CInt)
// CHECK-NEXT:    init()
// CHECK-NEXT:    var val: CInt
// CHECK-NEXT:    func getX() -> CInt
// CHECK-NEXT:    mutating func setX(_ v: CInt)
// CHECK-NEXT:    var x: CInt
// CHECK-NEXT: }

// CHECK:      struct IntGetterSetterSnakeCaseUpper {
// CHECK-NEXT:    init(val: CInt)
// CHECK-NEXT:    init()
// CHECK-NEXT:    var val: CInt
// CHECK-NEXT:    func Get_X() -> CInt
// CHECK-NEXT:    mutating func Set_X(_ v: CInt)
// CHECK-NEXT: }

// CHECK:      struct IntGetterSetterSnakeCase {
// CHECK-NEXT:    init(val: CInt)
// CHECK-NEXT:    init()
// CHECK-NEXT:    var val: CInt
// CHECK-NEXT:    func get_x() -> CInt
// CHECK-NEXT:    mutating func set_x(_ v: CInt)
// CHECK-NEXT:    var x: CInt
// CHECK-NEXT: }

// CHECK:      struct GetterHasArg {
// CHECK-NEXT:    init()
// CHECK-NEXT:    func getX(_ v: CInt) -> CInt
// CHECK-NEXT:    mutating func setX(_ v: CInt)
// CHECK-NEXT: }

// CHECK:      struct GetterSetterIsUpper {
// CHECK-NEXT:    init(val: CInt)
// CHECK-NEXT:    init()
// CHECK-NEXT:    var val: CInt
// CHECK-NEXT:    func GETX() -> CInt
// CHECK-NEXT:    mutating func SETX(_ v: CInt)
// CHECK-NEXT:    var x: CInt
// CHECK-NEXT: }

// CHECK:      struct HasXAndY {
// CHECK-NEXT:    init(val: CInt)
// CHECK-NEXT:    init()
// CHECK-NEXT:    var val: CInt
// CHECK-NEXT:    func GetXAndY() -> CInt
// CHECK-NEXT:    mutating func SetXAndY(_ v: CInt)
// CHECK-NEXT:    var xAndY: CInt
// CHECK-NEXT: }

// CHECK:      struct AllUpper {
// CHECK-NEXT:    init(val: CInt)
// CHECK-NEXT:    init()
// CHECK-NEXT:    var val: CInt
// CHECK-NEXT:    func GETFOOANDBAR() -> CInt
// CHECK-NEXT:    mutating func SETFOOANDBAR(_ v: CInt)
// CHECK-NEXT:    var fooandbar: CInt
// CHECK-NEXT: }

// CHECK:      struct BothUpper {
// CHECK-NEXT:    init(val: CInt)
// CHECK-NEXT:    init()
// CHECK-NEXT:    var val: CInt
// CHECK-NEXT:    func getFOOAndBAR() -> CInt
// CHECK-NEXT:    mutating func setFOOAndBAR(_ v: CInt)
// CHECK-NEXT:    var fooAndBAR: CInt
// CHECK-NEXT: }

// CHECK:      struct FirstUpper {
// CHECK-NEXT:    init(val: CInt)
// CHECK-NEXT:    init()
// CHECK-NEXT:    var val: CInt
// CHECK-NEXT:    func getFOOAndBar() -> CInt
// CHECK-NEXT:    mutating func setFOOAndBar(_ v: CInt)
// CHECK-NEXT:    var fooAndBar: CInt
// CHECK-NEXT: }

// CHECK:      struct NonConstGetter {
// CHECK-NEXT:    init(val: CInt)
// CHECK-NEXT:    init()
// CHECK-NEXT:    var val: CInt
// CHECK-NEXT:    mutating func getX() -> CInt
// CHECK-NEXT:    mutating func setX(_ v: CInt)
// CHECK-NEXT:    var x: CInt { mutating get set }
// CHECK-NEXT: }

// FIXME: rdar91961524
// TODO-CHECK:      struct ConstSetter {
// TODO-CHECK-NEXT:    init()
// TODO-CHECK-NEXT:    init(val: CInt)
// TODO-CHECK-NEXT:    var x: CInt { mutating get set }
// TODO-CHECK-NEXT:    mutating func getX() -> CInt
// TODO-CHECK-NEXT:    mutating func setX(_ v: CInt)
// TODO-CHECK-NEXT:    var val: CInt
// TODO-CHECK-NEXT: }

// CHECK:      struct MultipleArgsSetter {
// CHECK-NEXT:    init()
// CHECK-NEXT:    func getX() -> CInt
// CHECK-NEXT:    mutating func setX(_ a: CInt, _ b: CInt)
// CHECK-NEXT:    var x: CInt { get }
// CHECK-NEXT: }

// CHECK:      struct NonTrivial {
// CHECK-NEXT:    init(value: CInt)
// CHECK-NEXT:    init()
// CHECK-NEXT:    var value: CInt
// CHECK-NEXT: }

// CHECK:      struct PtrGetterSetter {
// CHECK-NEXT:    init(value: CInt)
// CHECK-NEXT:    init()
// CHECK-NEXT:    var value: CInt
// CHECK-NEXT:    mutating func getX() -> UnsafeMutablePointer<CInt>!
// CHECK-NEXT:    mutating func setX(_ v: UnsafeMutablePointer<CInt>!)
// CHECK-NEXT:    var x: UnsafeMutablePointer<CInt>? { mutating get set }
// CHECK-NEXT: }

// CHECK:      struct RefGetterSetter {
// CHECK-NEXT:    init(value: CInt)
// CHECK-NEXT:    init()
// CHECK-NEXT:    var value: CInt
// CHECK-NEXT:    mutating func getX() -> UnsafePointer<CInt>
// CHECK-NEXT:    mutating func setX(_ v: CInt)
// CHECK-NEXT: }

// CHECK:      struct NonTrivialGetterSetter {
// CHECK-NEXT:    init(value: NonTrivial)
// CHECK-NEXT:    init()
// CHECK-NEXT:    var value: NonTrivial
// CHECK-NEXT:    mutating func getX() -> NonTrivial
// CHECK-NEXT:    mutating func setX(_ v: NonTrivial)
// CHECK-NEXT:    var x: NonTrivial { mutating get set }
// CHECK-NEXT: }

// CHECK:      struct DifferentTypes {
// CHECK-NEXT:    init(value: NonTrivial)
// CHECK-NEXT:    init()
// CHECK-NEXT:    var value: NonTrivial
// CHECK-NEXT:    mutating func getX() -> NonTrivial
// CHECK-NEXT:    mutating func setX(_ v: CInt)
// CHECK-NEXT: }

// CHECK:      struct UTF8Str {
// CHECK-NEXT:    init(value: CInt)
// CHECK-NEXT:    init()
// CHECK-NEXT:    var value: CInt
// CHECK-NEXT:    func getUTF8Str() -> CInt
// CHECK-NEXT:    mutating func setUTF8Str(_ v: CInt)
// CHECK-NEXT:    var utf8Str: CInt
// CHECK-NEXT: }


// CHECK:      struct MethodWithSameName {
// CHECK-NEXT:    init()
// CHECK-NOT:     var value: CInt
// CHECK-NEXT:    mutating func value() -> CInt
// CHECK-NEXT:    func getValue() -> CInt
// CHECK-NEXT:    mutating func setValue(_ i: CInt)
// CHECK-NEXT: }

// CHECK:      struct PropertyWithSameName {
// CHECK-NEXT:    init(value: CInt)
// CHECK-NEXT:    init()
// CHECK-NEXT:    var value: CInt
// CHECK-NEXT:    func getValue() -> CInt
// CHECK-NEXT:    mutating func setValue(_ i: CInt)
// CHECK-NEXT: }

// CHECK:      struct PrivatePropertyWithSameName {
// CHECK-NEXT:    init()
// CHECK-NEXT:    var value: CInt
// CHECK-NEXT:    func getValue() -> CInt
// CHECK-NEXT:    mutating func setValue(_ i: CInt)
// CHECK-NEXT: }

// CHECK:      struct SnakeCaseGetterSetter {
// CHECK-NEXT:    init(value: CInt)
// CHECK-NEXT:    init()
// CHECK-NEXT:    var value: CInt
// CHECK-NEXT:    func get_foo() -> CInt
// CHECK-NEXT:    mutating func set_foo(_ v: CInt)
// CHECK-NEXT:    var foo: CInt
// CHECK-NEXT: }

// CHECK:      struct SnakeCaseUTF8Str {
// CHECK-NEXT:    init(value: CInt)
// CHECK-NEXT:    init()
// CHECK-NEXT:    var value: CInt
// CHECK-NEXT:    func get_utf8_string() -> CInt
// CHECK-NEXT:    mutating func set_utf8_string(_ v: CInt)
// CHECK-NEXT:    var utf8String: CInt
// CHECK-NEXT: }

// CHECK:      struct SnakeCaseTrailing {
// CHECK-NEXT:    init(value: CInt)
// CHECK-NEXT:    init()
// CHECK-NEXT:    var value: CInt
// CHECK-NEXT:    func get_x_() -> CInt
// CHECK-NEXT:    mutating func set_x_(_ v: CInt)
// CHECK-NEXT:    var x: CInt
// CHECK-NEXT: }
