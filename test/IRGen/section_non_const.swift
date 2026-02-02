// RUN: %target-swift-frontend -enable-experimental-feature CompileTimeValuesPreview -parse-as-library -emit-sil %s -o /dev/null -verify

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_CompileTimeValuesPreview

// TODO: this should only emit one error per bad compile-time expression, not two

@section("__TEXT,__mysection") var g0: Int = 1
@section("__TEXT,__mysection") var g1: (Int, Int) = (1, 2)
@section("__TEXT,__mysection") var g2: [Int] = [1, 2, 3] // expected-error {{global variable must be a compile-time constant to use @section attribute}}
// expected-error@-1 {{'@const' value should be initialized with a compile-time value}}
@section("__TEXT,__mysection") var g3: [Int:Int] = [:] // expected-error {{global variable must be a compile-time constant to use @section attribute}}
// expected-error@-1 {{'@const' value should be initialized with a compile-time value}}
@section("__TEXT,__mysection") var g4: UInt = 42
@section("__TEXT,__mysection") var g5: String = "hello" // expected-error {{global variable must be a compile-time constant to use @section attribute}}
// expected-error@-1 {{'@const' value should be initialized with a compile-time value}}
@section("__TEXT,__mysection") var g6: Any = 1 // expected-error {{global variable must be a compile-time constant to use @section attribute}}
// expected-error@-1 {{'@const' value should be initialized with a compile-time value}}
@section("__TEXT,__mysection") var g7: UInt8 = 42
@section("__TEXT,__mysection") var g8: Int = 5 * 5
@section("__TEXT,__mysection") var g9 = MemoryLayout<Int>.size
@section("__TEXT,__mysection") var g10 = MemoryLayout<Int>.stride
@section("__TEXT,__mysection") var g11 = MemoryLayout<Int>.alignment
@section("__TEXT,__mysection") var g12 = MemoryLayout<Int>.size * 2

@section("__TEXT,__mysection") var s: StaticString = "hello"

struct MyStruct1 {
    var a: [Int]
}
@section("__TEXT,__mysection") var g_MyStruct1: MyStruct1 = MyStruct1(a: [1, 2, 3]) // expected-error {{global variable must be a compile-time constant to use @section attribute}}
// expected-error@-1 {{'@const' value should be initialized with a compile-time value}}

struct MyStruct2 {
    struct SubStruct { var x: Int }
    var a: Int
    var b: SubStruct
    init(a: Int) {
        self.a = a
        self.b = SubStruct(x: self.a)
    }
}
@section("__TEXT,__mysection") var g_MyStruct2: MyStruct2 = MyStruct2(a: 42)

struct MyStruct3 {
    struct SubStruct { var x: Int }
    var a: Int
    var b: SubStruct
    init(a: Int, b: SubStruct) {
        self.a = a
        self.b = b
    }
}
@section("__TEXT,__mysection") var g_MyStruct3: MyStruct3 = MyStruct3(a: 42, b: MyStruct3.SubStruct(x: 77))

// Check that we don't end up infinitely inlining
struct MyStruct4 {
    var a: Int
    init(a: Int) {
        if a > 0 { _ = MyStruct4(a: a + 1) }
        self.a = a
    }
}
@section("__TEXT,__mysection") var g_MyStruct4: MyStruct4 = MyStruct4(a: 42) // expected-error {{global variable must be a compile-time constant to use @section attribute}}
// expected-error@-1 {{'@const' value should be initialized with a compile-time value}}

struct MyStruct5 {
    struct SubStruct { var x: Int }
    var a: (SubStruct, SubStruct)
    init(a: Int) {
        self.a = (SubStruct(x: a), SubStruct(x: a))
    }
}
@section("__TEXT,__mysection") var g_MyStruct5: MyStruct5 = MyStruct5(a: 42)

// Check that we don't end up infinitely inlining
struct MyStruct6 {
    struct SubStruct {
        var x: Int
        init(x: Int) {
            if x > 0 { _ = SubStruct(x: x + 1) }
            self.x = x
        }
    }
    var a: (SubStruct, SubStruct)
    init(a: Int) {
        self.a = (SubStruct(x: a), SubStruct(x: a))
    }
}
@section("__TEXT,__mysection") var g_MyStruct6: MyStruct6 = MyStruct6(a: 42) // expected-error {{global variable must be a compile-time constant to use @section attribute}}
// expected-error@-1 {{'@const' value should be initialized with a compile-time value}}

@section("__TEXT,__mysection") var gp1: UnsafeMutablePointer<Int>? = nil
@section("__TEXT,__mysection") var gp2: UnsafeMutablePointer<Int>? = UnsafeMutablePointer(bitPattern: 0x42424242)
