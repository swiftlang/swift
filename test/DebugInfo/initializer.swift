// RUN: %swift -triple x86_64-apple-darwin13 %s -emit-llvm -g -o - | FileCheck %s

import Foundation

protocol Named {
    var name : String
}

// initializer.Person.__allocating_init (initializer.Person.metatype)() -> initializer.Person
// CHECK: define %C11initializer6Person* @_TC11initializer6PersonCfMS0_FT_S0_(%swift.type*) {
// CHECK:  call %C11initializer6Person* @_TC11initializer6PersoncfMS0_FT_S0_(%C11initializer6Person* %2), !dbg ![[BEGIN:.*]]
// CHECK:   ret %C11initializer6Person* %3, !dbg ![[END1:.*]]

// initializer.Person.init (initializer.Person.metatype)() -> initializer.Person
// CHECK: define %C11initializer6Person* @_TC11initializer6PersoncfMS0_FT_S0_(%C11initializer6Person*) {
// CHECK: call { i8*, i64, %swift.refcounted* } @_TSS32_convertFromBuiltinStringLiteralfMSSFT5valueBp8byteSizeBi64_7isASCIIBi1__SS(i8* getelementptr inbounds ([8 x i8]* @0, i64 0, i64 0), i64 7, i1 true), !dbg ![[NAMEINIT:.*]]
// CHECK: ret %C11initializer6Person* %0, !dbg ![[END2:.*]]

// CHECK-DAG: ![[BEGIN]] = metadata !{i32 [[@LINE+1]],
class Person : Named {
// CHECK-DAG: ![[NAMEINIT]] = metadata !{i32 [[@LINE+1]],
    var name = "No Name"
    var age = 0
// CHECK-DAG: ![[END2]] = metadata !{i32 [[@LINE+2]],
// CHECK-DAG: ![[END1]] = metadata !{i32 [[@LINE+1]],
}

struct Pair<T> {
    var first : T
    var second : T
}

struct Point {
    var x : Double
    var y : Double
}

enum Suits {
    case Clubs
    case Diamonds
    case Hearts
    case Spades
}

func echo<T>(input : T) -> T {
    return input
}

func test() {
    var string = "Hello, World!"

    var i8 : Int8 = 8
    var i16 : Int16 = 16
    var i32 : Int32 = 32
    var i64 : Int64 = 64

    var u8 : UInt8 = 8
    var u16 : UInt16 = 16
    var u32 : UInt32 = 32
    var u64 : UInt64 = 64

    var float : Float = 32
    var double : Double = 64

    var suitEnum = Suits.Clubs

    var pair = Pair(Suits.Hearts, Suits.Diamonds)

    var function = test
    var range = 0..100
    var optionalNone : Float? = .None
    var optionalSome : Float? = 100
    var tuple = (i8, i16, i32, i64, string)
    var person = Person()
    var point = Point(10.0, 20.0)
    var object = NSObject()

    var arrayInt = [10, 20, 30]
    var arrayOptionalInt : Int?[] = [10, .None, 30, .None, 20]
    var arrayString = ["Kate", "Sean", "Barry"]
    var arrayPerson = [Person(), Person()]
    var dictionary = [1 : "Kate", 2 : "Sean", 3 : "Barry"]

    var sortedArrayString = sort(["C", "B", "A"],
    {
        $0 < $1
    })

    echo(string)
    echo(tuple)
    echo(point)

    println("Hello, World!")
}

test()
