// RUN: %swift -disable-objc-attr-requires-foundation-module -target x86_64-apple-macosx10.9 %s -import-objc-header %S/Inputs/serialized-objc-header.h -emit-ir -g -o - | FileCheck %s

protocol Named {
    var name : String { get }
}

// initializer.Person.init (initializer.Person.Type)() -> initializer.Person
// CHECK: define hidden %C11initializer6Person* @_TFC11initializer6PersoncfMS0_FT_S0_(%C11initializer6Person*) {

// initializer.Person.__allocating_init (initializer.Person.Type)() -> initializer.Person
// CHECK: define hidden %C11initializer6Person* @_TFC11initializer6PersonCfMS0_FT_S0_(%swift.type*) {
// CHECK:  call %C11initializer6Person* @_TFC11initializer6PersoncfMS0_FT_S0_(%C11initializer6Person* %3), !dbg ![[ALLOCATING_INIT:.*]]

// CHECK-DAG: ![[ALLOCATING_INIT]]  = !MDLocation(line: 0, scope
class Person : Named {
    var name : String { get { return "No Name" } }
    var age = 0
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

    var pair = Pair(first: Suits.Hearts, second: Suits.Diamonds)

    var function = test
    var range = 0..<100
    var optionalNone : Float? = .None
    var optionalSome : Float? = 100
    var tuple = (i8, i16, i32, i64, string)
    var person = Person()
    var point = Point(x: 10.0, y: 20.0)
    var object = ObjCClass()

    var arrayInt = [10, 20, 30]
    var arrayOptionalInt : [Int?] = [10, .None, 30, .None, 20]
    var arrayString = ["Kate", "Sean", "Barry"]
    var arrayPerson = [Person(), Person()]
    var dictionary = [1 : "Kate", 2 : "Sean", 3 : "Barry"]

    var sortedArrayString = sorted(["C", "B", "A"],
    {
        $0 < $1
    })

    echo(string)
    echo(tuple)
    echo(point)

    println("Hello, World!")
}

test()
