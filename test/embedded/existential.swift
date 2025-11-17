// RUN: %target-swift-frontend -enable-experimental-feature EmbeddedExistentials -enable-experimental-feature Embedded -parse-as-library -wmo -emit-sil %s | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature EmbeddedExistentials -enable-experimental-feature Embedded -parse-as-library -wmo) | %FileCheck %s --check-prefix=OUTPUT
// RUN: %target-run-simple-swift(-enable-experimental-feature EmbeddedExistentials -enable-experimental-feature Embedded -parse-as-library -wmo -O) | %FileCheck %s --check-prefix=OUTPUT

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedExistentials

class CP {
}

class C : CP {
    func foo() { }
}

class GC<T> {
    var x: T? = nil
    func foo() {}
    deinit {
        print("deinit called")
    }
}

struct StructWithClass {
    var c = GC<Int>()
}

struct GenericStructWithClass<T> {
    var c = GC<T>()
    var d = GC<T>()
}

enum EnumWithClass {
    case a
    case c(GC<Int>)
}

enum GenericEnumWithClass<T> {
    case a
    case c(GC<T>)
}

// CHECK: sil @$e11existential4testyyF
// CHECK: init_existential_addr
// CHECK: } // end sil function '$e11existential4testyyF'

// There are 8 class instances that are destroyed.

// OUTPUT:  deinit called
// OUTPUT:  deinit called
// OUTPUT:  deinit called
// OUTPUT:  deinit called

// OUTPUT:  deinit called
// OUTPUT:  deinit called
// OUTPUT:  deinit called
// OUTPUT:  deinit called

// OUTPUT:  deinit called
// OUTPUT:  deinit called
// OUTPUT:  deinit called
// OUTPUT:  deinit called

// OUTPUT-NOT:  deinit called

func test() {
    let _: any Any = GC<Int>()
    let _: any Any = 3
    let _: any Any = StructWithClass()
    let _: any Any = GenericStructWithClass<Int>()
    let _: any Any = EnumWithClass.c(GC<Int>())
    let _: any Any = GenericEnumWithClass.c(GC<Int>())
    let _: any Any = (3, 4)
    let _: any Any = (StructWithClass(), StructWithClass())
    // outline storage case
    let _: any Any = (StructWithClass(), StructWithClass(), StructWithClass(), StructWithClass())
}

protocol Basic  {
  func a()
}

protocol Derived : Basic {
  func b()
}

class Implementor : Derived {
  func a() { print("a") }
  func b() { print("b") }
}

extension Int : Derived {
  func a() { print("a Int \(self)") }
  func b() { print("b Int \(self)") }
}

struct MyStruct : Derived {
  var x = 5
  func a() { print("a MyStruct \(self.x)") }
  func b() { print("b MyStruct") }
}

struct LargeMyStruct : Derived {
  var x = (1, 2, 3, 4, 5)
  var refCounted = StructWithClass()

  func a() { print("a LargeMyStruct \(self.x.4)") }
  func b() { print("b LargeMyStruct") }
}

enum MyEnum : Derived {
    case a
    case b(Int)

  func a() {
    print("a MyEnum ")
    switch self  {
      case .a: break
      case .b(let x):
        print(x)
    }
  }

  func b() {
    print("b MyEnum ")
  }
}

func test2(_ p: any Derived) {
  p.a()
  p.b()
}


protocol ValuePrinter {
    func printValue()
    mutating func mutate()
}
protocol WithAssoc {
    associatedtype Assoc : ValuePrinter
    func a() -> Assoc
}

extension Int : ValuePrinter {
    func printValue() {
        print("my value: \(self)")
    }
    mutating func mutate() {
        self = 8
        print("my value (mutating expect 8): \(self)")
    }
}

extension LargeMyStruct : ValuePrinter {
    func printValue() {
        print("my value of LargeMyStruct: \(self.x.4)")
    }
    mutating func mutate() {
        self.x = (6, 7, 8, 9, 10)
        print("my value of LargeMyStruct (mutating expect 10): \(self.x.4)")
    }
}

struct ConformWithAssoc : WithAssoc {
  var x = 1
  func a() -> Int {
    return x
  }
}

struct ConformWithLargeAssoc : WithAssoc {
  var x = LargeMyStruct()

  func a() -> LargeMyStruct {
    return x
  }
}

func test3(_ p: any WithAssoc) {
  let x = p.a()
  x.printValue()
}

func test4(_ p: any WithAssoc) {
  var x = p.a()
  let c = x
  x.mutate()
  c.printValue()
}

@main
struct Main {
  static func main() {
    test()

    test2(Implementor())
// OUTPUT: a
// OUTPUT: b
    test2(5)
// OUTPUT: a Int 5
// OUTPUT: b Int 5
    test2(MyStruct())
// OUTPUT: a MyStruct 5
// OUTPUT: b MyStruct
    test2(MyEnum.b(5))
// OUTPUT: a MyEnum
// OUTPUT: 5
// OUTPUT: b MyEnum
    test3(ConformWithAssoc())
// OUTPUT: my value: 1
    test3(ConformWithLargeAssoc())
// OUTPUT: my value of LargeMyStruct: 5
// OUTPUT:  deinit called
    test4(ConformWithAssoc())
// OUTPUT: my value (mutating expect 8): 8
// OUTPUT: my value: 1
    test4(ConformWithLargeAssoc())
// OUTPUT: my value of LargeMyStruct (mutating expect 10): 10
// OUTPUT: my value of LargeMyStruct: 5
// OUTPUT:  deinit called
// OUTPUT-NOT:  deinit called
  }
}
