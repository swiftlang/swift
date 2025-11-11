// RUN: %target-swift-frontend -enable-experimental-feature EmbeddedExistentials -enable-experimental-feature Embedded -parse-as-library -wmo -emit-sil %s | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature EmbeddedExistentials -enable-experimental-feature Embedded -parse-as-library -wmo) | %FileCheck %s --check-prefix=OUTPUT

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
}
protocol WithAssoc {
    associatedtype Assoc : ValuePrinter
    func a() -> Assoc
}

extension Int : ValuePrinter {
    func printValue() {
        print("my value: \(self)")
    }
}

struct ConformWithAssoc : WithAssoc {
  var x = 1
  func a() -> Int {
    return x
  }
}

func test3(_ p: any WithAssoc) {
  let x = p.a()
  x.printValue()
}

@main
struct Main {
  static func main() {
    test()

// OUTPUT: a
// OUTPUT: b
// OUTPUT: a Int 5
// OUTPUT: b Int 5
// OUTPUT: a MyStruct 5
// OUTPUT: b MyStruct
// OUTPUT: a MyEnum
// OUTPUT: 5
// OUTPUT: b MyEnum
// OUTPUT: my value: 1
    test2(Implementor())
    test2(5)
    test2(MyStruct())
    test2(MyEnum.b(5))
    test3(ConformWithAssoc())
  }
}
