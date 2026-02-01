// RUN: %target-swift-frontend -enable-experimental-feature EmbeddedExistentials -enable-experimental-feature Embedded -parse-as-library -wmo -emit-sil %s | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature EmbeddedExistentials -enable-experimental-feature Embedded -parse-as-library -wmo) | %FileCheck %s --check-prefix=OUTPUT
// RUN: %target-run-simple-swift(-enable-experimental-feature EmbeddedExistentials -enable-experimental-feature Embedded -parse-as-library -wmo -O) | %FileCheck %s --check-prefix=OUTPUT

// RUN: not %target-swift-frontend -enable-experimental-feature EmbeddedExistentials -parse-as-library -wmo -emit-sil %s 2>&1 | %FileCheck --check-prefix=ERRMSG %s

// EmbeddedExistentials is the default.
// RUN: %target-run-simple-swift( -enable-experimental-feature Embedded -parse-as-library -wmo) | %FileCheck %s --check-prefix=OUTPUT

// Test -disable-experimental-feature EmbeddedExistentials
// RUN: not %target-swift-frontend -disable-experimental-feature EmbeddedExistentials -enable-experimental-feature Embedded -parse-as-library -wmo -emit-sil %s 2>&1 | %FileCheck --check-prefix=ERRMSG2 %s
// RUN: not %target-swift-frontend -enable-experimental-feature Embedded -disable-experimental-feature EmbeddedExistentials -parse-as-library -wmo -emit-sil %s 2>&1 | %FileCheck --check-prefix=ERRMSG2 %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedExistentials

// EmbeddedExistentials requires Embedded
// ERRMSG: error: EmbeddedExistentials requires enabling embedded Swift.

// -disable-experimental-feature EmbeddedExistentials
// ERRMSG2: error: cannot use a value of protocol type 'Any' in embedded Swift

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
// OUTPUT: hello world

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
    let c: any Any = { print("hello world") }
    if let cl = c as? () -> () {
        cl()
    }
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

func test5(_ p: any Any) {
  print("test any as? MyStruct")
  if let c = p as? MyStruct {
      print("success")
      c.a()
  } else {
      print("cast failed")
  }
}

func test6(_ p: any Any) {
  print("test any as? LargeMyStruct")
  if let c = p as? LargeMyStruct {
      print("success")
      c.a()
  } else {
      print("cast failed")
  }
}

func test7(_ p: any Any) {
  print("test any as! LargeMyStruct")
  let c = p as! LargeMyStruct
  c.a()
}

class BaseClass {
  func foo() { print("BaseClass.foo") }
  deinit {
    print("BaseClass.deinit")
  }
}

class SubClass : BaseClass {
  override func foo() { print("SubClass.foo") }
}

func test8(_ p: any Any) {
  print("test any as? SubClass")
  if let c = p as? SubClass {
    print("success")
    c.foo()
  } else {
    print("cast failed")
  }
}

func test9(_ p: any Any) {
  print("test any as? BaseClass")
  if let c = p as? BaseClass {
    print("success")
    c.foo()
  } else {
    print("cast failed")
  }
}

func test10(_ p: any Any) {
  print("test any as! BaseClass")
  let c = p as! BaseClass
  c.foo()
}

func test11(_ p: any Any) {
  print("test any as! SubClass")
  let c = p as! SubClass
  c.foo()
}

func test12(_ p: any Any) {
  print("test any as! (Int, Int, Int, Int)")
  if let c = p as? (Int, Int, Int, Int) {
    print("success")
    print("tuple: \(c.0)")
  } else {
    print("cast failed")
  }
}

protocol Q {
  func printit()
}

protocol P4<T> {
  associatedtype T: Q

  var t: T { get }
}

struct QConformer : Q {
  var x = (0, 1, 2,3)

  func printit() {
      print("QConformer \(x.3)")
  }
}

struct P4Conformer : P4 {
  var q = QConformer()

  var t : QConformer {
    get {
      return q
    }
  }
}

func test13(_ p: any P4) {
  print("test13")
  p.t.printit()
}

struct GenericConformer<T> : ValuePrinter {
  var t: T?
  var l = (0, 1, 2, 3)

  init(_ t: T) { self.t = t }

  func printValue() {
    print("GenericConformer \(l.0) \(l.1) \(l.2) \(l.3)")
  }

  mutating func mutate() {
    l = (4, 5, 6, 7)
  }
}

struct GenericConformerWithAssoc<T> : WithAssoc {
  var g : GenericConformer<T>

  init( _ g: T) {
    self.g = GenericConformer(g)
  }

  func a() -> GenericConformer<T> {
    return g
  }
}

func test14(_ p: any ValuePrinter) {
  print("test any ValuePrinter")
  p.printValue()
  var p2 = p
  p2.mutate()
  p2.printValue()
}

func test15(_ p: any WithAssoc) {
  print("test any WithAssoc")
  let l = p.a()
  l.printValue()
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

    test5(MyStruct())
// OUTPUT: test any as? MyStruct
// OUTPUT: success
// OUTPUT: a MyStruct 5
    test5(GC<Int>())
// OUTPUT: test any as? MyStruct
// OUTPUT: cast failed
// OUTPUT:  deinit called
// OUTPUT-NOT:  deinit called
    test5(LargeMyStruct())
// OUTPUT: test any as? MyStruct
// OUTPUT: cast failed
// OUTPUT:  deinit called
// OUTPUT-NOT:  deinit called
    test5(GenericStructWithClass<Int>())
// OUTPUT: test any as? MyStruct
// OUTPUT: cast failed
// OUTPUT:  deinit called
// OUTPUT:  deinit called
// OUTPUT-NOT:  deinit called
    test6(MyStruct())
// OUTPUT: test any as? LargeMyStruct
// OUTPUT: cast failed
// OUTPUT-NOT:  deinit called
    test6(LargeMyStruct())
// OUTPUT: test any as? LargeMyStruct
// OUTPUT: success
// OUTPUT: a LargeMyStruct 5
// OUTPUT:  deinit called
// OUTPUT-NOT:  deinit called
    test6(GenericStructWithClass<Int>())
// OUTPUT: test any as? LargeMyStruct
// OUTPUT: cast failed
// OUTPUT:  deinit called
// OUTPUT:  deinit called
// OUTPUT-NOT:  deinit called
    test7(LargeMyStruct())
// OUTPUT: test any as! LargeMyStruct
// OUTPUT: a LargeMyStruct 5
// OUTPUT:  deinit called
// OUTPUT-NOT:  deinit called
    test8(SubClass())
// OUTPUT:  success
// OUTPUT:  SubClass.foo
// OUTPUT:  BaseClass.deinit
// OUTPUT-NOT:  deinit
    test8(BaseClass())
// OUTPUT: test any as? SubClass
// OUTPUT: cast failed
// OUTPUT: BaseClass.deinit
// OUTPUT-NOT:  deinit
    test9(SubClass())
// OUTPUT: test any as? BaseClass
// OUTPUT:  success
// OUTPUT:  SubClass.foo
// OUTPUT:  BaseClass.deinit
// OUTPUT-NOT:  deinit
    test9(BaseClass())
// OUTPUT: test any as? BaseClass
// OUTPUT:  success
// OUTPUT:  BaseClass.foo
// OUTPUT:  BaseClass.deinit
// OUTPUT-NOT:  deinit
    test9(C())
// OUTPUT: test any as? BaseClass
// OUTPUT: cast failed
// OUTPUT-NOT:  deinit
    test10(BaseClass())
// OUTPUT: test any as! BaseClass
// OUTPUT:  BaseClass.foo
// OUTPUT:  BaseClass.deinit
// OUTPUT-NOT:  deinit
    test10(SubClass())
// OUTPUT: test any as! BaseClass
// OUTPUT:  SubClass.foo
// OUTPUT:  BaseClass.deinit
// OUTPUT-NOT:  deinit
    test11(SubClass())
// OUTPUT: test any as! SubClass
// OUTPUT:  SubClass.foo
// OUTPUT:  BaseClass.deinit
// OUTPUT-NOT:  deinit
    test12((0, 1, 2, 3))
// OUTPUT: test any as! (Int, Int, Int, Int)
// OUTPUT:   success
// OUTPUT:   tuple: 0
    test13(P4Conformer())
// OUTPUT: test13
// OUTPUT:  QConformer 3
// OUTPUT-NOT:  deinit
    test14(GenericConformer(1))
// OUTPUT: test any ValuePrinter
// OUTPUT:  GenericConformer 0 1 2 3
// OUTPUT:  GenericConformer 4 5 6 7
    test15(GenericConformerWithAssoc(1))
// OUTPUT: test any WithAssoc
// OUTPUT:  GenericConformer 0 1 2 3
  }
}
