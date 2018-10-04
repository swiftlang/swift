// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

let TypeNameTests = TestSuite("TypeName")

class C {}
struct S {}
enum E {}

protocol P {}
protocol P2 {}
protocol P3 {}
protocol AssociatedTypes {
  associatedtype A
  associatedtype B
  associatedtype C
}

class Model : AssociatedTypes {
  typealias A = C
  typealias B = S
  typealias C = E
}

struct Model2 : AssociatedTypes {
  typealias A = C
  typealias B = S
  typealias C = E
}

class GC<T : AssociatedTypes> {}
struct GS<T : AssociatedTypes> {}
enum GE<T : AssociatedTypes> {}
class GC2<T : AssociatedTypes, U : AssociatedTypes> {}

TypeNameTests.test("Prints") {
  expectEqual("Swift.Int", _typeName(Int.self))
  expectEqual("main.C", _typeName(C.self))
  expectEqual("main.S", _typeName(S.self))
  expectEqual("main.E", _typeName(E.self))
  expectEqual("main.GC<main.Model>",
    _typeName(GC<Model>.self))
  expectEqual("main.GS<main.Model>",
    _typeName(GS<Model>.self))
  expectEqual("main.GE<main.Model>",
    _typeName(GE<Model>.self))
  expectEqual("main.GC2<main.Model, main.Model2>",
    _typeName(GC2<Model, Model2>.self))
  
  expectEqual("main.P", _typeName(P.self))
  typealias PP2 = P & P2
  expectEqual("main.P & main.P2",
    _typeName(PP2.self))
  expectEqual("Any", _typeName(Any.self))
  expectEqual("main.P & main.P2", _typeName((P & P2).self))

  typealias F = () -> ()
  typealias F2 = () -> () -> ()
  typealias F3 = (() -> ()) -> ()

  expectEqual("() -> ()", _typeName(F.self))
  expectEqual("() -> () -> ()", _typeName(F2.self))
  expectEqual("(() -> ()) -> ()", _typeName(F3.self))
  expectEqual("() -> ()", _typeName((() -> ()).self))

  expectEqual("(main.P) -> main.P2 & main.P3",
    _typeName(((P) -> P2 & P3).self))
  expectEqual("() -> main.P & main.P2 & main.P3",
    _typeName((() -> P & P2 & P3).self))
  expectEqual("(main.P & main.P2) -> main.P & main.P3",
    _typeName(((P & P2) -> P3 & P).self))
 
  #if _runtime(_ObjC)
  typealias B = @convention(block) () -> ()
  typealias B2 = () -> @convention(block) () -> ()
  typealias B3 = (@convention(block) () -> ()) -> ()
  expectEqual("@convention(block) () -> ()", _typeName(B.self))
  expectEqual("() -> @convention(block) () -> ()",
    _typeName(B2.self))
  expectEqual("(@convention(block) () -> ()) -> ()",
    _typeName(B3.self))
  #endif

  expectEqual("(() -> ()).Type", _typeName(F.Type.self))
  expectEqual("main.C.Type", _typeName(C.Type.self))
  expectEqual("main.C.Type.Type", _typeName(C.Type.Type.self))
  expectEqual("Any.Type", _typeName(Any.Type.self))
  expectEqual("Any.Protocol", _typeName(Any.Protocol.self))
  expectEqual("Swift.AnyObject", _typeName(AnyObject.self))
  expectEqual("Swift.AnyObject.Type", _typeName(AnyClass.self))
  expectEqual("Swift.Optional<Swift.AnyObject>",
    _typeName((AnyObject?).self))
  expectEqual("()", _typeName(Void.self))


  typealias Tup = (Any, F, C)
  expectEqual("(Any, () -> (), main.C)",
    _typeName(Tup.self))
}

TypeNameTests.test("Inout") {
  typealias IF = (inout Int) -> ()
  typealias IF2 = (inout Int) -> (inout Int) -> ()
  typealias IF3 = ((inout Int) -> ()) -> ()
  typealias IF3a = (inout ((Int) -> ())) -> ()
  typealias IF3b = (inout ((Int) -> ())) -> ()
  typealias IF3c = ((inout Int) -> ()) -> ()
  typealias IF4 = (inout (() -> ())) -> ()
  typealias IF5 = (inout Int, Any) -> ()

  expectEqual("(inout Swift.Int) -> ()", _typeName(IF.self))
  expectEqual("(inout Swift.Int) -> (inout Swift.Int) -> ()",
    _typeName(IF2.self))
  expectEqual("((inout Swift.Int) -> ()) -> ()",
    _typeName(IF3.self))
  expectEqual("(inout (Swift.Int) -> ()) -> ()",
    _typeName(IF3a.self))
  expectEqual("(inout (Swift.Int) -> ()) -> ()",
    _typeName(IF3b.self))
  expectEqual("((inout Swift.Int) -> ()) -> ()",
    _typeName(IF3c.self))
  expectEqual("(inout () -> ()) -> ()",
    _typeName(IF4.self))
  expectEqual("(inout Swift.Int, Any) -> ()",
    _typeName(IF5.self))
}

TypeNameTests.test("Functions") {
  func curry1() {

  }

  func curry1Throws() throws {

  }

  func curry2() -> () -> () {
    return curry1
  }

  func curry2Throws() throws -> () -> () {
    return curry1
  }

  func curry3() -> () throws -> () {
    return curry1Throws
  }

  func curry3Throws() throws -> () throws -> () {
    return curry1Throws
  }

  expectEqual("() -> ()",
    _typeName(type(of: curry1)))
  expectEqual("() -> () -> ()",
    _typeName(type(of: curry2)))
  expectEqual("() throws -> () -> ()",
    _typeName(type(of: curry2Throws)))
  expectEqual("() -> () throws -> ()",
    _typeName(type(of: curry3)))
  expectEqual("() throws -> () throws -> ()",
    _typeName(type(of: curry3Throws)))
}

class SomeOuterClass {
  struct SomeInnerStruct {}
  struct SomeInnerGenericStruct<T> {}
}

class SomeOuterGenericClass<T> {
  struct SomeInnerStruct {}
  struct SomeInnerGenericStruct<U> {}
}

TypeNameTests.test("Nested") {
  expectEqual("main.SomeOuterClass.SomeInnerStruct",
              _typeName(SomeOuterClass.SomeInnerStruct.self));
  expectEqual("main.SomeOuterClass.SomeInnerGenericStruct<Swift.Int>",
              _typeName(SomeOuterClass.SomeInnerGenericStruct<Int>.self));
  expectEqual("main.SomeOuterGenericClass<Swift.Int>.SomeInnerStruct",
              _typeName(SomeOuterGenericClass<Int>.SomeInnerStruct.self));
  expectEqual("main.SomeOuterGenericClass<Swift.String>.SomeInnerGenericStruct<Swift.Int>",
              _typeName(SomeOuterGenericClass<String>.SomeInnerGenericStruct<Int>.self));
}

extension SomeOuterGenericClass {
  struct OtherInnerStruct {}
  struct OtherInnerGenericStruct<U> {}
}

TypeNameTests.test("NestedInExtension") {
  expectEqual("main.SomeOuterGenericClass<Swift.Int>.OtherInnerStruct",
              _typeName(SomeOuterGenericClass<Int>.OtherInnerStruct.self));
  expectEqual("main.SomeOuterGenericClass<Swift.Int>.OtherInnerGenericStruct<Swift.String>",
              _typeName(SomeOuterGenericClass<Int>.OtherInnerGenericStruct<String>.self));
}

extension SomeOuterGenericClass where T == Int {
  struct AnotherInnerStruct {}
  struct AnotherInnerGenericStruct<U> {}
}

TypeNameTests.test("NestedInConstrainedExtension") {
  expectEqual("(extension in main):main.SomeOuterGenericClass<Swift.Int>.AnotherInnerStruct",
              _typeName(SomeOuterGenericClass<Int>.AnotherInnerStruct.self));
  expectEqual("(extension in main):main.SomeOuterGenericClass<Swift.Int>.AnotherInnerGenericStruct<Swift.String>",
              _typeName(SomeOuterGenericClass<Int>.AnotherInnerGenericStruct<String>.self));
}

runAllTests()
