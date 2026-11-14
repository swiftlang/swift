// RUN: %sourcekitd-test -req=collect-decl-usr %s -- %s | %FileCheck %s

protocol MyProtocol {
  associatedtype Element
  func requirement(value: Element)
}

typealias MyAlias = Int

let globalValue = 1

func globalFunction(argument: Int) -> Int {
  let localValue = argument

  func localFunction() -> Int {
    return localValue
  }

  return localFunction()
}

enum MyEnum {
  case plain
  case payload(Int)
}

actor MyActor {
  static let staticValue = 1

  subscript(index: Int) -> Int {
    return index
  }
}

class MyClass {
  init(value: Int) {}
  class func classMethod() {}
}

struct Outer {
  struct Inner {
    var nestedValue: Int
  }
}

struct GenericBox<Element> {
  func map<Result>(_ transform: (Element) -> Result) -> Result? {
    nil
  }
}

// CHECK: key.kind: source.lang.swift.decl.protocol
// CHECK-NEXT: key.usr:
// CHECK-SAME: MyProtocol
// CHECK: key.kind: source.lang.swift.decl.associatedtype
// CHECK-NEXT: key.usr:
// CHECK-SAME: Element
// CHECK: key.kind: source.lang.swift.decl.function.method.instance
// CHECK-NEXT: key.usr:
// CHECK-SAME: requirement
// CHECK: key.kind: source.lang.swift.decl.var.parameter
// CHECK-NEXT: key.usr:
// CHECK-SAME: value
// CHECK: key.kind: source.lang.swift.decl.typealias
// CHECK-NEXT: key.usr:
// CHECK-SAME: MyAlias
// CHECK: key.kind: source.lang.swift.decl.var.global
// CHECK-NEXT: key.usr:
// CHECK-SAME: globalValue
// CHECK: key.kind: source.lang.swift.decl.function.free
// CHECK-NEXT: key.usr:
// CHECK-SAME: globalFunction
// CHECK: key.kind: source.lang.swift.decl.var.parameter
// CHECK-NEXT: key.usr:
// CHECK-SAME: argument
// CHECK: key.kind: source.lang.swift.decl.var.local
// CHECK-NEXT: key.usr:
// CHECK-SAME: localValue
// CHECK: key.kind: source.lang.swift.decl.function.free
// CHECK-NEXT: key.usr:
// CHECK-NEXT: key.offset: 284
// CHECK-NEXT: key.length: 13
// CHECK: key.kind: source.lang.swift.decl.enum
// CHECK-NEXT: key.usr:
// CHECK-SAME: MyEnum
// CHECK: key.kind: source.lang.swift.decl.enumelement
// CHECK-NEXT: key.usr:
// CHECK-SAME: plain
// CHECK: key.kind: source.lang.swift.decl.enumelement
// CHECK-NEXT: key.usr:
// CHECK-SAME: payload
// CHECK-NOT: key.length: 0
// Actor declarations are reported with the class declaration kind.
// CHECK: key.kind: source.lang.swift.decl.class
// CHECK-NEXT: key.usr:
// CHECK-SAME: MyActor
// CHECK: key.kind: source.lang.swift.decl.var.static
// CHECK-NEXT: key.usr:
// CHECK-SAME: staticValue
// CHECK: key.kind: source.lang.swift.decl.function.subscript
// CHECK-NEXT: key.usr:
// CHECK-SAME: MyActor
// CHECK-NEXT: key.offset: 462
// CHECK-NEXT: key.length: 9
// CHECK: key.kind: source.lang.swift.decl.var.parameter
// CHECK-NEXT: key.usr:
// CHECK-SAME: index
// CHECK-NOT: source.lang.swift.decl.function.accessor.getter
// CHECK-NOT: source.lang.swift.decl.function.constructor
// CHECK: key.kind: source.lang.swift.decl.class
// CHECK-NEXT: key.usr:
// CHECK-SAME: MyClass
// CHECK: key.kind: source.lang.swift.decl.function.constructor
// CHECK-NEXT: key.usr:
// CHECK-SAME: MyClass
// CHECK: key.kind: source.lang.swift.decl.var.parameter
// CHECK-NEXT: key.usr:
// CHECK-SAME: value
// CHECK: key.kind: source.lang.swift.decl.function.method.class
// CHECK-NEXT: key.usr:
// CHECK-SAME: classMethod
// CHECK: key.kind: source.lang.swift.decl.struct
// CHECK-NEXT: key.usr:
// CHECK-SAME: Outer
// CHECK: key.kind: source.lang.swift.decl.struct
// CHECK-NEXT: key.usr:
// CHECK-SAME: Inner
// CHECK: key.kind: source.lang.swift.decl.var.instance
// CHECK-NEXT: key.usr:
// CHECK-SAME: nestedValue
// CHECK-NOT: source.lang.swift.decl.function.constructor
// CHECK: key.kind: source.lang.swift.decl.struct
// CHECK-NEXT: key.usr:
// CHECK-SAME: GenericBox
// CHECK: key.kind: source.lang.swift.decl.generic_type_param
// CHECK-NEXT: key.usr:
// CHECK-SAME: Element
// CHECK: key.kind: source.lang.swift.decl.function.method.instance
// CHECK-NEXT: key.usr:
// CHECK-SAME: map
// CHECK: key.kind: source.lang.swift.decl.generic_type_param
// CHECK-NEXT: key.usr:
// CHECK-SAME: Result
// CHECK: key.kind: source.lang.swift.decl.var.parameter
// CHECK-NEXT: key.usr:
// CHECK-SAME: transform
