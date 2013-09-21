// RUN: %swift-ide-test -print-ast-typechecked -source-filename %s | FileCheck %s -strict-whitespace

struct FooStruct {
// CHECK: {{^}}struct FooStruct {{{$}}

  var instanceVar : Int
// CHECK-NEXT: {{^}}  var instanceVar : Int{{$}}

  func instanceFunc0() {}
// CHECK-NEXT: {{^}}  func instanceFunc0() {{{$}}
// CHECK-NEXT: {{^}}  }{{$}}

  func instanceFunc1(a : Int) {}
// CHECK-NEXT: {{^}}  func instanceFunc1(a : Int) {{{$}}
// CHECK-NEXT: {{^}}  }{{$}}

  func instanceFunc2(a : Int, b : Double) {}
// CHECK-NEXT: {{^}}  func instanceFunc2(a : Int, b : Double) {{{$}}
// CHECK-NEXT: {{^}}  }{{$}}

  func varargInstanceFunc0(v : Int...) {}
// CHECK-NEXT: {{^}}  func varargInstanceFunc0(v : Int...) {{{$}}
// CHECK-NEXT: {{^}}  }{{$}}

  func varargInstanceFunc1(a : Float, v : Int...) {}
// CHECK-NEXT: {{^}}  func varargInstanceFunc1(a : Float, v : Int...) {{{$}}
// CHECK-NEXT: {{^}}  }{{$}}

  func varargInstanceFunc2(a : Float, b : Double, v : Int...) {}
// CHECK-NEXT: {{^}}  func varargInstanceFunc2(a : Float, b : Double, v : Int...) {{{$}}
// CHECK-NEXT: {{^}}  }{{$}}

  func overloadedInstanceFunc1() -> Int {}
// CHECK-NEXT: {{^}}  func overloadedInstanceFunc1() -> Int {{{$}}
// CHECK-NEXT: {{^}}  }{{$}}

  func overloadedInstanceFunc1() -> Double {}
// CHECK-NEXT: {{^}}  func overloadedInstanceFunc1() -> Double {{{$}}
// CHECK-NEXT: {{^}}  }{{$}}

  func overloadedInstanceFunc2(x: Int) -> Int {}
// CHECK-NEXT: {{^}}  func overloadedInstanceFunc2(x : Int) -> Int {{{$}}
// CHECK-NEXT: {{^}}  }{{$}}

  func overloadedInstanceFunc2(x: Double) -> Int {}
// CHECK-NEXT: {{^}}  func overloadedInstanceFunc2(x : Double) -> Int {{{$}}
// CHECK-NEXT: {{^}}  }{{$}}

  func builderFunc1(a : Int) -> FooStruct {}
// CHECK-NEXT: {{^}}  func builderFunc1(a : Int) -> FooStruct {{{$}}
// CHECK-NEXT: {{^}}  }{{$}}

  subscript(i: Int) -> Double {
  get:
    return Double(i)
  set(val):
    instanceVar = i
  }
// CHECK-NEXT: {{^}}  subscript (i : Int) -> Double {{{$}}
// CHECK-NEXT: {{^}}    get: {{$}}
// CHECK:      {{^}}    set: {{$}}
// CHECK:      {{^}}  }{{$}}

  subscript(i: Int, j: Int) -> Double {
  get:
    return Double(i + j)
  set(val):
    instanceVar = i + j
  }
// CHECK-NEXT: {{^}}  subscript (i : Int, j : Int) -> Double {{{$}}
// CHECK-NEXT: {{^}}    get: {{$}}
// CHECK:      {{^}}    set: {{$}}
// CHECK:      {{^}}  }{{$}}

  func curriedVoidFunc1()() {}
// CHECK-NEXT: {{^}}   func curriedVoidFunc1()() {{{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  func curriedVoidFunc2()(a: Int) {}
// CHECK-NEXT: {{^}}   func curriedVoidFunc2()(a : Int) {{{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  func curriedVoidFunc3(a: Int)() {}
// CHECK-NEXT: {{^}}   func curriedVoidFunc3(a : Int)() {{{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  func curriedVoidFunc4(a: Int)(b: Int) {}
// CHECK-NEXT: {{^}}   func curriedVoidFunc4(a : Int)(b : Int) {{{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  func curriedStringFunc1()() -> String {}
// CHECK-NEXT: {{^}}   func curriedStringFunc1()() -> String {{{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  func curriedStringFunc2()(a: Int) -> String {}
// CHECK-NEXT: {{^}}   func curriedStringFunc2()(a : Int) -> String {{{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  func curriedStringFunc3(a: Int)() -> String {}
// CHECK-NEXT: {{^}}   func curriedStringFunc3(a : Int)() -> String {{{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  func curriedStringFunc4(a: Int)(b: Int) -> String {}
// CHECK-NEXT: {{^}}   func curriedStringFunc4(a : Int)(b : Int) -> String {{{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  func selectorVoidFunc1(a: Int) b(x: Float) {}
// CHECK-NEXT: {{^}}   func selectorVoidFunc1(a: Int) b(x: Float) {{{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  func selectorVoidFunc2(a: Int) b(x: Float) c(y: Double) {}
// CHECK-NEXT: {{^}}   func selectorVoidFunc2(a: Int) b(x: Float) c(y: Double) {{{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  func selectorStringFunc1(a: Int) b(x: Float) -> String {}
// CHECK-NEXT: {{^}}   func selectorStringFunc1(a: Int) b(x: Float) -> String {{{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  func selectorStringFunc2(a: Int) b(x: Float) c(y: Double) -> String {}
// CHECK-NEXT: {{^}}   func selectorStringFunc2(a: Int) b(x: Float) c(y: Double) -> String {{{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  struct NestedStruct {}
// CHECK-NEXT: {{^}}   struct NestedStruct {{{$}}
// CHECK-NEXT: {{^}}     init (){{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  class NestedClass {}
// CHECK-NEXT: {{^}}   class NestedClass : DynamicLookup {{{$}}
// CHECK-NEXT: {{^}}     destructor  {{{$}}
// CHECK-NEXT: {{^}}     }{{$}}
// CHECK-NEXT: {{^}}     init () {{{$}}
// CHECK-NEXT: {{^}}     }{{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  enum NestedUnion {}
// CHECK-NEXT: {{^}}   enum NestedUnion {{{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  // Can not declare a nested protocol.
  // protocol NestedProtocol {}

  typealias NestedTypealias = Int
// CHECK-NEXT: {{^}}   typealias NestedTypealias = Int{{$}}

  // FIXME: uncomment when we have static vars.
  // static var staticVar : Int

  static func staticFunc0() {}
// CHECK-NEXT: {{^}}   static func staticFunc0() {{{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  static func staticFunc1(a : Int) {}
// CHECK-NEXT: {{^}}   static func staticFunc1(a : Int) {{{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  static func overloadedStaticFunc1() -> Int {}
// CHECK-NEXT: {{^}}   static func overloadedStaticFunc1() -> Int {{{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  static func overloadedStaticFunc1() -> Double {}
// CHECK-NEXT: {{^}}   static func overloadedStaticFunc1() -> Double {{{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  static func overloadedStaticFunc2(x: Int) -> Int {}
// CHECK-NEXT: {{^}}   static func overloadedStaticFunc2(x : Int) -> Int {{{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  static func overloadedStaticFunc2(x: Double) -> Int {}
// CHECK-NEXT: {{^}}   static func overloadedStaticFunc2(x : Double) -> Int {{{$}}
// CHECK-NEXT: {{^}}   }{{$}}

}
// CHECK-NEXT: {{^}}   init (instanceVar : Int){{$}}
// CHECK-NEXT: {{^}}   init () {{{$}}
// CHECK-NEXT: {{^}}     {{$}}
// CHECK-NEXT: {{^}}   }{{$}}
// CHECK-NEXT: {{^}} }{{$}}

extension FooStruct {
// CHECK-NEXT: {{^}} extension FooStruct {{{$}}

  var extProp : Int {
  get:
    return 42
  set(val):
  }
// CHECK-NEXT: {{^}}   var extProp : Int {{{$}}
// CHECK-NEXT: {{^}}   get: {{$}}
// CHECK-NEXT: {{^}}     return {{$}}
// CHECK-NEXT: {{^}} {{$}}
// CHECK-NEXT: {{^}}   set: {{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  func extFunc0() {}
// CHECK-NEXT: {{^}}   func extFunc0() {{{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  static func extStaticFunc0() {}
// CHECK-NEXT: {{^}}   static func extStaticFunc0() {{{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  struct ExtNestedStruct {}
// CHECK-NEXT: {{^}}   struct ExtNestedStruct {{{$}}
// CHECK-NEXT: {{^}}     init (){{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  class ExtNestedClass {}
// CHECK-NEXT: {{^}}   class ExtNestedClass : DynamicLookup {{{$}}
// CHECK-NEXT: {{^}}     destructor  {{{$}}
// CHECK-NEXT: {{^}}     }{{$}}
// CHECK-NEXT: {{^}}     init () {{{$}}
// CHECK-NEXT: {{^}}     }{{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  enum ExtNestedUnion {
    case ExtUnionX(Int)
  }
// CHECK-NEXT: {{^}}   enum ExtNestedUnion {{{$}}
// CHECK-NEXT: {{^}}     case ExtUnionX(Int){{$}}
// CHECK-NEXT: {{^}}     {{$}}
// CHECK-NEXT: {{^}}   }{{$}}

  typealias ExtNestedTypealias = Int
// CHECK-NEXT: {{^}}   typealias ExtNestedTypealias = Int{{$}}
}
// CHECK-NEXT: {{^}} }{{$}}

var fooObject : FooStruct
// CHECK-NEXT: {{^}} {{{$}}
// CHECK-NEXT: {{^}}   var fooObject : FooStruct{{$}}
// CHECK-NEXT: {{^}} }{{$}}

