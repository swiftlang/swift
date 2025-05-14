// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature InlineArrayTypeSugar

// REQUIRES: swift_feature_InlineArrayTypeSugar

let a: InlineArray = [1, 2, 3] // Ok, InlineArray<3, Int>
let b: InlineArray<_, Int> = [1, 2, 3] // Ok, InlineArray<3, Int>
let c: InlineArray<3, _> = [1, 2, 3] // Ok, InlineArray<3, Int>

let d: InlineArray<2, _> = [1, 2, 3] // expected-error {{expected '2' elements in inline array literal, but got '3'}}
let e: InlineArray<2, _> = [1] // expected-error {{expected '2' elements in inline array literal, but got '1'}}

let f: InlineArray<_, Int> = ["hello"] // expected-error {{cannot convert value of type 'String' to expected element type 'Int'}}

let g: InlineArray<1, 1> // expected-error {{cannot use value type '1' for generic argument 'Element'}}

let _: [3 x Int] = [1, 2, 3]  // Ok, InlineArray<3, Int>
let _: [_ x Int] = [1, 2, 3]  // Ok, InlineArray<3, Int>
let _: [3 x _] = [1, 2, 3]    // Ok, InlineArray<3, Int>
let _: [_ x _] = ["", "", ""] // Ok, InlineArray<3, String>

let _: [3 x [3 x Int]] = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
let _: [3 x [3 x Int]] = [[1, 2], [3, 4, 5, 6]]
// expected-error@-1 {{'3' elements in inline array literal, but got '2'}}
// expected-error@-2 2{{cannot convert value of type '[Int]' to expected element type '[3 x Int]'}}

let _ = [3 x [3 x Int]](repeating: [1, 2]) // expected-error {{expected '3' elements in inline array literal, but got '2'}}
let _ = [3 x [_ x Int]](repeating: [1, 2])

let _: [Int x 10] = [1, 2] // expected-error {{element count must precede inline array element type}} {{15-17=Int}} {{9-12=10}}
// expected-error@-1 {{expected '10' elements in inline array literal, but got '2'}}

let _: [4 x _] = [1, 2, 3]   // expected-error {{expected '4' elements in inline array literal, but got '3'}}
let _: [3 x Int] = [1, 2, 3, 4] // expected-error {{expected '3' elements in inline array literal, but got '4'}}
let _: [3 x String] = [1, 2, 3] // expected-error 3{{cannot convert value of type 'Int' to expected element type 'String'}}
let _: [3 x String] = [1] // expected-error {{cannot convert value of type 'Int' to expected element type 'String'}}
// expected-error@-1 {{expected '3' elements in inline array literal, but got '1'}}

func takeVectorOf2<T>(_: InlineArray<2, T>) {}

takeVectorOf2([1, 2]) // Ok
takeVectorOf2(["hello", "world"]) // Ok

takeVectorOf2([1]) // expected-error {{expected '2' elements in inline array literal, but got '1'}}

takeVectorOf2([1, 2, 3]) // expected-error {{expected '2' elements in inline array literal, but got '3'}}

takeVectorOf2(["hello"]) // expected-error {{expected '2' elements in inline array literal, but got '1'}}

takeVectorOf2(["hello", "world", "!"]) // expected-error {{expected '2' elements in inline array literal, but got '3'}}

func takeVectorOf2Int(_: InlineArray<2, Int>) {}

takeVectorOf2Int([1, 2]) // Ok

takeVectorOf2Int([1]) // expected-error {{expected '2' elements in inline array literal, but got '1'}}

takeVectorOf2Int([1, 2, 3]) // expected-error {{expected '2' elements in inline array literal, but got '3'}}

takeVectorOf2Int(["hello"]) // expected-error {{cannot convert value of type '[String]' to expected argument type 'InlineArray<2, Int>'}}

takeVectorOf2Int(["hello", "world"]) // expected-error {{cannot convert value of type 'String' to expected element type 'Int'}}
                                     // expected-error@-1 {{cannot convert value of type 'String' to expected element type 'Int'}}

takeVectorOf2Int(["hello", "world", "!"]) // expected-error {{cannot convert value of type '[String]' to expected argument type 'InlineArray<2, Int>'}}

func takeSugarVectorOf2<T>(_: [2 x T], ty: T.Type = T.self) {}
takeSugarVectorOf2([1, 2])
takeSugarVectorOf2(["hello"]) // expected-error {{expected '2' elements in inline array literal, but got '1'}}
takeSugarVectorOf2(["hello"], ty: Int.self) // expected-error {{cannot convert value of type '[String]' to expected argument type '[2 x Int]'}}
takeSugarVectorOf2(["hello", "hi"], ty: Int.self) // expected-error 2{{cannot convert value of type 'String' to expected element type 'Int'}}


struct X {
  var sprites: InlineArray<2, Int>
}

func foo(x: inout X) {
  x.sprites = [1, 2, 3] // expected-error {{cannot assign value of type '[Int]' to type 'InlineArray<2, Int>'}}
}

struct MySprites {
  var bricks: InlineArray<40, MySprite>
}

struct MySprite {
  var x = 42
}

nonisolated(unsafe)
var sprites: MySprites? = nil

func foo() {
  let bricks: InlineArray<1, MySprite> = [MySprite()]

  sprites = .init(bricks: bricks) // expected-error {{cannot convert value of type 'InlineArray<1, MySprite>' to expected argument type 'InlineArray<40, MySprite>'}}
                                  // expected-note@-1 {{arguments to generic parameter 'count' ('1' and '40') are expected to be equal}}
}

// Make sure the deserialized integer generic argument gets treated as an integer
// generic argument when we clone the generic param list for extensions.
extension InlineArray where Element: ~Copyable {
  func forEach(_ body: (borrowing Element) -> Void) {
    for i in 0 ..< count {
      body(self[i])
    }
  }

  func enumerated(_ body: (Int, borrowing Element) -> Void) {
    for i in 0 ..< count {
      body(i, self[i])
    }
  }
}

extension [3 x Int] { // expected-note 2{{where 'count' = '2'}} expected-note {{where 'Element' = 'String'}}
  func methodOnSugar() {}
}

func testExtension(
  _ a: [3 x Int],
  _ b: InlineArray<3, Int>,
  _ c: [2 x Int],
  _ d: [2 x String]
) {
  a.enumerated { _, _ in }
  a.methodOnSugar()
  b.methodOnSugar()
  c.methodOnSugar()
  // expected-error@-1 {{referencing instance method 'methodOnSugar()' on 'InlineArray' requires the types '2' and '3' be equivalent}}
  d.methodOnSugar()
  // expected-error@-1 {{referencing instance method 'methodOnSugar()' on 'InlineArray' requires the types '2' and '3' be equivalent}}
  // expected-error@-2 {{referencing instance method 'methodOnSugar()' on 'InlineArray' requires the types 'String' and 'Int' be equivalent}}
}

func redecl(_ x: InlineArray<2, Int>) {} // expected-note {{'redecl' previously declared here}}
func redecl(_ x: [2 x Int]) {} // expected-error {{invalid redeclaration of 'redecl'}}

func noRedecl(_ x: InlineArray<2, Int>) {}
func noRedecl(_ x: [3 x Int]) {}
func noRedecl(_ x: [2 x String]) {}
func noRedecl(_ x: [3 x String]) {}

func testMismatches(_ x: [3 x Int], _ y: InlineArray<3, Int>) {
  let _: InlineArray<3, Int> = x
  let _: InlineArray<4, Int> = x // expected-error {{cannot assign value of type '[3 x Int]' to type 'InlineArray<4, Int>'}}
  // expected-note@-1 {{arguments to generic parameter 'count' ('3' and '4') are expected to be equal}}
  let _: InlineArray<3, String> = x  // expected-error {{cannot assign value of type '[3 x Int]' to type 'InlineArray<3, String>'}}
  // expected-note@-1 {{arguments to generic parameter 'Element' ('Int' and 'String') are expected to be equal}}

  let _: [3 x Int] = y
  let _: [4 x Int] = y // expected-error {{cannot assign value of type 'InlineArray<3, Int>' to type '[4 x Int]'}}
  // expected-note@-1 {{arguments to generic parameter 'count' ('3' and '4') are expected to be equal}}
  let _: [3 x String] = y  // expected-error {{cannot assign value of type 'InlineArray<3, Int>' to type '[3 x String]'}}
  // expected-note@-1 {{arguments to generic parameter 'Element' ('Int' and 'String') are expected to be equal}}
}

func testPointerConversion() {
  var inlineArray = InlineArray<1, Int>(repeating: 0)
  acceptPointer(&inlineArray) // expected-error {{cannot convert value of type 'UnsafeMutablePointer<InlineArray<1, Int>>' to expected argument type 'UnsafeMutablePointer<Int>'}}
                              // expected-note@-1 {{arguments to generic parameter 'Pointee' ('InlineArray<1, Int>' and 'Int') are expected to be equal}}
}

func acceptPointer(_ pointer: UnsafeMutablePointer<Int>) {}
