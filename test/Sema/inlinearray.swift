// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature ValueGenerics

// REQUIRES: swift_feature_ValueGenerics

let a: InlineArray = [1, 2, 3] // Ok, InlineArray<3, Int>
let b: InlineArray<_, Int> = [1, 2, 3] // Ok, InlineArray<3, Int>
let c: InlineArray<3, _> = [1, 2, 3] // Ok, InlineArray<3, Int>

let d: InlineArray<2, _> = [1, 2, 3] // expected-error {{expected '2' elements in inline array literal, but got '3'}}
let e: InlineArray<2, _> = [1] // expected-error {{expected '2' elements in inline array literal, but got '1'}}

let f: InlineArray<_, Int> = ["hello"] // expected-error {{cannot convert value of type 'String' to expected element type 'Int'}}

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
