// RUN: %target-typecheck-verify-swift -enable-experimental-feature DefaultGenerics -disable-availability-checking

// REQUIRES: swift_feature_DefaultGenerics

struct A<T = Int> {} // expected-note{{generic struct 'A' declared here}}
enum B<T = Int> {
  case foo
}
class C<T = Int> {}
typealias D<T = Int> = A<T>
actor E<T = Int> {}

protocol F<T = Int> {} // expected-error{{an associated type named 'T' must be declared in the protocol 'F' or a protocol it inherits}}
                       // expected-error@-1{{expected '>' to complete primary associated type list}}
                       // expected-note@-2{{to match this opening '<'}}

protocol G<T = Int> { // expected-error{{expected '>' to complete primary associated type list}}
                      // expected-note@-1{{to match this opening '<'}}
  associatedtype T
}

func h<T = Int>(_: T) {} // expected-error{{generic parameter 'T' can only have a default type on type declarations}}

struct I {
  subscript<T = Int>(_: T) -> Int { 123 } // expected-error{{generic parameter 'T' can only have a default type on type declarations}}
}

struct J<each T, U = Int> {} // expected-error{{declaration 'J' cannot have both parameter packs and generic parameters with default types}}

struct K<let n: Int, T = Int> {} // OK

struct L<T = Int, U, V = String> {} // expected-error{{generic parameter 'T' with default type 'Int' must be trailing}}

protocol Allocator {}

struct SystemAllocator: Allocator {}
struct BumpAllocator: Allocator {}

struct Vec<Element: ~Copyable, Alloc: Allocator = SystemAllocator> {} // OK
                                                                      // expected-note@-1{{arguments to generic parameter 'Alloc' ('BumpAllocator' and 'SystemAllocator') are expected to be equal}}
// FIXME: This needs to be diagnosed somehow...
struct Vec2<Element: ~Copyable, Alloc: Allocator = Int> {}

let m = A() // OK
let n = B.foo // OK
let o = C() // OK
let p = D() // OK
let q = E() // OK

struct R<T = Int> {
  struct S<U = String> {}
}

let t = R.S() // OK
let u = R<Bool>.S() // OK
let v = R.S<Bool>() // OK

let w = Vec<Int>() // OK

// FIXME: We need to diagnose the declaration with an incorrect default type
let x = Vec2<String>() // expected-error{{type 'Int' does not conform to protocol 'Allocator'}}

func y(_: Vec<Int>) {} // OK

// TODO: Perhaps we can eliminate generic arguments from being printed if they are the default?
y(Vec<Int, BumpAllocator>()) // expected-error{{cannot convert value of type 'Vec<Int, BumpAllocator>' to expected argument type 'Vec<Int, SystemAllocator>'}}

// FIXME: We need to disallow this.
struct Z<T, U = T?> {}

// FIXME: We should probably allow this.
func aa(_: A) {} // expected-error{{reference to generic type 'A' requires arguments in <...>}}

func ab(_: A< >) {} // OK

struct MyArray<Element, Alloc: Allocator = SystemAllocator> {} // OK

extension MyArray: ExpressibleByArrayLiteral {
  init(arrayLiteral: Element...) {}
}

func ac<Element>(_: MyArray<Element>) {}

let ad: MyArray = [1, 2, 3] // OK
ac(ad) // OK
