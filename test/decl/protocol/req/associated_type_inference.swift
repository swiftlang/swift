// RUN: %target-parse-verify-swift

protocol P0 {
  typealias Assoc1 : PSimple // expected-note{{ambiguous inference of associated type 'Assoc1': 'Double' vs. 'Int'}}
  // expected-note@-1{{ambiguous inference of associated type 'Assoc1': 'Double' vs. 'Int'}}
  // expected-note@-2{{unable to infer associated type 'Assoc1' for protocol 'P0'}}
  // expected-note@-3{{unable to infer associated type 'Assoc1' for protocol 'P0'}}
  func f0(_: Assoc1)
  func g0(_: Assoc1)
}

protocol PSimple { }
extension Int : PSimple { }
extension Double : PSimple { }

struct X0a : P0 { // okay: Assoc1 == Int
  func f0(_: Int) { }
  func g0(_: Int) { }
}

struct X0b : P0 { // expected-error{{type 'X0b' does not conform to protocol 'P0'}}
  func f0(_: Int) { } // expected-note{{matching requirement 'f0' to this declaration inferred associated type to 'Int'}}
  func g0(_: Double) { } // expected-note{{matching requirement 'g0' to this declaration inferred associated type to 'Double'}}
}

struct X0c : P0 { // okay: Assoc1 == Int
  func f0(_: Int) { }
  func g0(_: Float) { }
  func g0(_: Int) { }
}

struct X0d : P0 { // okay: Assoc1 == Int
  func f0(_: Int) { }
  func g0(_: Double) { } // viable, but no correspinding f0
  func g0(_: Int) { }
}

struct X0e : P0 { // expected-error{{type 'X0e' does not conform to protocol 'P0'}}
  func f0(_: Double) { } // expected-note{{matching requirement 'f0' to this declaration inferred associated type to 'Double}}
  func f0(_: Int) { } // expected-note{{matching requirement 'f0' to this declaration inferred associated type to 'Int'}}
  func g0(_: Double) { }
  func g0(_: Int) { } 
}

struct X0f : P0 { // okay: Assoc1 = Int because Float doesn't conform to PSimple
  func f0(_: Float) { }
  func f0(_: Int) { }
  func g0(_: Float) { }
  func g0(_: Int) { }
}

struct X0g : P0 { // expected-error{{type 'X0g' does not conform to protocol 'P0'}}
  func f0(_: Float) { } // expected-note{{inferred type 'Float' (by matching requirement 'f0') is invalid: does not conform to 'PSimple'}}
  func g0(_: Float) { } // expected-note{{inferred type 'Float' (by matching requirement 'g0') is invalid: does not conform to 'PSimple'}}
}

struct X0h<T : PSimple> : P0 {
  func f0(_: T) { }
}

extension X0h {
  func g0(_: T) { }
}

struct X0i<T : PSimple> {
}

extension X0i {
  func g0(_: T) { }
}

extension X0i : P0 { }

extension X0i {
  func f0(_: T) { }
}

// Protocol extension used to infer requirements
protocol P1 {
}

extension P1 {
  final func f0(x: Int) { }
  final func g0(x: Int) { }
}

struct X0j : P0, P1 { }

protocol P2 {
  typealias P2Assoc

  func h0(x: P2Assoc)
}

extension P2 where Self.P2Assoc : PSimple {
  final func f0(x: P2Assoc) { } // expected-note{{inferred type 'Float' (by matching requirement 'f0') is invalid: does not conform to 'PSimple'}}
  final func g0(x: P2Assoc) { } // expected-note{{inferred type 'Float' (by matching requirement 'g0') is invalid: does not conform to 'PSimple'}}
}

struct X0k : P0, P2 {
  func h0(x: Int) { }
}

struct X0l : P0, P2 { // expected-error{{type 'X0l' does not conform to protocol 'P0'}}
  func h0(x: Float) { }
}

// Prefer declarations in the type to those in protocol extensions
struct X0m : P0, P2 {
  func f0(x: Double) { }
  func g0(x: Double) { }
  func h0(x: Double) { }
}

// Inference from properties.
protocol PropertyP0 {
  typealias Prop : PSimple // expected-note{{unable to infer associated type 'Prop' for protocol 'PropertyP0'}}
  var property: Prop { get }
}

struct XProp0a : PropertyP0 { // okay PropType = Int
  var property: Int
}

struct XProp0b : PropertyP0 { // expected-error{{type 'XProp0b' does not conform to protocol 'PropertyP0'}}
  var property: Float // expected-note{{inferred type 'Float' (by matching requirement 'property') is invalid: does not conform to 'PSimple'}}
}

// Inference from subscripts
protocol SubscriptP0 {
  typealias Index
  typealias Element : PSimple // expected-note{{unable to infer associated type 'Element' for protocol 'SubscriptP0'}}

  subscript (i: Index) -> Element { get }
}

struct XSubP0a : SubscriptP0 {
  subscript (i: Int) -> Int { get { return i } }
}

struct XSubP0b : SubscriptP0 { // expected-error{{type 'XSubP0b' does not conform to protocol 'SubscriptP0'}}
  subscript (i: Int) -> Float { get { return Float(i) } } // expected-note{{inferred type 'Float' (by matching requirement 'subscript') is invalid: does not conform to 'PSimple'}}
}

// Inference from properties and subscripts
protocol CollectionLikeP0 {
  typealias Index
  typealias Element

  var startIndex: Index { get }
  var endIndex: Index { get }

  subscript (i: Index) -> Element { get }
}

struct SomeSlice<T> { }

struct XCollectionLikeP0a<T> : CollectionLikeP0 {
  var startIndex: Int
  var endIndex: Int

  subscript (i: Int) -> T { get { fatalError("blah") } }

  subscript (r: Range<Int>) -> SomeSlice<T> { get { return SomeSlice() } }
}

// rdar://problem/21304164
public protocol Thenable {
    typealias T // expected-note{{ambiguous inference of associated type 'T': 'T' vs. '(t: T, CorePromise<T>)'}}
    func then(success: (_: T) -> T) -> Self
}

public class CorePromise<T> : Thenable { // expected-error{{type 'CorePromise<T>' does not conform to protocol 'Thenable'}}
    public func then(success: (t: T, _: CorePromise<T>) -> T) -> Self { // expected-note 2{{matching requirement 'then' to this declaration}}
        return self.then() { (t: T) -> T in
            return success(t: t, self)
        }
    }
}

// rdar://problem/21559670
protocol P3 {
  typealias Assoc = Int
  // expected-note@-1{{ambiguous inference of associated type 'Assoc': 'Int' vs. 'Float'}}
  // expected-note@-2{{using associated type default 'Int'}}
  typealias Assoc2
  func foo(x: Assoc2) -> Assoc?
}

protocol P4 : P3 { }

extension P4 {
  func foo(x: Int) -> Float? { return 0 }
  // expected-note@-1{{matching requirement 'foo' to this declaration inferred associated type to 'Float'}}
}

extension P3 where Assoc == Int {
  func foo(x: Int) -> Assoc? { return nil }
}


struct X4 : P4 { } // expected-error{{type 'X4' does not conform to protocol 'P3'}}

