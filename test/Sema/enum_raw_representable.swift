// RUN: %target-typecheck-verify-swift

enum Foo : Int {
  case a, b, c
}

var raw1: Int = Foo.a.rawValue
var raw2: Foo.RawValue = raw1
var cooked1: Foo? = Foo(rawValue: 0)
var cooked2: Foo? = Foo(rawValue: 22)

enum Bar : Double {
  case a, b, c
}

func localEnum() -> Int {
  enum LocalEnum : Int {
    case a, b, c
  }
  return LocalEnum.a.rawValue
}

enum MembersReferenceRawType : Int {
  case a, b, c

  init?(rawValue: Int) {
    self = MembersReferenceRawType(rawValue: rawValue)!
  }

  func successor() -> MembersReferenceRawType {
    return MembersReferenceRawType(rawValue: rawValue + 1)!
  }
}

func serialize<T : RawRepresentable>(_ values: [T]) -> [T.RawValue] {
  return values.map { $0.rawValue }
}

func deserialize<T : RawRepresentable>(_ serialized: [T.RawValue]) -> [T] {
  return serialized.map { T(rawValue: $0)! }
}

var ints: [Int] = serialize([Foo.a, .b, .c])
var doubles: [Double] = serialize([Bar.a, .b, .c])

var foos: [Foo] = deserialize([1, 2, 3])
var bars: [Bar] = deserialize([1.2, 3.4, 5.6])

// Infer RawValue from witnesses.
enum Color : Int {
  case red
  case blue

  init?(rawValue: Double) {
    return nil
  }

  var rawValue: Double {
    return 1.0
  }
}

var colorRaw: Color.RawValue = 7.5

// Mismatched case types

enum BadPlain : UInt { // expected-error {{'BadPlain' declares raw type 'UInt', but does not conform to RawRepresentable and conformance could not be synthesized}} expected-note {{do you want to add protocol stubs?}}
    case a = "hello"   // expected-error {{cannot convert value of type 'String' to raw type 'UInt'}}
}

// Recursive diagnostics issue in tryRawRepresentableFixIts()
class Outer {
  // The setup is that we have to trigger the conformance check
  // while diagnosing the conversion here. For the purposes of
  // the test I'm putting everything inside a class in the right
  // order, but the problem can trigger with a multi-file
  // scenario too.
  let a: Int = E.a // expected-error {{cannot convert value of type 'Outer.E' to specified type 'Int'}}

  enum E : Array<Int> { // expected-error {{raw type 'Array<Int>' is not expressible by a string, integer, or floating-point literal}}
    case a
  }
}

// rdar://problem/32431736 - Conversion fix-it from String to String raw value enum can't look through optionals

func rdar32431736() {
  enum E : String {
    case A = "A"
    case B = "B"
  }

  let items1: [String] = ["A", "a"]
  let items2: [String]? = ["A"]

  let myE1: E = items1.first
  // expected-error@-1 {{cannot convert value of type 'String?' to specified type 'E'}}
  // expected-note@-2 {{construct 'E' from unwrapped 'String' value}} {{17-17=E(rawValue: }} {{29-29=!)}}

  let myE2: E = items2?.first
  // expected-error@-1 {{cannot convert value of type 'String?' to specified type 'E'}}
  // expected-note@-2 {{construct 'E' from unwrapped 'String' value}} {{17-17=E(rawValue: (}} {{30-30=)!)}}
}

// rdar://problem/32431165 - improve diagnostic for raw representable argument mismatch

enum E_32431165 : String {
  case foo = "foo"
  case bar = "bar" // expected-note {{'bar' declared here}}
}

func rdar32431165_1(_: E_32431165) {}
func rdar32431165_1(_: Int) {}
func rdar32431165_1(_: Int, _: E_32431165) {}

rdar32431165_1(E_32431165.baz)
// expected-error@-1 {{type 'E_32431165' has no member 'baz'; did you mean 'bar'?}}

rdar32431165_1(.baz)
// expected-error@-1 {{reference to member 'baz' cannot be resolved without a contextual type}}

rdar32431165_1("")
// expected-error@-1 {{cannot convert value of type 'String' to expected argument type 'E_32431165'}} {{16-16=E_32431165(rawValue: }} {{18-18=)}}
rdar32431165_1(42, "")
// expected-error@-1 {{cannot convert value of type 'String' to expected argument type 'E_32431165'}} {{20-20=E_32431165(rawValue: }} {{22-22=)}}

func rdar32431165_2(_: String) {}
func rdar32431165_2(_: Int) {}
func rdar32431165_2(_: Int, _: String) {}

rdar32431165_2(E_32431165.bar)
// expected-error@-1 {{cannot convert value of type 'E_32431165' to expected argument type 'String'}} {{16-16=}} {{30-30=.rawValue}}
rdar32431165_2(42, E_32431165.bar)
// expected-error@-1 {{cannot convert value of type 'E_32431165' to expected argument type 'String'}} {{20-20=}} {{34-34=.rawValue}}

E_32431165.bar == "bar"
// expected-error@-1 {{cannot convert value of type 'E_32431165' to expected argument type 'String}} {{1-1=}} {{15-15=.rawValue}}

"bar" == E_32431165.bar
// expected-error@-1 {{cannot convert value of type 'E_32431165' to expected argument type 'String}} {{10-10=}} {{24-24=.rawValue}}

func rdar32432253(_ condition: Bool = false) {
  let choice: E_32431165 = condition ? .foo : .bar
  let _ = choice == "bar"
  // expected-error@-1 {{cannot convert value of type 'E_32431165' to expected argument type 'String'}} {{11-11=}} {{17-17=.rawValue}}
}

func sr8150_helper1(_: Int) {}
func sr8150_helper1(_: Double) {}

func sr8150_helper2(_: Double) {}
func sr8150_helper2(_: Int) {}

func sr8150_helper3(_: Foo) {}
func sr8150_helper3(_: Bar) {}

func sr8150_helper4(_: Bar) {}
func sr8150_helper4(_: Foo) {}

func sr8150(bar: Bar) {
  sr8150_helper1(bar)
  // expected-error@-1 {{cannot convert value of type 'Bar' to expected argument type 'Double'}} {{18-18=}} {{21-21=.rawValue}}
  sr8150_helper2(bar)
  // expected-error@-1 {{cannot convert value of type 'Bar' to expected argument type 'Double'}} {{18-18=}} {{21-21=.rawValue}}
  sr8150_helper3(0.0)
  // expected-error@-1 {{cannot convert value of type 'Double' to expected argument type 'Bar'}} {{18-18=Bar(rawValue: }} {{21-21=)}}
  sr8150_helper4(0.0)
  // expected-error@-1 {{cannot convert value of type 'Double' to expected argument type 'Bar'}} {{18-18=Bar(rawValue: }} {{21-21=)}}
}

class SR8150Box {
  var bar: Bar
  init(bar: Bar) { self.bar = bar }
}
// Bonus problem with mutable values being passed.
func sr8150_mutable(obj: SR8150Box) {
  sr8150_helper1(obj.bar)
  // expected-error@-1 {{cannot convert value of type 'Bar' to expected argument type 'Double'}} {{18-18=}} {{25-25=.rawValue}}

  var bar = obj.bar
  sr8150_helper1(bar)
  // expected-error@-1 {{cannot convert value of type 'Bar' to expected argument type 'Double'}} {{18-18=}} {{21-21=.rawValue}}
}

struct NotEquatable { }

enum ArrayOfNewEquatable : Array<NotEquatable> { }
// expected-error@-1{{raw type 'Array<NotEquatable>' is not expressible by a string, integer, or floating-point literal}}
// expected-error@-2{{'ArrayOfNewEquatable' declares raw type 'Array<NotEquatable>', but does not conform to RawRepresentable and conformance could not be synthesized}}
// expected-error@-3{{RawRepresentable conformance cannot be synthesized because raw type 'Array<NotEquatable>' is not Equatable}}
// expected-note@-4{{do you want to add protocol stubs?}}
// expected-error@-5 {{an enum with no cases cannot declare a raw type}}
