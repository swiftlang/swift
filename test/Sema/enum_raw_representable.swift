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

// We reject enums where the raw type stated in the inheritance clause does not
// match the types of the witnesses.
enum Color : Int {
  case red
  case blue

  init?(rawValue: Double) {
    return nil
  }

  var rawValue: Double { // expected-note {{found this candidate}}
    return 1.0
  }
}

func useRawValue(of color: Color) {
  _ = color.rawValue
  // expected-error@-1 {{ambiguous use of 'rawValue'}}
}

var colorRaw: Color.RawValue = 7.5
// expected-error@-1 {{cannot convert value of type 'Double' to specified type 'Color.RawValue' (aka 'Int')}}

// Mismatched case types

enum BadPlain : UInt { // expected-error {{'BadPlain' declares raw type 'UInt', but does not conform to RawRepresentable and conformance could not be synthesized}} expected-note {{add stubs for conformance}}
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

  enum E : Array<Int> {
  // expected-error@-1 {{raw type 'Array<Int>' is not expressible by a string, integer, or floating-point literal}}
  // expected-error@-2 {{'Outer.E' declares raw type 'Array<Int>', but does not conform to RawRepresentable and conformance could not be synthesized}}
  // expected-note@-3 {{add stubs for conformance}}
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
  // expected-note@-2 {{construct 'E' from unwrapped 'String' value}} {{17-17=E(rawValue: }} {{29-29=!) ?? <#default value#>}}

  let myE2: E = items2?.first
  // expected-error@-1 {{cannot convert value of type 'String?' to specified type 'E'}}
  // expected-note@-2 {{construct 'E' from unwrapped 'String' value}} {{17-17=E(rawValue: (}} {{30-30=)!) ?? <#default value#>}}
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
// expected-error@-1 {{cannot convert value of type 'String' to expected argument type 'E_32431165'}}
rdar32431165_1(42, "")
// expected-error@-1 {{cannot convert value of type 'String' to expected argument type 'E_32431165'}} {{20-20=E_32431165(rawValue: }} {{22-22=) ?? <#default value#>}}

func rdar32431165_2(_: String) {}
func rdar32431165_2(_: Int) {}
func rdar32431165_2(_: Int, _: String) {}

rdar32431165_2(E_32431165.bar)
// expected-error@-1 {{cannot convert value of type 'E_32431165' to expected argument type 'String'}} {{30-30=.rawValue}}
rdar32431165_2(42, E_32431165.bar)
// expected-error@-1 {{cannot convert value of type 'E_32431165' to expected argument type 'String'}} {{34-34=.rawValue}}

// TODO: In following two examples it's possible to fix a problem by either using `.rawValue` on first argument
// or constructing raw representable type from second, both ways are valid.
do {
  E_32431165.bar == "bar"
  // expected-error@-1 {{cannot convert value of type 'E_32431165' to expected argument type 'String'}} {{17-17=.rawValue}}

  "bar" == E_32431165.bar
  // expected-error@-1 {{cannot convert value of type 'E_32431165' to expected argument type 'String'}} {{26-26=.rawValue}}
}

func rdar32431165_overloaded() -> Int { 42 }     // expected-note {{'rdar32431165_overloaded()' produces 'Int', not the expected contextual result type 'E_32431165'}}
func rdar32431165_overloaded() -> String { "A" } // expected-note {{'rdar32431165_overloaded()' produces 'String', not the expected contextual result type 'E_32431165'}}

func test_candidate_diagnostic() {
  func test_argument(_: E_32431165) {}

  let _: E_32431165 = rdar32431165_overloaded() // expected-error {{no 'rdar32431165_overloaded' candidates produce the expected contextual result type 'E_32431165'}}
  test_argument(rdar32431165_overloaded()) // expected-error {{cannot convert value of type 'String' to expected argument type 'E_32431165'}} {{17-17=E_32431165(rawValue: }} {{42-42=) ?? <#default value#>}}
}

func rdar32432253(_ condition: Bool = false) {
  let choice: E_32431165 = condition ? .foo : .bar
  let _ = choice == "bar"
  // expected-error@-1 {{cannot convert value of type 'E_32431165' to expected argument type 'String'}} {{17-17=.rawValue}}
}

// https://github.com/apple/swift/issues/50682
do {
  func helper1(_: Int) {}
  func helper1(_: Double) {}

  func helper2(_: Double) {}
  func helper2(_: Int) {}

  func helper3(_: Foo) {}
  func helper3(_: Bar) {}

  func helper4(_: Bar) {}
  func helper4(_: Foo) {}

  do {
    let bar: Bar

    helper1(bar)
    // expected-error@-1 {{cannot convert value of type 'Bar' to expected argument type 'Double'}} {{16-16=.rawValue}}
    helper2(bar)
    // expected-error@-1 {{cannot convert value of type 'Bar' to expected argument type 'Double'}} {{16-16=.rawValue}}
    helper3(0.0)
    // expected-error@-1 {{cannot convert value of type 'Double' to expected argument type 'Bar'}} {{13-13=Bar(rawValue: }} {{16-16=) ?? <#default value#>}}
    helper4(0.0)
    // expected-error@-1 {{cannot convert value of type 'Double' to expected argument type 'Bar'}} {{13-13=Bar(rawValue: }} {{16-16=) ?? <#default value#>}}
  }

  // Bonus problem with mutable values being passed.
  do {
    class Box {
      var bar: Bar
      init(bar: Bar) {}
    }

    let box: Box

    helper1(box.bar)
    // expected-error@-1 {{cannot convert value of type 'Bar' to expected argument type 'Double'}} {{20-20=.rawValue}}

    var bar = box.bar
    helper1(bar)
    // expected-error@-1 {{cannot convert value of type 'Bar' to expected argument type 'Double'}} {{16-16=.rawValue}}
  }

  do {
    let opt: Bar?

    helper1(opt)
    // expected-error@-1 {{cannot convert value of type 'Bar?' to expected argument type 'Double'}} {{16-16=?.rawValue ?? <#default value#>}}
    helper1(opt ?? Bar.a)
    // expected-error@-1 {{cannot convert value of type 'Bar' to expected argument type 'Double'}} {{13-13=(}} {{25-25=).rawValue}}
    let _: Double? = opt
    // expected-error@-1 {{cannot convert value of type 'Bar?' to specified type 'Double?'}} {{25-25=?.rawValue}}
  }
}

struct NotEquatable { }

enum ArrayOfNewEquatable : Array<NotEquatable> { }
// expected-error@-1{{raw type 'Array<NotEquatable>' is not expressible by a string, integer, or floating-point literal}}
// expected-error@-2{{'ArrayOfNewEquatable' declares raw type 'Array<NotEquatable>', but does not conform to RawRepresentable and conformance could not be synthesized}}
// expected-error@-3{{RawRepresentable conformance cannot be synthesized because raw type 'Array<NotEquatable>' is not Equatable}}
// expected-error@-4{{an enum with no cases cannot declare a raw type}}
// expected-note@-5 {{add stubs for conformance}}

// rdar://58127114
struct NotEquatableInteger : ExpressibleByIntegerLiteral {
  typealias IntegerLiteralType = Int

  init(integerLiteral: Int) {}
}

enum NotEquatableRawType1 : NotEquatableInteger {
// expected-error@-1 {{'NotEquatableRawType1' declares raw type 'NotEquatableInteger', but does not conform to RawRepresentable and conformance could not be synthesized}}
// expected-error@-2 {{RawRepresentable conformance cannot be synthesized because raw type 'NotEquatableInteger' is not Equatable}}
// expected-note@-3 {{add stubs for conformance}}
  case a = 123
}


enum NotEquatableRawType2 : NotEquatableInteger {
// expected-error@-1 {{'NotEquatableRawType2' declares raw type 'NotEquatableInteger', but does not conform to RawRepresentable and conformance could not be synthesized}}
// expected-error@-2 {{RawRepresentable conformance cannot be synthesized because raw type 'NotEquatableInteger' is not Equatable}}
// expected-note@-3 {{add stubs for conformance}}
  typealias RawValue = NotEquatableInteger

  case a = 123
}

struct NotEquatableString : ExpressibleByStringLiteral {
  init(stringLiteral: String) {}
}

// FIXME: This could be diagnosed a bit better. The notes are disembodied
enum NotEquatableRawType3: NotEquatableString {
// expected-error@-1 {{RawRepresentable conformance cannot be synthesized because raw type 'NotEquatableString' is not Equatable}}
// expected-error@-2 {{'NotEquatableRawType3' declares raw type 'NotEquatableString', but does not conform to RawRepresentable and conformance could not be synthesized}}
// expected-note@-3 {{add stubs for conformance}}
  case a
  typealias RawValue = NotEquatableString
  init?(rawValue: Int) { self = .a }
  // expected-note@-1 {{candidate has non-matching type '(rawValue: Int)'}}
  var rawValue: Int { 0 }
  // expected-note@-1 {{candidate has non-matching type 'Int'}}
}

enum MismatchedRawValues {
  enum ExistentialBound: Any? {
    // expected-error@-1 {{raw type 'Any?' is not expressible}}
    // expected-error@-2 {{'MismatchedRawValues.ExistentialBound' declares raw type 'Any?'}}
    // expected-error@-3 {{RawRepresentable conformance cannot be synthesized }}
    // expected-note@-4 {{add stubs for conformance}}
    case test = nil
  }

  public enum StringViaStaticString: StaticString {
    // expected-error@-1 {{'MismatchedRawValues.StringViaStaticString' declares raw type 'StaticString', but does not conform to RawRepresentable}}
    // expected-error@-2 {{RawRepresentable conformance cannot be synthesized because}}
    // expected-note@-3 {{add stubs for conformance}}
    public typealias RawValue = String

    case TRUE = "TRUE"
    case FALSE = "FALSE"
  }

  public enum IntViaString: String {
    // expected-error@-1 {{'MismatchedRawValues.IntViaString' declares raw type 'String', but does not conform to RawRepresentable}}
    // expected-note@-2 {{add stubs for conformance}}
    public typealias RawValue = Int

    case TRUE = "TRUE"
    case FALSE = "FALSE"
  }

  public enum ViaNested: String {
    // expected-error@-1 {{'MismatchedRawValues.ViaNested' declares raw type 'String', but does not conform to RawRepresentable}}
    // expected-note@-2 {{add stubs for conformance}}
    struct RawValue: Equatable {
      let x: String
    }

    case TRUE = "TRUE"
    case FALSE = "FALSE"
  }

  public enum ViaGenericBound<RawValue: Equatable>: String {
    // expected-error@-1 {{'MismatchedRawValues.ViaGenericBound<RawValue>' declares raw type 'String', but does not conform to RawRepresentable}}
    // expected-note@-2 {{add stubs for conformance}}
    typealias RawValue = RawValue
    case TRUE = "TRUE"
    case FALSE = "FALSE"
  }
}

