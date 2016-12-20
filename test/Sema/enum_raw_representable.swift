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

enum BadPlain : UInt { // expected-error {{'BadPlain' declares raw type 'UInt', but does not conform to RawRepresentable and conformance could not be synthesized}}
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

  enum E : Array<Int> { // expected-error {{raw type 'Array<Int>' is not expressible by any literal}}
  // expected-error@-1 {{'Outer.E' declares raw type 'Array<Int>', but does not conform to RawRepresentable and conformance could not be synthesized}}
  // expected-error@-2 {{RawRepresentable conformance cannot be synthesized because raw type 'Array<Int>' is not Equatable}}
    case a
  }
}
