// RUN: %target-parse-verify-swift

enum Foo : Int {
  case A, B, C
}

var raw1: Int = Foo.A.rawValue
var raw2: Foo.RawValue = raw1
var cooked1: Foo? = Foo(rawValue: 0)
var cooked2: Foo? = Foo(rawValue: 22)

enum Bar : Double {
  case A, B, C
}

func localEnum() -> Int {
  enum LocalEnum : Int {
    case A, B, C
  }
  return LocalEnum.A.rawValue
}

enum MembersReferenceRawType : Int {
  case A, B, C

  init?(rawValue: Int) {
    self = MembersReferenceRawType(rawValue: rawValue)!
  }

  func successor() -> MembersReferenceRawType {
    return MembersReferenceRawType(rawValue: rawValue + 1)!
  }
}

func serialize<T : RawRepresentable>(values: [T]) -> [T.RawValue] {
  return values.map { $0.rawValue }
}

func deserialize<T : RawRepresentable>(serialized: [T.RawValue]) -> [T] {
  return serialized.map { T(rawValue: $0)! }
}

var ints: [Int] = serialize([Foo.A, .B, .C])
var doubles: [Double] = serialize([Bar.A, .B, .C])

var foos: [Foo] = deserialize([1, 2, 3])
var bars: [Bar] = deserialize([1.2, 3.4, 5.6])

// Infer RawValue from witnesses.
enum Color : Int, RawRepresentable {
  case Red
  case Blue

  init?(rawValue: Double) {
    return nil
  }

  var rawValue: Double {
    return 1.0
  }
}

var colorRaw: Color.RawValue = 7.5
