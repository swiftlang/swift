// RUN: %target-parse-verify-swift

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
enum Color : Int, RawRepresentable {
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
