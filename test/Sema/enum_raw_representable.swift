// RUN: %swift -parse -verify %s

enum Foo : Int {
  case A, B, C
}

var raw1: Int = Foo.A.toRaw()
var raw2: Foo.RawType = raw1
var cooked1: Foo? = Foo.fromRaw(0)
var cooked2: Foo? = Foo.fromRaw(22)

enum Bar : Double {
  case A, B, C
}

func localEnum() -> Int {
  enum LocalEnum : Int {
    case A, B, C
  }
  return LocalEnum.A.toRaw()
}

enum MembersReferenceRawType : Int {
  case A, B, C

  init(x:Int) {
    self = MembersReferenceRawType.fromRaw(x)!
  }

  func succ() -> MembersReferenceRawType {
    return MembersReferenceRawType.fromRaw(toRaw() + 1)!
  }
}

func serialize<T : RawRepresentable>(values: T[]) -> T.RawType[] {
  return new T.RawType[values.count] { values[$0].toRaw() }
}

func deserialize<T : RawRepresentable>(serialized: T.RawType[]) -> T[] {
  return new T[serialized.count] { T.fromRaw(serialized[$0])! }
}

var ints: Int[] = serialize([Foo.A, .B, .C])
var doubles: Double[] = serialize([Bar.A, .B, .C])

var foos: Foo[] = deserialize([1, 2, 3])
var bars: Bar[] = deserialize([1.2, 3.4, 5.6])

// Infer RawType from witnesses.
enum Color : Int, RawRepresentable {
  case Red
  case Blue

  static func fromRaw(raw: Double) -> Color? {
    return .None
  }

  func toRaw() -> Double {
    return 1.0
  }
}

var colorRaw: Color.RawType = 7.5
