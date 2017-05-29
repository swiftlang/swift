// RUN: %target-run-simple-swift

// REQUIRES: executable_test

enum Foo<T : ExpressibleByIntegerLiteral> : T where T:Equatable {
  case a = 1
  case b = 10
  case c = 42
  case d // = 43 by auto-incrementing
}

struct MyNumber : ExpressibleByIntegerLiteral, Equatable {
  typealias IntegerLiteralType = Int
  let _storage : Int
  init(integerLiteral: Int) {
    _storage = integerLiteral
  }
  static func ==(lhs: MyNumber, rhs: MyNumber) -> Bool {
    return lhs._storage == rhs._storage
  }
}

let _ : Int8  = Foo.a.rawValue
let _ : Int16 = Foo.a.rawValue
let _ : Int32 = Foo.a.rawValue
let _ : Int64 = Foo.a.rawValue
let _ : UInt8  = Foo.a.rawValue
let _ : UInt16 = Foo.a.rawValue
let _ : UInt32 = Foo.a.rawValue
let _ : UInt64 = Foo.a.rawValue
let _ : MyNumber = Foo.a.rawValue

guard case .c = Foo(rawValue: UInt8(42))!  else { fatalError() }
guard case .c = Foo(rawValue: UInt16(42))! else { fatalError() }
guard case .c = Foo(rawValue: UInt32(42))! else { fatalError() }
guard case .c = Foo(rawValue: UInt64(42))! else { fatalError() }
guard case .c = Foo(rawValue: MyNumber(integerLiteral: 42))! else { fatalError() }

guard case .d = Foo(rawValue: MyNumber(integerLiteral: 43))! else { fatalError() }
