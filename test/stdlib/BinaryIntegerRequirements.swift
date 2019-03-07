// RUN: %target-typecheck-verify-swift -swift-version 4

struct MyInt: FixedWidthInteger { // expected-error {{type 'MyInt' does not conform to protocol 'BinaryInteger'}}
  typealias IntegerLiteralType = Int
  static let isSigned = false
  init(integerLiteral value: Int) { fatalError() }
  init(_truncatingBits bits: UInt) { fatalError() }
  init<T : BinaryFloatingPoint>(_ source: T) { fatalError() }
  init?<T : BinaryFloatingPoint>(exactly source: T) { fatalError() }
  init<T : BinaryInteger>(_ source: T) { fatalError() }
  init?<T : BinaryInteger>(exactly source: T) { fatalError() }
  init<T : BinaryInteger>(truncatingIfNeeded source: T) { fatalError() }
  init<T : BinaryInteger>(clamping source: T) { fatalError() }

  let words = [UInt]()
  let _lowWord: UInt = 0
  static var bitWidth: Int { fatalError() }
  var trailingZeroBitCount: Int { fatalError() }

  static func /=(_ lhs: inout MyInt, _ rhs: MyInt) { fatalError() }
  static func /(_ lhs: MyInt, _ rhs: MyInt) -> MyInt { fatalError() }
  static func %=(_ lhs: inout MyInt, _ rhs: MyInt) { fatalError() }
  static func %(_ lhs: MyInt, _ rhs: MyInt) -> MyInt { fatalError() }
  static func +=(_ lhs: inout MyInt, _ rhs: MyInt) { fatalError() }
  static func +(_ lhs: MyInt, _ rhs: MyInt) -> MyInt { fatalError() }
  static func -=(_ lhs: inout MyInt, _ rhs: MyInt) { fatalError() }
  static func -(_ lhs: MyInt, _ rhs: MyInt) -> MyInt { fatalError() }
  static func *=(_ lhs: inout MyInt, _ rhs: MyInt) { fatalError() }
  static func *(_ lhs: MyInt, _ rhs: MyInt) -> MyInt { fatalError() }

  static func ==(_ lhs: MyInt, _ rhs: MyInt) -> Bool { fatalError() }
  static func <(_ lhs: MyInt, _ rhs: MyInt) -> Bool { fatalError() }

  static prefix func ~ (_ x: MyInt) -> MyInt { fatalError() }

  static func >><RHS: BinaryInteger>(_ lhs: MyInt, _ rhs: RHS) -> MyInt { fatalError() }

  static func >>=<RHS: BinaryInteger>(_ lhs: inout MyInt, _ rhs: RHS) { fatalError() } 
  static func <<<RHS: BinaryInteger>(_ lhs: MyInt, _ rhs: RHS) -> MyInt { fatalError() }
  static func <<=<RHS: BinaryInteger>(_ lhs: inout MyInt, _ rhs: RHS) { fatalError() }

  func quotientAndRemainder(dividingBy rhs: MyInt) -> (quotient: MyInt, remainder: MyInt) { fatalError() }
  func signum() -> MyInt { fatalError() }

  func hash(into hasher: inout Hasher) { fatalError() }
  var byteSwapped: MyInt { fatalError() }
  static var max: MyInt { fatalError() }
  static var min: MyInt { fatalError() }
  func addingReportingOverflow(_ rhs: MyInt) -> (partialValue: MyInt, overflow: Bool) { fatalError() }
  func subtractingReportingOverflow(_ rhs: MyInt) -> (partialValue: MyInt, overflow: Bool) { fatalError() }
  func multipliedReportingOverflow(by rhs: MyInt) -> (partialValue: MyInt, overflow: Bool) { fatalError() }
  func dividedReportingOverflow(by rhs: MyInt) -> (partialValue: MyInt, overflow: Bool) { fatalError() }
  func remainderReportingOverflow(dividingBy rhs: MyInt) -> (partialValue: MyInt, overflow: Bool) { fatalError() }
  func multipliedFullWidth(by other: MyInt) -> (high: MyInt, low: Magnitude) { fatalError() }
  func dividingFullWidth(_ dividend: (high: MyInt, low: Magnitude)) -> (quotient: MyInt, remainder: MyInt) { fatalError() }

  var nonzeroBitCount: Int { fatalError() }
  var leadingZeroBitCount: Int { fatalError() }

  var magnitude: UInt { fatalError() }
}
