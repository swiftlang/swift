// rdar://186868294 (https://github.com/swiftlang/swift/issues/92047)
// RUN: %target-typecheck-verify-swift -swift-version 5

@resultBuilder
struct Builder {
  static func buildBlock<C: P>(_ c: C) -> C { c }
}

protocol P {
  associatedtype Body: P
  @Builder var body: Body { get }
}

extension Never: P {
  var body: Never { fatalError() }
}

protocol Style {
  associatedtype Input
}

struct Option {
  static var never: Option { fatalError() }
}

struct Decimal {
  struct FormatStyle: Style {
    typealias Input = Decimal
    func grouping(_ o: Option) -> Self { self }
  }
}

struct Half {}

struct FloatingPointFormatStyle<T>: Style {
  typealias Input = T
  func grouping(_ o: Option) -> Self { self }
}

struct IntegerFormatStyle<T>: Style {
  typealias Input = T
  func grouping(_ o: Option) -> Self { self }
}

extension Style where Self == Decimal.FormatStyle {
  static var number: Self { fatalError() }
}

extension Style where Self == FloatingPointFormatStyle<Double> {
  static var number: Self { fatalError() }
}

extension Style where Self == FloatingPointFormatStyle<Float> {
  static var number: Self { fatalError() }
}

extension Style where Self == FloatingPointFormatStyle<Half> {
  static var number: Self { fatalError() }
}

extension Style where Self == IntegerFormatStyle<Int> {
  static var number: Self { fatalError() }
}

extension Style where Self == IntegerFormatStyle<Int16> {
  static var number: Self { fatalError() }
}

extension Style where Self == IntegerFormatStyle<Int32> {
  static var number: Self { fatalError() }
}

extension Style where Self == IntegerFormatStyle<Int64> {
  static var number: Self { fatalError() }
}

extension Style where Self == IntegerFormatStyle<Int8> {
  static var number: Self { fatalError() }
}

extension Style where Self == IntegerFormatStyle<UInt> {
  static var number: Self { fatalError() }
}

extension Style where Self == IntegerFormatStyle<UInt16> {
  static var number: Self { fatalError() }
}

extension Style where Self == IntegerFormatStyle<UInt32> {
  static var number: Self { fatalError() }
}

extension Style where Self == IntegerFormatStyle<UInt64> {
  static var number: Self { fatalError() }
}

extension Style where Self == IntegerFormatStyle<UInt8> {
  static var number: Self { fatalError() }
}

struct Key: ExpressibleByStringInterpolation {
  init(stringLiteral value: String) {}
  init(stringInterpolation: StringInterpolation) {}

  struct StringInterpolation: StringInterpolationProtocol {
    init(literalCapacity: Int, interpolationCount: Int) {}
    mutating func appendLiteral(_ literal: String) {}
    mutating func appendInterpolation<F: Style>(_ input: F.Input, format: F) {}
  }
}

struct Text: P {
  var body: Never { fatalError() }
  init(_ key: Key) {}
}

struct TestView: P {
  var year = "2010"

  var body: some P { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    Text("\(year, format: .number.grouping(.never))")
  }
}
