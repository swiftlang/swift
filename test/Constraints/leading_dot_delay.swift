// RUN: %target-typecheck-verify-swift

protocol FormatStyle {
  associatedtype FormatOutput
}

struct Date {
  func formatted<F: FormatStyle>(_ format: F) -> F.FormatOutput {
    fatalError()
  }
}

func broken1(_ date: Date) -> String {
  return date.formatted(.dateTime).localizedUppercase
}

func broken2(_ date: Date) -> AnyHashable {
  return date.formatted(.dateTime).localizedUppercase
  // expected-error@-1 {{failed to produce diagnostic for expression}}
  // FIXME: This should work
}

func broken3(_ date: Date) -> Any {
  return date.formatted(.dateTime).localizedUppercase
}

struct DateFormatStyle: FormatStyle {
  typealias FormatOutput = String
}

extension FormatStyle where Self == DateFormatStyle {
  static var dateTime: DateFormatStyle {
    return DateFormatStyle()
  }
}

extension String {
  var localizedUppercase: String { return self }
}

// Another reduction using the Foundation formatting API which exposed
// a bad interaction between leading dot members and the common result
// type optimization.
public struct MyFormatStyle: FormatStyle {
  typealias FormatOutput = String

  public func locale() -> Self { self }
}

extension FormatStyle where Self == MyFormatStyle {
  static func mine1(_: Int) -> Self { fatalError() }
  static func mine1(_: Bool) -> Self { fatalError() }

  static func mine2(_: Int) -> MyFormatStyle { fatalError() }
  static func mine2(_: Bool) -> MyFormatStyle { fatalError() }
}

func formatted<T: FormatStyle>(_: T) {}

func fancy(value: Int) {
  formatted(.mine1(value)) // works in 6.3
  formatted(.mine1(value).locale()) // works in 6.3
  formatted(.mine2(value)) // works in 6.3
  formatted(.mine2(value).locale()) // fails in 6.3
}
