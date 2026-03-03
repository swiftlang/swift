// RUN: %target-typecheck-verify-swift

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

protocol Format {
  associatedtype FormatOutput
}

struct Date {
  struct FormatStyle {}
  func formatted<F: Format>(_ format: F) -> F.FormatOutput {
    fatalError()
  }
}

extension Date.FormatStyle: Format {
  typealias FormatOutput = String
}

extension Format where Self == Date.FormatStyle {
  static var dateTime: Date.FormatStyle {
    return Date.FormatStyle()
  }
}

extension String {
  var localizedUppercase: String { return self }
}
