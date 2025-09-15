// RUN: %target-swift-frontend -typecheck -verify -primary-file %s %S/../Inputs/case_iterable_other.swift

enum Simple: CaseIterable {
  case a, b

  static func staticMethod() -> Int {
    return Self.allCases.count
  }
}

enum Generic<T>: CaseIterable {
  case a, b

  static func staticMethod() -> Int {
    return Self.allCases.count
  }
}

enum InExtension {
  case a, b
}

extension InExtension: CaseIterable {}

enum UnavailableCase: CaseIterable {
  case a
  @available(*, unavailable)
  case b

  public static var allCases: [UnavailableCase] {
    return [.a]
  }
}

extension FromOtherFile: CaseIterable {
  public static var allCases: [FromOtherFile] {
    return [.a, .b]
  }
}

enum InvalidAvailableAttribute: CaseIterable {
  case a
  @available(deprecated, renamed: "a") // expected-warning {{unrecognized platform name 'deprecated'}}
  case b
}
