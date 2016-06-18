import has_accessibility

public let a = 0 // expected-note * {{did you mean 'a'?}}
internal let b = 0 // expected-note * {{did you mean 'b'?}}
fileprivate let c = 0

extension Foo {
  public static func a() {}
  internal static func b() {}
  fileprivate static func c() {}  // expected-note {{'c' declared here}}
}

struct PrivateInit {
  fileprivate init() {}  // expected-note {{'init' declared here}}
}

extension Foo {
  fileprivate func method() {}
  fileprivate typealias TheType = Float
}

extension OriginallyEmpty {
  func method() {}
  typealias TheType = Float
}

fileprivate func privateInBothFiles() {}
func privateInPrimaryFile() {} // expected-note {{previously declared here}}
fileprivate func privateInOtherFile() {} // expected-error {{invalid redeclaration}}
