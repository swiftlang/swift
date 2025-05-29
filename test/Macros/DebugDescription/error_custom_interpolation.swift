// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -verify -plugin-path %swift-plugin-dir

extension DefaultStringInterpolation {
  fileprivate func appendInterpolation(custom: Int) {}
  fileprivate func appendInterpolation<A, B>(_ a: A, _ b: B) {}
}

@DebugDescription
struct MyStruct1 {
  // expected-error @+1 {{unsupported custom string interpolation expression}}
  var debugDescription: String { "\(custom: 30)" }
}

@DebugDescription
struct MyStruct2 {
  // expected-error @+1 {{unsupported custom string interpolation expression}}
  var debugDescription: String { "\(30, true)" }
}
