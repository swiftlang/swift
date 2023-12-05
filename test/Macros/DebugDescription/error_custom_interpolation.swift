// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -typecheck -verify -plugin-path %swift-plugin-dir

@attached(memberAttribute)
public macro _DebugDescription() =
  #externalMacro(module: "SwiftMacros", type: "DebugDescriptionMacro")

@attached(peer, names: named(_lldb_summary))
public macro _DebugDescriptionProperty(_ debugIdentifier: String, _ computedProperties: [String]) =
  #externalMacro(module: "SwiftMacros", type: "_DebugDescriptionPropertyMacro")

extension DefaultStringInterpolation {
  fileprivate func appendInterpolation(custom: Int) {}
  fileprivate func appendInterpolation<A, B>(_ a: A, _ b: B) {}
}

@_DebugDescription
struct MyStruct1 {
  // expected-error @+1 {{unsupported custom string interpolation expression}}
  var debugDescription: String { "\(custom: 30)" }
}

@_DebugDescription
struct MyStruct2 {
  // expected-error @+1 {{unsupported custom string interpolation expression}}
  var debugDescription: String { "\(30, true)" }
}
