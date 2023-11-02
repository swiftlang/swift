// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -typecheck -verify -plugin-path %swift-plugin-dir

@attached(peer, names: suffixed(_lldb_summary))
public macro _DebugDescription() =
  #externalMacro(module: "SwiftMacros", type: "DebugDescriptionMacro")

@_DebugDescription
struct MyStruct {
  var flag: Bool

  // expected-error @+1 {{debugDescription must consist of a single string literal}}
  var debugDescription: String {
    flag ? "yes" : "no"
  }
}
