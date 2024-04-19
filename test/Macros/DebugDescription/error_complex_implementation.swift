// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -verify -plugin-path %swift-plugin-dir

@DebugDescription
struct MyStruct {
  var flag: Bool

  // expected-error @+1 {{body must consist of a single string literal}}
  var debugDescription: String {
    flag ? "yes" : "no"
  }
}
