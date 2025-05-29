// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -verify -plugin-path %swift-plugin-dir

@DebugDescription
struct MyStruct {
  // expected-error @+1 {{only references to stored properties are allowed}}
  var debugDescription: String { "\(1)" }
}
