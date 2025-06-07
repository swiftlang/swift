// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -verify -plugin-path %swift-plugin-dir

// expected-error @+1 {{cannot be attached to a protocol}}
@DebugDescription
protocol MyProto {
  func action()
}
