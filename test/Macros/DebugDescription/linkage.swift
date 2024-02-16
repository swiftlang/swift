// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -enable-experimental-feature SymbolLinkageMarkers -plugin-path %swift-plugin-dir -dump-macro-expansions > %t/expansions-dump.txt 2>&1
// RUN: %FileCheck %s < %t/expansions-dump.txt

@_DebugDescription
struct MyStruct: CustomDebugStringConvertible {
  var debugDescription: String { "thirty" }
}
// CHECK: #if os(Linux)
// CHECK: @_section(".lldbsummaries")
// CHECK: #elseif os(Windows)
// CHECK: @_section(".lldbsummaries")
// CHECK: #else
// CHECK: @_section("__TEXT,__lldbsummaries")
// CHECK: #endif
// CHECK: @_used
// CHECK: static let _lldb_summary = (

