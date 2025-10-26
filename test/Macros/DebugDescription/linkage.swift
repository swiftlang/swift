// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions > %t/expansions-dump.txt 2>&1
// RUN: %FileCheck %s < %t/expansions-dump.txt

@DebugDescription
struct MyStruct: CustomDebugStringConvertible {
  var debugDescription: String { "thirty" }
}
// CHECK: #if !os(Windows)
// CHECK: #if os(Linux)
// CHECK: @section(".lldbsummaries")
// CHECK: #else
// CHECK: @section("__TEXT,__lldbsummaries")
// CHECK: #endif
// CHECK: @used
// CHECK: static let _lldb_summary: (
// CHECK:   {{UInt8(, UInt8)*}}
// CHECK: ) =
// CHECK: (
// CHECK:     /* version */ 1,
// CHECK:     /* record size */ 23,
// CHECK:     /* "main.MyStruct" */ 14, 109, 97, 105, 110, 46, 77, 121, 83, 116, 114, 117, 99, 116, 0,
// CHECK:     /* "thirty" */ 7, 116, 104, 105, 114, 116, 121, 0
// CHECK: )
// CHECK: #endif
