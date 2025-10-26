// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions > %t/expansions-dump.txt 2>&1
// RUN: %FileCheck %s < %t/expansions-dump.txt

@DebugDescription
struct MyStruct1: CustomStringConvertible {
  var description: String { "thirty" }
}
// CHECK: static let _lldb_summary: (
// CHECK:   {{UInt8(, UInt8)*}}
// CHECK: ) =
// CHECK: (
// CHECK:     /* version */ 1,
// CHECK:     /* record size */ 24,
// CHECK:     /* "main.MyStruct1" */ 15, 109, 97, 105, 110, 46, 77, 121, 83, 116, 114, 117, 99, 116, 49, 0,
// CHECK:     /* "thirty" */ 7, 116, 104, 105, 114, 116, 121, 0
// CHECK: )

@DebugDescription
struct MyStruct2: CustomDebugStringConvertible {
  var description: String { "thirty" }
  var debugDescription: String { "eleven" }
}
// CHECK: static let _lldb_summary: (
// CHECK:   {{UInt8(, UInt8)*}}
// CHECK: ) =
// CHECK: (
// CHECK:     /* version */ 1,
// CHECK:     /* record size */ 24,
// CHECK:     /* "main.MyStruct2" */ 15, 109, 97, 105, 110, 46, 77, 121, 83, 116, 114, 117, 99, 116, 50, 0,
// CHECK:     /* "eleven" */ 7, 101, 108, 101, 118, 101, 110, 0
// CHECK: )

@DebugDescription
struct MyStruct3: CustomDebugStringConvertible {
  var description: String { "thirty" }
  var debugDescription: String { "eleven" }
  var lldbDescription: String { "two" }
}
// CHECK: static let _lldb_summary: (
// CHECK:   {{UInt8(, UInt8)*}}
// CHECK: ) =
// CHECK: (
// CHECK:     /* version */ 1,
// CHECK:     /* record size */ 21,
// CHECK:     /* "main.MyStruct3" */ 15, 109, 97, 105, 110, 46, 77, 121, 83, 116, 114, 117, 99, 116, 51, 0,
// CHECK:     /* "two" */ 4, 116, 119, 111, 0
