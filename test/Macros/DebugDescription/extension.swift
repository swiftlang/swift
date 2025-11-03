// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions > %t/expansions-dump.txt 2>&1
// RUN: %FileCheck %s < %t/expansions-dump.txt

struct MyStruct {}

@DebugDescription
extension MyStruct {
  var debugDescription: String { "thirty" }
}
// CHECK: static let _lldb_summary: (
// CHECK:   {{UInt8(, UInt8)*}}
// CHECK: ) =
// CHECK: (
// CHECK:     /* version */ 1,
// CHECK:     /* record size */ 34,
// CHECK:     /* "^main[.]MyStruct(<.+>)?$" */ 25, 94, 109, 97, 105, 110, 91, 46, 93, 77, 121, 83, 116, 114, 117, 99, 116, 40, 60, 46, 43, 62, 41, 63, 36, 0,
// CHECK:     /* "thirty" */ 7, 116, 104, 105, 114, 116, 121, 0
// CHECK: )
