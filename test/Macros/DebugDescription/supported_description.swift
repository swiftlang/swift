// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions > %t/expansions-dump.txt 2>&1
// RUN: %FileCheck %s < %t/expansions-dump.txt

@DebugDescription
struct MyStruct1: CustomStringConvertible {
  var description: String { "thirty" }
}
// CHECK: static let _lldb_summary = (
// CHECK:     /* version */ 1 as UInt8,
// CHECK:     /* record size */ 24 as UInt8,
// CHECK:     /* "main.MyStruct1" */ 15 as UInt8, 109 as UInt8, 97 as UInt8, 105 as UInt8, 110 as UInt8, 46 as UInt8, 77 as UInt8, 121 as UInt8, 83 as UInt8, 116 as UInt8, 114 as UInt8, 117 as UInt8, 99 as UInt8, 116 as UInt8, 49 as UInt8, 0 as UInt8,
// CHECK:     /* "thirty" */ 7 as UInt8, 116 as UInt8, 104 as UInt8, 105 as UInt8, 114 as UInt8, 116 as UInt8, 121 as UInt8, 0 as UInt8
// CHECK: )

@DebugDescription
struct MyStruct2: CustomDebugStringConvertible {
  var description: String { "thirty" }
  var debugDescription: String { "eleven" }
}
// CHECK: static let _lldb_summary = (
// CHECK:     /* version */ 1 as UInt8,
// CHECK:     /* record size */ 24 as UInt8,
// CHECK:     /* "main.MyStruct2" */ 15 as UInt8, 109 as UInt8, 97 as UInt8, 105 as UInt8, 110 as UInt8, 46 as UInt8, 77 as UInt8, 121 as UInt8, 83 as UInt8, 116 as UInt8, 114 as UInt8, 117 as UInt8, 99 as UInt8, 116 as UInt8, 50 as UInt8, 0 as UInt8,
// CHECK:     /* "eleven" */ 7 as UInt8, 101 as UInt8, 108 as UInt8, 101 as UInt8, 118 as UInt8, 101 as UInt8, 110 as UInt8, 0 as UInt8
// CHECK: )

@DebugDescription
struct MyStruct3: CustomDebugStringConvertible {
  var description: String { "thirty" }
  var debugDescription: String { "eleven" }
  var lldbDescription: String { "two" }
}
// CHECK: static let _lldb_summary = (
// CHECK:     /* version */ 1 as UInt8,
// CHECK:     /* record size */ 21 as UInt8,
// CHECK:     /* "main.MyStruct3" */ 15 as UInt8, 109 as UInt8, 97 as UInt8, 105 as UInt8, 110 as UInt8, 46 as UInt8, 77 as UInt8, 121 as UInt8, 83 as UInt8, 116 as UInt8, 114 as UInt8, 117 as UInt8, 99 as UInt8, 116 as UInt8, 51 as UInt8, 0 as UInt8,
// CHECK:     /* "two" */ 4 as UInt8, 116 as UInt8, 119 as UInt8, 111 as UInt8, 0 as UInt8
