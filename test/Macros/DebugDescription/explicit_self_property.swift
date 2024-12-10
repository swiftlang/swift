// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions > %t/expansions-dump.txt 2>&1
// RUN: %FileCheck %s < %t/expansions-dump.txt

@DebugDescription
struct MyStruct: CustomDebugStringConvertible {
  var name: String = "thirty"
  var debugDescription: String { "name: \(self.name)" }
}
// CHECK: static let _lldb_summary = (
// CHECK:     /* version */ 1 as UInt8,
// CHECK:     /* record size */ 34 as UInt8,
// CHECK:     /* "main.MyStruct" */ 14 as UInt8, 109 as UInt8, 97 as UInt8, 105 as UInt8, 110 as UInt8, 46 as UInt8, 77 as UInt8, 121 as UInt8, 83 as UInt8, 116 as UInt8, 114 as UInt8, 117 as UInt8, 99 as UInt8, 116 as UInt8, 0 as UInt8,
// CHECK:     /* "name: ${var.name}" */ 18 as UInt8, 110 as UInt8, 97 as UInt8, 109 as UInt8, 101 as UInt8, 58 as UInt8, 32 as UInt8, 36 as UInt8, 123 as UInt8, 118 as UInt8, 97 as UInt8, 114 as UInt8, 46 as UInt8, 110 as UInt8, 97 as UInt8, 109 as UInt8, 101 as UInt8, 125 as UInt8, 0 as UInt8
// CHECK: )

@DebugDescription
class MyClass: CustomDebugStringConvertible {
  var name: String = "thirty"
  var debugDescription: String { "name: \(self.name)" }
}
// CHECK: static let _lldb_summary = (
// CHECK:     /* version */ 1 as UInt8,
// CHECK:     /* record size */ 33 as UInt8,
// CHECK:     /* "main.MyClass" */ 13 as UInt8, 109 as UInt8, 97 as UInt8, 105 as UInt8, 110 as UInt8, 46 as UInt8, 77 as UInt8, 121 as UInt8, 67 as UInt8, 108 as UInt8, 97 as UInt8, 115 as UInt8, 115 as UInt8, 0 as UInt8,
// CHECK:     /* "name: ${var.name}" */ 18 as UInt8, 110 as UInt8, 97 as UInt8, 109 as UInt8, 101 as UInt8, 58 as UInt8, 32 as UInt8, 36 as UInt8, 123 as UInt8, 118 as UInt8, 97 as UInt8, 114 as UInt8, 46 as UInt8, 110 as UInt8, 97 as UInt8, 109 as UInt8, 101 as UInt8, 125 as UInt8, 0 as UInt8
// CHECK: )

