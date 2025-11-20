// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions > %t/expansions-dump.txt 2>&1
// RUN: %FileCheck %s < %t/expansions-dump.txt

@DebugDescription
struct MyStruct: CustomDebugStringConvertible {
  var name: String = "thirty"
  var debugDescription: String { "\(self.name.count) \(name.count)" }
}
// CHECK: static let _lldb_summary: (
// CHECK:   {{UInt8(, UInt8)*}}
// CHECK: ) =
// CHECK: (
// CHECK:     /* version */ 1,
// CHECK:     /* record size */ 52,
// CHECK:     /* "main.MyStruct" */ 14, 109, 97, 105, 110, 46, 77, 121, 83, 116, 114, 117, 99, 116, 0,
// CHECK:     /* "${var.name.count} ${var.name.count}" */ 36, 36, 123, 118, 97, 114, 46, 110, 97, 109, 101, 46, 99, 111, 117, 110, 116, 125, 32, 36, 123, 118, 97, 114, 46, 110, 97, 109, 101, 46, 99, 111, 117, 110, 116, 125, 0
// CHECK: )

@DebugDescription
class MyClass: CustomDebugStringConvertible {
  var name: String = "thirty"
  var debugDescription: String { "\(self.name.count) \(name.count)" }
}
// CHECK: static let _lldb_summary: (
// CHECK:   {{UInt8(, UInt8)*}}
// CHECK: ) =
// CHECK: (
// CHECK:     /* version */ 1,
// CHECK:     /* record size */ 51,
// CHECK:     /* "main.MyClass" */ 13, 109, 97, 105, 110, 46, 77, 121, 67, 108, 97, 115, 115, 0,
// CHECK:     /* "${var.name.count} ${var.name.count}" */ 36, 36, 123, 118, 97, 114, 46, 110, 97, 109, 101, 46, 99, 111, 117, 110, 116, 125, 32, 36, 123, 118, 97, 114, 46, 110, 97, 109, 101, 46, 99, 111, 117, 110, 116, 125, 0
// CHECK: )
