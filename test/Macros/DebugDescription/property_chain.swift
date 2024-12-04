// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_DebugDescriptionMacro

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -enable-experimental-feature DebugDescriptionMacro -plugin-path %swift-plugin-dir -dump-macro-expansions > %t/expansions-dump.txt 2>&1
// RUN: %FileCheck %s < %t/expansions-dump.txt

@DebugDescription
struct MyStruct: CustomDebugStringConvertible {
  var name: String = "thirty"
  var debugDescription: String { "\(self.name.count) \(name.count)" }
}
// CHECK: static let _lldb_summary = (
// CHECK:     /* version */ 1 as UInt8,
// CHECK:     /* record size */ 52 as UInt8,
// CHECK:     /* "main.MyStruct" */ 14 as UInt8, 109 as UInt8, 97 as UInt8, 105 as UInt8, 110 as UInt8, 46 as UInt8, 77 as UInt8, 121 as UInt8, 83 as UInt8, 116 as UInt8, 114 as UInt8, 117 as UInt8, 99 as UInt8, 116 as UInt8, 0 as UInt8,
// CHECK:     /* "${var.name.count} ${var.name.count}" */ 36 as UInt8, 36 as UInt8, 123 as UInt8, 118 as UInt8, 97 as UInt8, 114 as UInt8, 46 as UInt8, 110 as UInt8, 97 as UInt8, 109 as UInt8, 101 as UInt8, 46 as UInt8, 99 as UInt8, 111 as UInt8, 117 as UInt8, 110 as UInt8, 116 as UInt8, 125 as UInt8, 32 as UInt8, 36 as UInt8, 123 as UInt8, 118 as UInt8, 97 as UInt8, 114 as UInt8, 46 as UInt8, 110 as UInt8, 97 as UInt8, 109 as UInt8, 101 as UInt8, 46 as UInt8, 99 as UInt8, 111 as UInt8, 117 as UInt8, 110 as UInt8, 116 as UInt8, 125 as UInt8, 0 as UInt8
// CHECK: )

@DebugDescription
class MyClass: CustomDebugStringConvertible {
  var name: String = "thirty"
  var debugDescription: String { "\(self.name.count) \(name.count)" }
}
// CHECK: static let _lldb_summary = (
// CHECK:     /* version */ 1 as UInt8,
// CHECK:     /* record size */ 51 as UInt8,
// CHECK:     /* "main.MyClass" */ 13 as UInt8, 109 as UInt8, 97 as UInt8, 105 as UInt8, 110 as UInt8, 46 as UInt8, 77 as UInt8, 121 as UInt8, 67 as UInt8, 108 as UInt8, 97 as UInt8, 115 as UInt8, 115 as UInt8, 0 as UInt8,
// CHECK:     /* "${var.name.count} ${var.name.count}" */ 36 as UInt8, 36 as UInt8, 123 as UInt8, 118 as UInt8, 97 as UInt8, 114 as UInt8, 46 as UInt8, 110 as UInt8, 97 as UInt8, 109 as UInt8, 101 as UInt8, 46 as UInt8, 99 as UInt8, 111 as UInt8, 117 as UInt8, 110 as UInt8, 116 as UInt8, 125 as UInt8, 32 as UInt8, 36 as UInt8, 123 as UInt8, 118 as UInt8, 97 as UInt8, 114 as UInt8, 46 as UInt8, 110 as UInt8, 97 as UInt8, 109 as UInt8, 101 as UInt8, 46 as UInt8, 99 as UInt8, 111 as UInt8, 117 as UInt8, 110 as UInt8, 116 as UInt8, 125 as UInt8, 0 as UInt8
// CHECK: )

