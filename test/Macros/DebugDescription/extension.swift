// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions > %t/expansions-dump.txt 2>&1
// RUN: %FileCheck %s < %t/expansions-dump.txt

struct MyStruct {}

@DebugDescription
extension MyStruct {
  var debugDescription: String { "thirty" }
}
// CHECK: static let _lldb_summary = (
// CHECK:     /* version */ 1 as UInt8,
// CHECK:     /* record size */ 34 as UInt8,
// CHECK:     /* "^main[.]MyStruct(<.+>)?$" */ 25 as UInt8, 94 as UInt8, 109 as UInt8, 97 as UInt8, 105 as UInt8, 110 as UInt8, 91 as UInt8, 46 as UInt8, 93 as UInt8, 77 as UInt8, 121 as UInt8, 83 as UInt8, 116 as UInt8, 114 as UInt8, 117 as UInt8, 99 as UInt8, 116 as UInt8, 40 as UInt8, 60 as UInt8, 46 as UInt8, 43 as UInt8, 62 as UInt8, 41 as UInt8, 63 as UInt8, 36 as UInt8, 0 as UInt8,
// CHECK:     /* "thirty" */ 7 as UInt8, 116 as UInt8, 104 as UInt8, 105 as UInt8, 114 as UInt8, 116 as UInt8, 121 as UInt8, 0 as UInt8
// CHECK: )
