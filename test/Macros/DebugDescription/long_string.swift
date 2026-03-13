// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions > %t/expansions-dump.txt 2>&1
// RUN: %FileCheck %s < %t/expansions-dump.txt

@DebugDescription
struct MyStruct: CustomDebugStringConvertible {
  var debugDescription: String {
    """
    A string longer than one hundred and twenty eight characters, for the purpose of
    testing Unsigned Little Endian encoding (ULEB). Values less than or equal to 127 are
    represented as a single byte, with a bit representation identical to an unsigned
    8-bit integer. See https://en.wikipedia.org/wiki/LEB128
    """
  }
}
/// CHECK: static let _lldb_summary: (
// CHECK:   {{UInt8(, UInt8)*}}
// CHECK: ) =
// CHECK: (
// CHECK:     /* version */ 1,
// CHECK:     /* record size */ 192, 2,
// CHECK: 8-bit integer. See https://en.wikipedia.org/wiki/LEB128" */ 175, 2, 65,
