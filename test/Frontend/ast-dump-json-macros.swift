// Checks that macro-expanded members are dumped and that their source ranges
// contain the appropriate buffer ID.

// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend -target %target-swift-5.9-abi-triple -disable-availability-checking -plugin-path %swift-plugin-dir -parse-as-library -dump-ast -dump-ast-format json %s -module-name main -o - | %FileCheck %s

@DebugDescription
struct X {
    var y: Int

    var debugDescription: String {
        "y is \(y)"
    }
}

// CHECK: "buffer_id":"@__swiftmacro_
