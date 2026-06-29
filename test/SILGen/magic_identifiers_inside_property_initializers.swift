// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-emit-silgen %s | %FileCheck %s

class Test {
    // CHECK-LABEL: sil hidden [transparent] [ossa] @$s46magic_identifiers_inside_property_initializers4TestC4fileSSvpfi
    // CHECK: string_literal utf8 "{{.*}}.swift"
    let file = #file
}
