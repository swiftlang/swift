// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

class Test {
    // CHECK-LABEL: sil hidden [transparent] @_T046magic_identifiers_inside_property_initializers4TestC4fileSSvfi
    // CHECK: string_literal utf16 "{{.*}}.swift"
    let file = #file
}
