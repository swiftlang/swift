// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

class Test {
    // CHECK-LABEL: sil hidden [transparent] @_TIvC46magic_identifiers_inside_property_initializers4Test4fileSSi
    // CHECK: string_literal utf16 "{{.*}}.swift"
    let file = #file
}
