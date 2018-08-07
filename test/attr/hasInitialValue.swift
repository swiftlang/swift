// RUN: %target-swift-ide-test -skip-deinit=false -print-ast-typechecked -source-filename %s -function-definitions=true -prefer-type-repr=false -print-implicit-attrs=true -explode-pattern-binding-decls=true -swift-version 4 -enable-source-import -I %S/Inputs | %FileCheck %s

// CHECK-LABEL: {{^}}class C {
class C {
    // CHECK: {{^}}  var without: Int
    var without: Int
    // CHECK: {{^}}  @_hasInitialValue var with: Int
    var with: Int = 0

    // CHECK: {{^}}  @_hasInitialValue var option: Int
    var option: Int?
    // CHECK: {{^}}  @_implicitly_unwrapped_optional @_hasInitialValue var iuo: Int!
    var iuo: Int!

    // CHECK: {{^}}  lazy var lazyIsntARealInit: Int
    lazy var lazyIsntARealInit: Int = 0

    init() {
        without = 0
    }
}
