// RUN: %empty-directory(%t)
//
// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck -check-prefix=CHECK %s

public struct CustomFoo: Equatable, Hashable {
    public let a: Int
    public let b: String
}
func testFoo() {
    var hasher = Hasher()
    let f = CustomFoo(a: 0, b: "b")
    // CHECK: [[@LINE+1]]:7 | instance-method/Swift | hash(into:) | s:14swift_ide_test9CustomFooV4hash4intoys6HasherVz_tF | {{.*}}Ref
    f.hash(into: &hasher)
    hasher.finalize()
    // CHECK: [[@LINE+1]]:11 | static-method/Swift | __derived_struct_equals(_:_:) | s:14swift_ide_test9CustomFooV23__derived_struct_equalsySbAC_ACtFZ | {{.*}}Ref
    _ = f == CustomFoo(a: 0, b: "b")
}
