// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

struct Box<Representation, T> {
    let value: Representation
}
enum Repr {}
extension Repr {
    typealias RawEnum = ()
}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=Box
// CHECK-NEXT: Generic signature: <Representation, T where Representation == Repr, T == ()>
extension Box where Representation == Repr, T == Representation.RawEnum {
    init(rawEnumValue: Representation.RawEnum) {
        let _: ().Type = T.self
        fatalError()
    }
}
