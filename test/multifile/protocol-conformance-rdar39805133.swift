// RUN: %target-swift-frontend -verify -emit-ir -o - -primary-file %s %S/Inputs/protocol-conformance-rdar39805133-other.swift -module-name foo -requirement-machine-protocol-signatures=on -requirement-machine-inferred-signatures=on
// RUN: %target-swift-frontend -emit-ir -o - %s -primary-file %S/Inputs/protocol-conformance-rdar39805133-other.swift -module-name foo -requirement-machine-protocol-signatures=on -requirement-machine-inferred-signatures=on

protocol _Int : DefaultInit {
    associatedtype Minus1 : _Int
    associatedtype Plus1 : _Int = Inc<Self>
    static var value: Int { get }
}

struct Inc<T : _Int> : _Int {
    typealias Minus1 = T
    static var value: Int { return T.value + 1 }
}

extension _Int {
    var plus1: Plus1 { return Plus1() }
    var minus1: Minus1 { return Minus1() }
}

struct _0_ : _Int {
    typealias Minus1 = _0_//Underflow
    static let value = 0
}

let _0 = _0_()
protocol AtLeast0 : _Int {}
extension _0_ : AtLeast0 {}
protocol AtLeast1 : AtLeast0 {}
extension Inc : AtLeast1, AtLeast0 where T == _0_ {}
// expected-error@-1 {{cannot build rewrite system for generic signature; rule length limit exceeded}}
// expected-note@-2 {{failed rewrite rule is τ_0_0.[_Int:Plus1].[_Int:Plus1].[_Int:Plus1].[_Int:Plus1].[_Int:Plus1].[_Int:Plus1].[_Int:Plus1].[_Int:Plus1].[_Int:Plus1].[_Int:Plus1].[_Int:Plus1].[concrete: Inc<Inc<Inc<Inc<Inc<Inc<Inc<Inc<Inc<Inc<Inc<_0_>>>>>>>>>>>] => τ_0_0.[_Int:Plus1].[_Int:Plus1].[_Int:Plus1].[_Int:Plus1].[_Int:Plus1].[_Int:Plus1].[_Int:Plus1].[_Int:Plus1].[_Int:Plus1].[_Int:Plus1].[_Int:Plus1]}}