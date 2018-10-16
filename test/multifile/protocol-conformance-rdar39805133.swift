// RUN: %target-swift-frontend -emit-ir -o - -primary-file %s %S/Inputs/protocol-conformance-rdar39805133-other.swift -module-name foo
// RUN: %target-swift-frontend -emit-ir -o - %s -primary-file %S/Inputs/protocol-conformance-rdar39805133-other.swift -module-name foo

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
