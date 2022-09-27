// RUN: %target-swift-frontend -emit-ir -primary-file %s %S/Inputs/issue-52030-other.swift -module-name foo

// https://github.com/apple/swift/issues/52030

protocol P {
    associatedtype A
    typealias T = S1<Self>
}

struct S1<G: P>: Sequence, IteratorProtocol {
    mutating func next() -> G.A? {
        return nil
    }
}

func f(_: LazyFilterSequence<S3.T>) { }
