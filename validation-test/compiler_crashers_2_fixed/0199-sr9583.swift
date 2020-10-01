// RUN: %target-swift-frontend -emit-ir -primary-file %s %S/Inputs/sr9583-other.swift -module-name foo

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
