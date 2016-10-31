// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-ir -verify %s

public struct TestGeneratorCrashy <Key: AnyObject, Value: AnyObject> {
    public mutating func next() -> (Key, Value)? {
        return nil
    }
}

extension TestGeneratorCrashy: IteratorProtocol {
}
