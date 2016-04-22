// RUN: %target-swift-frontend -emit-ir -disable-incremental-llvm-codegen -verify %s

public struct TestGeneratorCrashy <Key: AnyObject, Value: AnyObject> {
    public mutating func next() -> (Key, Value)? {
        return nil
    }
}

extension TestGeneratorCrashy: IteratorProtocol {
}
