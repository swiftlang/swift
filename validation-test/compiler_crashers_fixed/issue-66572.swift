// RUN: %target-swift-frontend -emit-ir %s

public final class TypedNode {
    public var property: String?
    
    public func withProperty(_ generator: (Self) -> String) -> Self {
        self.property = generator(self)
        return self
    }
}

let tree = TypedNode().withProperty { "\($0)" }
