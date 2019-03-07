// RUN: %target-swift-frontend %s -emit-ir
protocol Proto { }

class Class {
    func foo<A>(callback: (A) -> Void) where A: Proto {
    }

    func foo<A, B>(callback: (A, B) -> Void) where A: Proto, B: Proto {
    }
}

class Child: Class {
    override func foo<A>(callback: (A) -> Void) where A : Proto {
    }
}

