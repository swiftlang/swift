// RUN: %target-swift-frontend -emit-ir -target %target-swift-5.9-abi-triple %s

protocol P {
    var p: Int { get }
}

protocol Q {
    func p(content: Int) -> Int
}

struct Zero: P {
    var p: Int { 0 }
}

struct One: P {
    var p: Int { 1 }
}

struct Add: Q {
    let rhs: Int

    func p(content: Int) -> Int {
        content + rhs
    }
}

struct Multiply: Q {
    let rhs: Int

    func p(content: Int) -> Int {
        content * rhs
    }
}

struct G<Content: P, each Modifier: Q>: P {
    var content: Content
    var modifiers: (repeat each Modifier)

    init(content: Content, modifiers: (repeat each Modifier)) {
        self.content = content
        self.modifiers = modifiers
    }

    var p: Int {
        var r = content.p
        for m in repeat each modifiers {
            r = m.p(content: r)
        }
        return r
    }
}

extension G: Equatable where Content: Equatable,
    repeat each Modifier: Equatable
{
    static func ==(lhs: Self, rhs: Self) -> Bool {
        guard lhs.content == rhs.content else { return false}
        for (left, right) in repeat (each lhs.modifiers, each rhs.modifiers) {
            guard left == right else { return false }
        }
        return true
    }
}


extension G {
    func modifier<T>(_ modifier: T) -> G<Content, repeat each Modifier, T> {
        .init(content: content, modifiers: (repeat each modifiers, modifier))
    }

    func add(_ rhs: Int) -> G<Content, repeat each Modifier, some Q> {
        modifier(Add(rhs: rhs))
    }

    func multiply(_ rhs: Int) -> G<Content, repeat each Modifier, some Q> {
        modifier(Multiply(rhs: rhs))
    }
}

extension P {
    func modifier<T>(_ modifier: T) -> G<Self, T> {
        return G(content: self, modifiers: modifier)
    }

    func add(_ rhs: Int) -> G<Self, some Q> {
        modifier(Add(rhs: rhs))
    }

    func multiply(_ rhs: Int) -> G<Self, some Q> {
        modifier(Multiply(rhs: rhs))
    }
}

public func test() {
    let r = Zero()
        .multiply(1)
        .multiply(2)
        .add(3)
        .multiply(4)
        .add(2)
        .multiply(6)
        .add(2)
        .multiply(6)
        .add(2)
        .multiply(6)

    print(type(of: r))
}

test()
