public struct Empty: Error {}

public struct OneWord: Error {
    public let x = 0
}

public struct Impl: P {
    public init() {}

    public func f0(_ b: Bool) throws(Empty) {
        guard b else {
            throw Empty()
        }
    }

    public func f1(_ b: Bool) throws(Empty) -> Int {
        guard b else {
            throw Empty()
        }
        return 1
    }

    public func f2(_ b: Bool) throws(Empty) -> (Int, Int) {
        guard b else {
            throw Empty()
        }
        return (1, 2)
    }

    public func f3(_ b: Bool) throws(Empty) -> (Int, Int, Int) {
        guard b else {
            throw Empty()
        }
        return (1, 2, 3)
    }

    public func f4(_ b: Bool) throws(Empty) -> (Int, Int, Int, Int) {
        guard b else {
            throw Empty()
        }
        return (1, 2, 3, 4)
    }

    public func f5(_ b: Bool) throws(Empty) -> (Int, Int, Int, Int, Int) {
        guard b else {
            throw Empty()
        }
        return (1, 2, 3, 4, 5)
    }

    public func g0(_ b: Bool) throws(OneWord) {
        guard b else {
            throw OneWord()
        }
    }

    public func g1(_ b: Bool) throws(OneWord) -> Int {
        guard b else {
            throw OneWord()
        }
        return 1
    }

    public func g2(_ b: Bool) throws(OneWord) -> (Int, Int) {
        guard b else {
            throw OneWord()
        }
        return (1, 2)
    }

    public func g3(_ b: Bool) throws(OneWord) -> (Int, Int, Int) {
        guard b else {
            throw OneWord()
        }
        return (1, 2, 3)
    }

    public func g4(_ b: Bool) throws(OneWord) -> (Int, Int, Int, Int) {
        guard b else {
            throw OneWord()
        }
        return (1, 2, 3, 4)
    }

    public func g5(_ b: Bool) throws(OneWord) -> (Int, Int, Int, Int, Int) {
        guard b else {
            throw OneWord()
        }
        return (1, 2, 3, 4, 5)
    }

    public func nonMatching_f0(_ b: Bool) throws(OneWord) -> (Float, Float) {
        guard b else {
            throw OneWord()
        }
        return (1.0, 2.0)
    }

    public func nonMatching_f1(_ b: Bool) throws(OneWord) -> (Float, Bool, Float) {
        guard b else {
            throw OneWord()
        }
        return (1.0, true, 2.0)
    }
}

@available(SwiftStdlib 6.0, *)
public struct ImplAsync: PAsync {
    public init() {}

    public func f0(_ b: Bool) async throws(Empty) {
        guard b else {
            throw Empty()
        }
    }

    public func f1(_ b: Bool) async throws(Empty) -> Int {
        guard b else {
            throw Empty()
        }
        return 1
    }

    public func f2(_ b: Bool) async throws(Empty) -> (Int, Int) {
        guard b else {
            throw Empty()
        }
        return (1, 2)
    }

    public func f3(_ b: Bool) async throws(Empty) -> (Int, Int, Int) {
        guard b else {
            throw Empty()
        }
        return (1, 2, 3)
    }

    public func f4(_ b: Bool) async throws(Empty) -> (Int, Int, Int, Int) {
        guard b else {
            throw Empty()
        }
        return (1, 2, 3, 4)
    }

    public func f5(_ b: Bool) async throws(Empty) -> (Int, Int, Int, Int, Int) {
        guard b else {
            throw Empty()
        }
        return (1, 2, 3, 4, 5)
    }

    public func g0(_ b: Bool) async throws(OneWord) {
        guard b else {
            throw OneWord()
        }
    }

    public func g1(_ b: Bool) async throws(OneWord) -> Int {
        guard b else {
            throw OneWord()
        }
        return 1
    }

    public func g2(_ b: Bool) async throws(OneWord) -> (Int, Int) {
        guard b else {
            throw OneWord()
        }
        return (1, 2)
    }

    public func g3(_ b: Bool) async throws(OneWord) -> (Int, Int, Int) {
        guard b else {
            throw OneWord()
        }
        return (1, 2, 3)
    }

    public func g4(_ b: Bool) async throws(OneWord) -> (Int, Int, Int, Int) {
        guard b else {
            throw OneWord()
        }
        return (1, 2, 3, 4)
    }

    public func g5(_ b: Bool) async throws(OneWord) -> (Int, Int, Int, Int, Int) {
        guard b else {
            throw OneWord()
        }
        return (1, 2, 3, 4, 5)
    }

    public func nonMatching_f0(_ b: Bool) async throws(OneWord) -> (Float, Float) {
        guard b else {
            throw OneWord()
        }
        return (1.0, 2.0)
    }

    public func nonMatching_f1(_ b: Bool) async throws(OneWord) -> (Float, Bool, Float) {
        guard b else {
            throw OneWord()
        }
        return (1.0, true, 2.0)
    }
}

public protocol P {
    func f0(_ b: Bool) throws(Empty)
    func f1(_ b: Bool) throws(Empty) -> Int
    func f2(_ b: Bool) throws(Empty) -> (Int, Int)
    func f3(_ b: Bool) throws(Empty) -> (Int, Int, Int)
    func f4(_ b: Bool) throws(Empty) -> (Int, Int, Int, Int)
    func f5(_ b: Bool) throws(Empty) -> (Int, Int, Int, Int, Int)

    func g0(_ b: Bool) throws(OneWord)
    func g1(_ b: Bool) throws(OneWord) -> Int
    func g2(_ b: Bool) throws(OneWord) -> (Int, Int)
    func g3(_ b: Bool) throws(OneWord) -> (Int, Int, Int)
    func g4(_ b: Bool) throws(OneWord) -> (Int, Int, Int, Int)
    func g5(_ b: Bool) throws(OneWord) -> (Int, Int, Int, Int, Int)

    func nonMatching_f0(_ b: Bool) throws(OneWord) -> (Float, Float)
    func nonMatching_f1(_ b: Bool) throws(OneWord) -> (Float, Bool, Float)
}

@available(SwiftStdlib 6.0, *)
public protocol PAsync {
    func f0(_ b: Bool) async throws(Empty)
    func f1(_ b: Bool) async throws(Empty) -> Int
    func f2(_ b: Bool) async throws(Empty) -> (Int, Int)
    func f3(_ b: Bool) async throws(Empty) -> (Int, Int, Int)
    func f4(_ b: Bool) async throws(Empty) -> (Int, Int, Int, Int)
    func f5(_ b: Bool) async throws(Empty) -> (Int, Int, Int, Int, Int)

    func g0(_ b: Bool) async throws(OneWord)
    func g1(_ b: Bool) async throws(OneWord) -> Int
    func g2(_ b: Bool) async throws(OneWord) -> (Int, Int)
    func g3(_ b: Bool) async throws(OneWord) -> (Int, Int, Int)
    func g4(_ b: Bool) async throws(OneWord) -> (Int, Int, Int, Int)
    func g5(_ b: Bool) async throws(OneWord) -> (Int, Int, Int, Int, Int)

    func nonMatching_f0(_ b: Bool) async throws(OneWord) -> (Float, Float)
    func nonMatching_f1(_ b: Bool) async throws(OneWord) -> (Float, Bool, Float)
}
