public struct Empty: Error {}

public struct OneWord: Error {
    public let x = 0
}

public struct TwoWords: Error {
    public let x = 0
    public let y = 0
}

public struct ThreeWords: Error {
    public let x = 0
    public let y = 0
    public let z = 0
}

public struct ManyBytes {
    let x0: UInt8 = 0
    let x1: UInt8 = 1
    let x2: UInt8 = 2
    let x3: UInt8 = 3
    let x4: UInt8 = 4
    let x5: UInt8 = 5
    let x6: UInt8 = 6
    let x7: UInt8 = 7
    let x8: UInt8 = 8
    let x9: UInt8 = 9
    let x10: UInt8 = 10
    let x11: UInt8 = 11
    let x12: UInt8 = 12
    let x13: UInt8 = 13
    let x14: UInt8 = 14
    let x15: UInt8 = 15
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

    public func fManyBytes(_ b: Bool) throws(Empty) -> ManyBytes {
        guard b else {
            throw Empty()
        }
        return ManyBytes()
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

    public func h0(_ b: Bool) throws(TwoWords) {
        guard b else {
            throw TwoWords()
        }
    }

    public func h1(_ b: Bool) throws(TwoWords) -> Int {
        guard b else {
            throw TwoWords()
        }
        return 1
    }

    public func h2(_ b: Bool) throws(TwoWords) -> (Int, Int) {
        guard b else {
            throw TwoWords()
        }
        return (1, 2)
    }

    public func h3(_ b: Bool) throws(TwoWords) -> (Int, Int, Int) {
        guard b else {
            throw TwoWords()
        }
        return (1, 2, 3)
    }

    public func h4(_ b: Bool) throws(TwoWords) -> (Int, Int, Int, Int) {
        guard b else {
            throw TwoWords()
        }
        return (1, 2, 3, 4)
    }

    public func h5(_ b: Bool) throws(TwoWords) -> (Int, Int, Int, Int, Int) {
        guard b else {
            throw TwoWords()
        }
        return (1, 2, 3, 4, 5)
    }

    public func i0(_ b: Bool) throws(ThreeWords) {
        guard b else {
            throw ThreeWords()
        }
    }

    public func i1(_ b: Bool) throws(ThreeWords) -> Int {
        guard b else {
            throw ThreeWords()
        }
        return 1
    }

    public func i2(_ b: Bool) throws(ThreeWords) -> (Int, Int) {
        guard b else {
            throw ThreeWords()
        }
        return (1, 2)
    }

    public func i3(_ b: Bool) throws(ThreeWords) -> (Int, Int, Int) {
        guard b else {
            throw ThreeWords()
        }
        return (1, 2, 3)
    }

    public func i4(_ b: Bool) throws(ThreeWords) -> (Int, Int, Int, Int) {
        guard b else {
            throw ThreeWords()
        }
        return (1, 2, 3, 4)
    }

    public func i5(_ b: Bool) throws(ThreeWords) -> (Int, Int, Int, Int, Int) {
        guard b else {
            throw ThreeWords()
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

    public func fManyBytes(_ b: Bool) async throws(Empty) -> ManyBytes {
        guard b else {
            throw Empty()
        }
        return ManyBytes()
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

    public func h0(_ b: Bool) async throws(TwoWords) {
        guard b else {
            throw TwoWords()
        }
    }

    public func h1(_ b: Bool) async throws(TwoWords) -> Int {
        guard b else {
            throw TwoWords()
        }
        return 1
    }

    public func h2(_ b: Bool) async throws(TwoWords) -> (Int, Int) {
        guard b else {
            throw TwoWords()
        }
        return (1, 2)
    }

    public func h3(_ b: Bool) async throws(TwoWords) -> (Int, Int, Int) {
        guard b else {
            throw TwoWords()
        }
        return (1, 2, 3)
    }

    public func h4(_ b: Bool) async throws(TwoWords) -> (Int, Int, Int, Int) {
        guard b else {
            throw TwoWords()
        }
        return (1, 2, 3, 4)
    }

    public func h5(_ b: Bool) async throws(TwoWords) -> (Int, Int, Int, Int, Int) {
        guard b else {
            throw TwoWords()
        }
        return (1, 2, 3, 4, 5)
    }

    public func i0(_ b: Bool) async throws(ThreeWords) {
        guard b else {
            throw ThreeWords()
        }
    }

    public func i1(_ b: Bool) async throws(ThreeWords) -> Int {
        guard b else {
            throw ThreeWords()
        }
        return 1
    }

    public func i2(_ b: Bool) async throws(ThreeWords) -> (Int, Int) {
        guard b else {
            throw ThreeWords()
        }
        return (1, 2)
    }

    public func i3(_ b: Bool) async throws(ThreeWords) -> (Int, Int, Int) {
        guard b else {
            throw ThreeWords()
        }
        return (1, 2, 3)
    }

    public func i4(_ b: Bool) async throws(ThreeWords) -> (Int, Int, Int, Int) {
        guard b else {
            throw ThreeWords()
        }
        return (1, 2, 3, 4)
    }

    public func i5(_ b: Bool) async throws(ThreeWords) -> (Int, Int, Int, Int, Int) {
        guard b else {
            throw ThreeWords()
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
    func fManyBytes(_ b: Bool) throws(Empty) -> ManyBytes

    func g0(_ b: Bool) throws(OneWord)
    func g1(_ b: Bool) throws(OneWord) -> Int
    func g2(_ b: Bool) throws(OneWord) -> (Int, Int)
    func g3(_ b: Bool) throws(OneWord) -> (Int, Int, Int)
    func g4(_ b: Bool) throws(OneWord) -> (Int, Int, Int, Int)
    func g5(_ b: Bool) throws(OneWord) -> (Int, Int, Int, Int, Int)

    func h0(_ b: Bool) throws(TwoWords)
    func h1(_ b: Bool) throws(TwoWords) -> Int
    func h2(_ b: Bool) throws(TwoWords) -> (Int, Int)
    func h3(_ b: Bool) throws(TwoWords) -> (Int, Int, Int)
    func h4(_ b: Bool) throws(TwoWords) -> (Int, Int, Int, Int)
    func h5(_ b: Bool) throws(TwoWords) -> (Int, Int, Int, Int, Int)

    func i0(_ b: Bool) throws(ThreeWords)
    func i1(_ b: Bool) throws(ThreeWords) -> Int
    func i2(_ b: Bool) throws(ThreeWords) -> (Int, Int)
    func i3(_ b: Bool) throws(ThreeWords) -> (Int, Int, Int)
    func i4(_ b: Bool) throws(ThreeWords) -> (Int, Int, Int, Int)
    func i5(_ b: Bool) throws(ThreeWords) -> (Int, Int, Int, Int, Int)

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
    func fManyBytes(_ b: Bool) async throws(Empty) -> ManyBytes

    func g0(_ b: Bool) async throws(OneWord)
    func g1(_ b: Bool) async throws(OneWord) -> Int
    func g2(_ b: Bool) async throws(OneWord) -> (Int, Int)
    func g3(_ b: Bool) async throws(OneWord) -> (Int, Int, Int)
    func g4(_ b: Bool) async throws(OneWord) -> (Int, Int, Int, Int)
    func g5(_ b: Bool) async throws(OneWord) -> (Int, Int, Int, Int, Int)

    func h0(_ b: Bool) async throws(TwoWords)
    func h1(_ b: Bool) async throws(TwoWords) -> Int
    func h2(_ b: Bool) async throws(TwoWords) -> (Int, Int)
    func h3(_ b: Bool) async throws(TwoWords) -> (Int, Int, Int)
    func h4(_ b: Bool) async throws(TwoWords) -> (Int, Int, Int, Int)
    func h5(_ b: Bool) async throws(TwoWords) -> (Int, Int, Int, Int, Int)

    func i0(_ b: Bool) async throws(ThreeWords)
    func i1(_ b: Bool) async throws(ThreeWords) -> Int
    func i2(_ b: Bool) async throws(ThreeWords) -> (Int, Int)
    func i3(_ b: Bool) async throws(ThreeWords) -> (Int, Int, Int)
    func i4(_ b: Bool) async throws(ThreeWords) -> (Int, Int, Int, Int)
    func i5(_ b: Bool) async throws(ThreeWords) -> (Int, Int, Int, Int, Int)

    func nonMatching_f0(_ b: Bool) async throws(OneWord) -> (Float, Float)
    func nonMatching_f1(_ b: Bool) async throws(OneWord) -> (Float, Bool, Float)
}
