// RUN: %target-typecheck-verify-swift

// SR-8767: a number of related problems with unqualified lookup of
// associated type names.


// #1
public protocol PA {
    associatedtype F
}

public protocol PDA : PA {
}

public protocol PrB {
    associatedtype F
}

extension PDA where Self : PrB {
    public init(first: F?) {
        fatalError()
    }
}

// #2
public protocol S { associatedtype F }
public protocol AM : S {}
public protocol AL { associatedtype F }
extension AM where Self : AL {
    public init(first: F?) { fatalError() }
}

// #3
public protocol S2 { associatedtype F }
public protocol A2 : S2 {}
public protocol Z2 { associatedtype F }
extension A2 where Self : Z2 {
    public init(first: F?) { fatalError() }
}

// #4
public protocol BM { associatedtype F }
public protocol C : BM {}
public protocol BL { associatedtype F }
extension C where Self : BL { public init(first: F?) { fatalError() } }

// #5
public protocol AZ { associatedtype F }
public protocol ZA : AZ {}
public protocol AA { associatedtype F }
extension ZA where Self : AA { public init(first: F?) { fatalError() } }

// #6
public protocol AZ2 { associatedtype F }
public protocol ZA2 : AZ2 {}
public protocol ZZ2 { associatedtype F }
extension ZA2 where Self : ZZ2 { public init(first: F?) { fatalError() } }

// #7
public protocol ZA3 { associatedtype F }
public protocol AZ3 : ZA3 {}
public protocol ZZ3 { associatedtype F }
extension AZ3 where Self : ZZ3 { public init(first: F?) { fatalError() } }

