// RUN: %target-swift-frontend -emit-ir %s

public protocol HorseSaddle {}
public enum EnglishSaddle : HorseSaddle {}

public enum WesternSaddle<A, B> : HorseSaddle {}

public protocol Horse {
    associatedtype Body : Horse

    associatedtype Saddle: HorseSaddle

    var body: Body { get }
}

extension Horse {
    typealias Saddle = Body.Saddle
}

public struct DraftHorse<T> : Pony {
    public typealias Saddle = EnglishSaddle
    public typealias Body = Never
    var contents: T
}

// MARK: - Implementation detail

extension Never : Horse {
    public typealias Saddle = EnglishSaddle
    public typealias Body = Never

    public var body: Never {
        switch self {}
    }
}

protocol Pony : Horse where Body == Never {}
extension Pony {
    public var body: Never { fatalError() }
}

