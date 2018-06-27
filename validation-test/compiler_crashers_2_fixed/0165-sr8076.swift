// RUN: %target-swift-frontend %s -emit-ir

import Foundation

public struct LargeValue {
    public var m1: String
    public var m2: String
    public var m3: String
}

public typealias Filter = (LargeValue) -> Bool

public protocol Query {
    associatedtype Returned
}

public protocol ProtoQueryHandler {
    func forceHandle<Q: Query>(query: Q) throws -> (Q.Returned, Filter?)
}

public protocol QueryHandler: ProtoQueryHandler {
    associatedtype Handled: Query
    func handle(query: Handled) throws -> (Handled.Returned, Filter?)
}

public extension QueryHandler {
    func forceHandle<Q: Query>(query: Q) throws -> (Q.Returned, Filter?) {
        guard let body = handle as? (Q) throws -> (Q.Returned, Filter?) else {
            fatalError("handler \(self) is expected to handle query \(query)")
        }
        return try body(query)
    }
}
