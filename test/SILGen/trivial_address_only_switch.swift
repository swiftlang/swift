// RUN: %target-swift-emit-silgen -verify -disable-availability-checking %s

public enum StreamYieldResult<let count: Int>: Sendable {
    case literal(buffer: InlineArray<count, UInt8>)
    case end(buffer: InlineArray<count, UInt8>, endIndex: Int)
    
    public func buffer() -> InlineArray<count, UInt8> {
        switch self {
        case .literal(let b):
            return b
        case .end(let b, _):
            return b
        }
    }
}
