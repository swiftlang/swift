// RUN: %target-swift-emit-silgen -verify -target %target-swift-6.2-abi-triple %s
// RUN: %target-swift-emit-silgen-ossa -enable-sil-opaque-values -verify -target %target-swift-6.2-abi-triple %s
// RUN: %target-swift-emit-sil -enable-sil-opaque-values -sil-verify-all -target %target-swift-6.2-abi-triple %s

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
