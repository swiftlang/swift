// RUN: not %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop

import Foundation

protocol AttributedStringKey {
    associatedtype Value : Hashable
}

protocol AttributedStringProtocol {
    subscript<K>(_: K.Type) -> K.Value? where K : AttributedStringKey { get set }
    subscript<R>(bounds: R) -> AttributedSubstring where R : RangeExpression, R.Bound == AttributedString.Index { get }
}

public struct AttributedSubstring : Sendable {
}

extension AttributedSubstring : AttributedStringProtocol {
}
