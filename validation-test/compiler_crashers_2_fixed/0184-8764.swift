// RUN: %target-swift-frontend -emit-ir %s

@discardableResult public func applyWrapped<T, U>(function: Optional<(T) -> U>, to value: Optional<T>) -> Optional<U> {
    switch (function, value) {
    case (let .some(f), let .some(v)):
        return .some(f(v))
    case (.none, _):
        return .none
    case (_, .none):
        return .none
    }
}
