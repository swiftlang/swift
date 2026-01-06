// RUN: %target-swift-frontend -emit-ir %s

public func withOptionalsAsPointers<T, each Opt>(
	_ optional: repeat Optional<each Opt>,
	body: (repeat UnsafePointer<each Opt>?) throws -> T
) rethrows -> T {
	return try body(repeat (each optional).map { withUnsafePointer(to: $0) { $0 } })
}
