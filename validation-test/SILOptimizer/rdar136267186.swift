// RUN: %target-swift-frontend -O -emit-sil -verify %s

public struct Foo {

	@Clamped(0...1)
	public var value: Double = 1.0
}

@propertyWrapper
public struct Clamped<WrappedValue: Numeric & Comparable> {

	public init(wrappedValue: WrappedValue, _ range: ClosedRange<WrappedValue>) {
		self.range = range
		self._wrappedValue = wrappedValue.clamped(to: range)
	}

	public let range: ClosedRange<WrappedValue>

	private var _wrappedValue: WrappedValue
	public var wrappedValue: WrappedValue {
		get { _wrappedValue }
		set { _wrappedValue = newValue.clamped(to: range) }
	}
}

public extension Comparable {

	func clamped(to range: ClosedRange<Self>, exceptions: Self...) -> Self { clamped(to: range, exceptions: exceptions) }
	func clamped(to range: ClosedRange<Self>, exceptions: any Collection<Self>) -> Self {

		return exceptions.contains(self)
			? self
			: max(range.lowerBound, min(range.upperBound, self))
	}

	mutating func clamp(to range: ClosedRange<Self>, exceptions: Self...) { clamp(to: range, exceptions: exceptions) }
	mutating func clamp(to range: ClosedRange<Self>, exceptions: any Collection<Self>) {
		self = self.clamped(to: range, exceptions: exceptions)
	}
}

