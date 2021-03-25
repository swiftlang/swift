@_exported import CircularLibrary

extension MyWholeNumber: Strideable {
	public typealias Stride = RawValue.Stride

	public func advanced(by n: Stride) -> MyWholeNumber {
		return MyWholeNumber(rawValue: rawValue.advanced(by: n))!
	}

	public func distance(to other: MyWholeNumber) -> Stride {
		return rawValue.distance(to: other.rawValue)
	}
}
