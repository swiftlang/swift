public struct MyPoint {
	public let x: Double
	public let y: Double

	public init(x: Double, y: Double) {
		self.x = x
		self.y = y
	}

	public var magnitudeSquared: Double {
		return x*x + y*y
	}
}
