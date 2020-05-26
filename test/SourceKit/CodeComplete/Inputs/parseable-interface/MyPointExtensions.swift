import MyPoint

public extension MyPoint {
	var magnitude: Double {
		return magnitudeSquared.squareRoot()
	}
}
