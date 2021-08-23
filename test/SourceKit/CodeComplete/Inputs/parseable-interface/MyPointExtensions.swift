import MyPointModule

public extension MyPoint {
	var magnitude: Double {
		return magnitudeSquared.squareRoot()
	}
}
