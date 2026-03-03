// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100

class Something {
	var placeName: String? = "Home"
	var streetAddress: String?  = "123 Home Street"
	var cityTown: String?  = "New Hometown"
	var state: String?  = "HO"
	var zipCode: String?  = "12345"
	var coordinate = (latitude: 40.5, longitude: 40.5)

	var description: String {
		return "-----------------------------\n" +
			"Place Name: \(placeName ?? "")\n" +
			"Street Address: \(streetAddress ?? "")\n" +
			"City: \(cityTown ?? "")\n" +
			"State: \(state ?? "")\n" +
			"Zip Code: \(zipCode ?? "")\n" +
			"Coordinate: latitude: \(coordinate.latitude) longitude \(coordinate.longitude)\n" +
		"-----------------------------\n"
	}
}
