// RUN: %target-swift-frontend %s -emit-ir

// https://github.com/apple/swift/issues/46291

let occurrences: [String] = []
let results: [(String, Int)] = occurrences.flatMap({ (match: String) -> (String, Int) in
	return ("", 0)
})
