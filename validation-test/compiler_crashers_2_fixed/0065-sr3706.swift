// RUN: %target-swift-frontend %s -emit-ir

let occurrences: [String] = []
let results: [(String, Int)] = occurrences.flatMap({ (match: String) -> (String, Int) in
	return ("", 0)
})
