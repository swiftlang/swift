// RUN: %target-typecheck-verify-swift -solver-scope-threshold=300

enum DebugValue {
	case TupleValue([(name: String?, value: DebugValue)])

	var stringRepresentation: String {
		switch self {
		case .TupleValue(let values):
			return "(\(values.map { ($0.name.flatMap { "\($0): " } ?? "") + $0.value.stringRepresentation }.joined(separator: ", ")))"
		}
	}
}

extension DebugValue : Equatable { }

func == (lhs: DebugValue, rhs: DebugValue) -> Bool {
	switch (lhs, rhs) {
	case (DebugValue.TupleValue(let xs), DebugValue.TupleValue(let ys)) where xs.count == ys.count:
		return zip(xs, ys).reduce(true) { $0 ? $1.0.name == $1.1.name && $1.0.value == $1.1.value : false }
	default:
		return false
	}
}
