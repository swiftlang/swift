enum AnEnum {
	case /*last-enum-element:def*/lastOccurrence(a: Int)
}

// RUN: %refactor -syntactic-rename -source-filename %s -pos="last-enum-element" -is-function-like -old-name "lastOccurrence(a:)" -new-name "lastOccurrence(b:)"
// REQUIRES asserts
