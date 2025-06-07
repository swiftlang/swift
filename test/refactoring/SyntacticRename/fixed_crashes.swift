enum AnEnum {
	case /*last-enum-element:def*/lastOccurrence(a: Int)
}

// REQUIRES: swift_swift_parser
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="last-enum-element" -is-function-like -old-name "lastOccurrence(a:)" -new-name "lastOccurrence(b:)"
// REQUIRES asserts
