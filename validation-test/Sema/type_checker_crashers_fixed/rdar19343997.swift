// RUN: not %target-swift-frontend %s -typecheck

func ignore<T>(_ parser: (String) -> (T, String)?) -> (String) -> ((), String)? {
	return { parser($0).map { _ in () } }
}
func | <T> (left: (String) -> (T, String)?, right: (String) -> ((), String)?) -> (String) -> (T?, String)? {
	return { _ in nil }
}

ignore(" " | "\r" | "\t" | "\n")

// Related: rdar://problem/19924870
func unit<T>(_ x: T) -> T? { return x }
func f() -> Int? {
	return unit(1) ?? unit(2).map { 1 } ?? nil
}
