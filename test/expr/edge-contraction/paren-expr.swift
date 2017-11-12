// RUN: %target-typecheck-verify-swift

let x = [1,2,3]
func fun() -> Float {
	return 1 + (x.count == 1 ? 0 : 21)
}
