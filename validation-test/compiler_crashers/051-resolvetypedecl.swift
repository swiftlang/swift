// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/rnapier (Rob Napier)
// RUN: not %swift %s -emit-ir
func b(c) -> <d>(() -> d) {
}
