// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/rnapier (Rob Napier)

func b(c) -> <d>(() -> d) {
}
