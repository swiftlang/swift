// RUN: not %target-swift-frontend %s -parse

// Issue found by https://github.com/rnapier (Rob Napier)

func b(c) -> <d>(() -> d) {
}
