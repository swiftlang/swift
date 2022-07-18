// RUN: %target-swift-frontend -experimental-performance-annotations -experimental-skip-non-inlinable-function-bodies-without-types -emit-module %s -o /dev/null

// Don't crash when emitting a module

@_noAllocation
func foo() -> Int {
  return 42
}
print(foo())
