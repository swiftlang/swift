// RUN: %target-swift-frontend %s -typecheck -enable-experimental-cxx-interop -Xcc -std=c++17

#if canImport(Foundation)
// Foundation depends on C++ standard library
// on some platforms already, and as such
// may bring math functions from it.
import Foundation

func test() -> Float {
  let x: Float = 1.0
  // Note: we mix 'Float' and 'Double' (literal)
  // intentionally, as this might trigger Swift
  // to instantiate a function template from
  // the C++ standard library.
  let z = pow(x, 2.0)
  let _ = abs(x)
  let m = z + 1.0
  return m
}
#endif
