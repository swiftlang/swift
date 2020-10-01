// REQUIRES: VENDOR=apple
// RUN: %target-swift-frontend -emit-ir -o/dev/null -O -module-name test -validate-tbd-against-ir=missing %s

@_specialize(exported: true, where T: _Trivial)
public func foo<T>(_ x : T) -> T {
  return x
}
