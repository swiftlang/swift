// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-emit-silgen -verify %s

// https://github.com/apple/swift/issues/71608
func f(x:[Int]?)
{
}
func g()
{
    let x:[Int]? = nil
    f(x: consume x)
}
