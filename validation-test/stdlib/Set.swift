// RUN: %target-build-swift -Xfrontend -disable-access-control -module-name a %s -o %t.out -O
// RUN: %target-run %t.out

// <rdar://19331717> Optimizer breaks Set<T> and Dictionary<K, V>
func f() {
  var s: Set<Int> = [ 10 ]
  let index = s.indexOf(10)!
  s.removeAtIndex(index)
}
f()
