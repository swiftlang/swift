// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -O -emit-sil -primary-file %s | %FileCheck %s

// We can't deserialize apply_inst with subst lists. When radar://14443304
// is fixed then we should convert this test to a SIL test.

protocol Pingable {
  func ping()
}

class ABC : Pingable {
  func ping() {}
}

func generic_call<T : Pingable>(_ x: T) {
  x.ping()
}

// Make sure we can devirtualize and inline the virtual call to ping.
//CHECK: @_T023devirt_archetype_method21interesting_code_hereyyF
//CHECK-NOT: apply
//CHECK: return
func interesting_code_here() {
  _ = ABC()
  // Make sure that we can specialize the function generic_call that has a
  // generic call to x.ping().
  generic_call(ABC())
}



// Devirtualize generic archetype_methods with subst list.
protocol TPingable {
  func ping<T>(_ x : T)
}

class Foo : TPingable {
  func ping<T>(_ x : T) {}
}

func aMethod<T : TPingable>(_ x : T) {
  x.ping(2)
}

// Make sure that we devirtualizer, specialize and inline the call to aMethod
// and that everything is optimized away.
//CHECK: _T023devirt_archetype_method4mainyyF
//CHECK-NOT: apply
//CHECK: return
func main() {
  let x = Foo()
  aMethod(x)
}

