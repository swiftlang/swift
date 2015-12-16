// RUN: %target-swift-frontend -O -emit-sil -primary-file %s | FileCheck %s

// We can't deserialize apply_inst with subst lists. When radar://14443304
// is fixed then we should convert this test to a SIL test.

protocol Pingable {
  func ping()
}

class ABC : Pingable {
  func ping() {}
}

func generic_call<T : Pingable>(x: T) {
  x.ping()
}

// Make sure we can devirtualize and inline the virtual call to ping.
//CHECK: @_TF23devirt_archetype_method21interesting_code_hereFT_T_
//CHECK-NOT: apply
//CHECK: return
func interesting_code_here() {
  var x = ABC()
  // Make sure that we can specialize the function generic_call that has a
  // generic call to x.ping().
  generic_call(ABC())
}



// Devirtualize generic archetype_methods with subst list.
protocol TPingable {
  func ping<T>(x : T)
}

class Foo : TPingable {
  func ping<T>(x : T) {}
}

func aMethod<T : TPingable>(x : T) {
  x.ping(2)
}

// Make sure that we devirtualizer, specialize and inline the call to aMethod
// and that everything is optimized away.
//CHECK: _TF23devirt_archetype_method4mainFT_T_
//CHECK-NOT: apply
//CHECK: return
func main() {
  var x = Foo()
  aMethod(x)
}

