// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -parse -verify -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s
// RUN: ls -lR %t/clang-module-cache | grep cfuncs.pcm
import cfuncs

func test_cfunc1(i : Int) {
  cfunc1() // okay
  cfunc1(i) // expected-error{{invalid conversion from type 'Int' to '()'}} expected-note{{while converting}}
}

func test_cfunc2(i : Int) {
  var f = cfunc2(i, 17)
  var f2 : Float = f
  cfunc2(b=17, a=i)
}

func test_cfunc3() {
  // FIXME: Breaks with an explicit closure, but I'm not sure why.
  var b = cfunc3( func(a : Double, b : Double) -> Double { return a + b } )
  var d : Double = b(1.5, 2.5)
  var d1 : Double = b // expected-error{{invalid conversion from type 'double_bin_op_block' to 'Double'}} expected-note{{while converting}}
}
