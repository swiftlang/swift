// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -cxx-interoperability-mode=default)
//
// REQUIRES: executable_test
import InheritedLookup
import StdlibUnittest

struct Foo {
  let bar: C3
}

func c3(x: C3) {
  _ = x.GetPrim()
}

runAllTests()
