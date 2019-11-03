// RUN: %target-swift-frontend -emit-sil %s -parse-as-library -o /dev/null -verify
// RUN: %target-swift-frontend -emit-sil %s -parse-as-library -o /dev/null -verify -enable-ownership-stripping-after-serialization

var gg: Bool = false
var rg: Int = 0

func f1() { }
func f2() { }

// The old implementation of the LifetimeChecker in DefiniteInitialization had
// an exponential computation complexity in some cases.
// This test should finish in almost no time. With the old implementation it
// took about 8 minutes.


func testit() {

  var tp: (a: Int, b: Int, c: Int) // expected-note {{variable defined here}}
  tp.a = 1

  while gg {

    if gg {
      rg = tp.a
      rg = tp.b // expected-error {{variable 'tp.b' used before being initialized}}
      tp.c = 27
    }

    // Create some control flow.
    // With the old implementation each line doubles the computation time.
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
    if gg { f1() } else { f2() }
  }
}
