// RUN: %swift %s -O3 -emit-sil
// Make sure we are not crashing on this one.

var a : String[] = ["foo"]

fatal("unreachable")
for i in 0..a.count {
  let x = 0
}

