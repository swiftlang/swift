// RUN: %swift -I %S/.. %s -i | FileCheck %s

// CHECK: 1000 150000

var n = 150

var x = new Int[n];
var y = new Int[n];

foreach i in 0..n {
  x[i] = i+1
}

foreach k in 0..1000 {
  // FIXME: This really wants a negative step of (n-1 .. -1), something
  // like:  foreach i in (n-1).downTo(-1) { y[i] = y[i] + x[i] }
  foreach i in 0 .. n {
    var i2 = n-1-i
    y[i2] = y[i2] + x[i2];
  }
}

println(String(y[0]) + " " + String(y[n-1]))
