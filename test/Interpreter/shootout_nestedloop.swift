// RUN: %swift -I %S/.. %s -i | FileCheck %s

// CHECK: 1771561

var x = 0
var n = 11

foreach a in 0 .. n {
  println(a)
  foreach b in 0 .. n { 
    foreach c in 0 .. n { 
      foreach d in 0 .. n { 
        foreach e in 0 .. n { 
          foreach f in 0 .. n {
            ++x
          }
        }
      }
    }
  }
}

println(x)
