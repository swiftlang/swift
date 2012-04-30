// RUN: %swift -I %S/.. %s -i | FileCheck %s

// CHECK: Count: 1028


var NUM = 170

// FIXME: Array of bool someday.
var flags = new Int[8193] 
var count = 0


// This is the most ridiculous sieve ever.

foreach x in 0 .. NUM {
  count = 0
  foreach i in 2 .. 8192+1 {
    flags[i] = 1
  }
  
  foreach i in 2 .. 8192+1 {
    if (flags[i] == 1) {
      /* remove all multiples of prime: i */
      var k = i+i
      for (; k <= 8192; k = k + i) {
        flags[k] = 0;
      }
      ++count
    }
  }
}

println("Count: " + String(count))