// RUN: %swift -I %S/.. %s -i | FileCheck %s

// CHECK: 0.999707


// TODO: static variable in genRandom.
var lastRandom = 42

func genRandom(max : Double) -> Double {
  /*const*/ var IM = 139968
  /*const*/ var IA = 3877
  /*const*/ var IC = 29573
  
  lastRandom = ((lastRandom*IA+IC) % IM)
  
  return max * Double(lastRandom) / Double(IM)
}


func heapsort(n : Int, ra : Double[]) {
  var l = (n >> 1) + 1
  var ir = n

  while (true) {
    var rra : Double
    if (l > 1) {
      --l
      rra = ra[l]
    } else {
      rra = ra[ir]
      ra[ir] = ra[1]
      ;--ir
      if (ir == 1) {
        ra[1] = rra;
        return
      }
    }

    var i = l
    var j = l << 1
    while (j <= ir) {
      if (j < ir && ra[j] < ra[j+1]) { 
        ++j
      }
      if (rra < ra[j]) {
        ra[i] = ra[j]
        i = j
        j = j + j
      } else {
        j = ir + 1
      }
    }
    ra[i] = rra
  }
}


var N = 8000

// create an array of N+1 random doubles
var ary = new Double[N+1]
foreach i in 1 .. N+1 {
  ary[i] = genRandom(1.0)
}

heapsort(N, ary)

println(ary[N])

