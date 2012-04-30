// RUN: %swift -I %S/.. %s -i | FileCheck %s

// CHECK: 0.670153


// TODO: static variable in genRandom.
var lastRandom = 42

func genRandom(max : Double) -> Double {
  /*const*/ var IM = 139968
  /*const*/ var IA = 3877
  /*const*/ var IC = 29573
  
  lastRandom = ((lastRandom*IA+IC) % IM)
  
  return max * Double(lastRandom) / Double(IM)
}

var N = 400000

foreach i in 0 .. N-1 {
  genRandom(100.0)
}

println(genRandom(100.0))

