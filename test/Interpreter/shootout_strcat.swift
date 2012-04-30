// RUN: %swift -I %S/.. %s -i | FileCheck %s

// CHECK: FIXME: rdar://11344937


var NUM = 10

var str : String

foreach i in 0 .. NUM {
  //str = str + "hello\n"
}

//println(str.size())

println("FIXME: rdar://11344937")
