// RUN: %target-run-simple-swift | FileCheck %s

func StringPrint() {
  var str = String()
  assert(str.isEmpty)
  print(str)
  str = "\u00B5"
  assert(str.core.count == 1)
  print(str)
}

StringPrint()
println()  // FIXME: iOS simulator needs newline

// CHECK: µ
