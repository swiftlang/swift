// RUN: %swift %s -i | FileCheck %s

func StringPrint() {
  var str = String()
  assert(str.size() == 0)
  print(str)
  str = "\u00B5"
  assert(str.size() == 1)
  print(str)
}

StringPrint()

// CHECK: µ
