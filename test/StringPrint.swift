// RUN: %swift %s -i | FileCheck %s

func StringPrint() {
  var str : String
  str = "\u00C2\u00B5"
  print(str);
}

StringPrint()

// CHECK: µ
