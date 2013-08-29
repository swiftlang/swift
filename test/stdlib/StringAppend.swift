// RUN: %swift %s -i | FileCheck %s

func StringAppend() {
  var str : String
  str = "So" + "me"
  var str2 : String
  str2 = 't' + 'e' + "xt"
  var str3 : String
  str3 = '¡' + str + ' ' + str2 + '!'
  print(str3)
}

StringAppend()

// CHECK: ¡Some text!
