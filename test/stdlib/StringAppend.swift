// RUN: %target-run-simple-swift | FileCheck %s

func StringAppend() {
  var str : String
  str = "So" + "me"
  var str2 : String
  var ch: Character = "e"
  str2 = "t" + ch + "xt"
  var str3 : String
  str3 = "¡" + str + " " + str2 + "!"
  print(str3)
}

StringAppend()
println()  // FIXME: iOS simulator needs newline

// CHECK: ¡Some text!
