// RUN: %swift %s -i | FileCheck %s

func StringAppend() {
  var str : String
  str = "Some"
  var str2 : String
  str2 = "text"
  var str3 : String
  str3 = str + " " + str2
  print(str3);
}

StringAppend()

// CHECK: Some text
