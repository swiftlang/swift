// RUN: %swift %s -i | FileCheck %s

func TestSliceUInt8() {
  var a = new UInt8[4]
  a[0] = 65
  a[1] = 66
  a[2] = 67
  a[3] = 68
  for i in a {
    var j = Int(i)
    print(j) print(' ')
  }
}

TestSliceUInt8()

// CHECK: 65 66 67 68 
