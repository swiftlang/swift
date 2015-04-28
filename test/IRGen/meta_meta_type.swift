// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s

protocol Proto {
}

struct Mystruct : Proto {
}

func testit(p: Proto) -> Proto.Type.Type {
  return p.dynamicType.dynamicType
}

func testit2(p: Proto) -> Proto.Type.Type.Type {
  return p.dynamicType.dynamicType.dynamicType
}

var tt = testit(Mystruct())
var tt2 = testit2(Mystruct())

// CHECK: a.Mystruct.Type
println(tt)

// CHECK: a.Mystruct.Type.Type
println(tt2)
