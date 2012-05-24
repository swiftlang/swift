// RUN: %swift %s -i | FileCheck %s


//===----------------------------------------------------------------------===//
// String Splits
//===----------------------------------------------------------------------===//
{
  var (before, after, found) = "foo.bar".splitFirst('.')
  assert(found)
  assert(before == "foo")
  assert(after == "bar")
}

// CHECK: OKAY
print("OKAY")
