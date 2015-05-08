// RUN: %target-run-simple-swift | FileCheck %s

func test(s: String)
{
  print(s)
  var s2 = s[advance(s.startIndex, 2)..<advance(s.startIndex, 4)]
  print(s2)
  var s3 = s2[s2.startIndex..<s2.startIndex]
  var s4 = s3[s2.startIndex..<s2.startIndex]
}

test("some text")

// CHECK: some text
// CHECK: me
