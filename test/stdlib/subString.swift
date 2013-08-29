// RUN: %swift %s -i | FileCheck %s

func test(s : String)
{
    println(s)
    var s2 = s[2..4]
    println(s2)
    var s3 = s2[0..0]
    var s4 = s3[0..0]
}

test("some text")

// CHECK: some text
// CHECK: me
