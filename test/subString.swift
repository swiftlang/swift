// RUN: %swift %s -i | FileCheck %s

func test(s : String)
{
    println(s)
    var s2 = s[2..4]
    println(s2)
}

test("some text")

// CHECK: some text
// CHECK: me
