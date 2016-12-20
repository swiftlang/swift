func foo() -> Int{
  var aaa = 1 + 2
  aaa = aaa + 3
  if aaa == 3 { aaa = 4 }
  return aaa
}

func foo1() -> Int { return 0 }
class C { func foo() {} }
struct S { func foo() {} }

func foo2() {
  let a = 3
  var b = a.bigEndian
  let c = a.byteSwapped
  b = b.bigEndian.bigEndian.byteSwapped
  print(b + c)
}

struct S1 {
  var a = 3
  func foo() -> Int { return 0 }
  mutating func increment() -> S1 {
    a = a + 1
    return self
  }
}

func foo3(s: inout S1) -> Int {
  let b = s.a
  let c = s.foo() + b
  s = s.increment()
  return c + b
}

func foo4(s: S1) -> Int {
  let b = s.a
  let c = s.foo() + b
  return c + b
}

// RUN: %target-swift-ide-test -range -pos=8:1 -end-pos 8:32 -source-filename %s | %FileCheck %s -check-prefix=CHECK1
// RUN: %target-swift-ide-test -range -pos=9:1 -end-pos 9:26 -source-filename %s | %FileCheck %s -check-prefix=CHECK2
// RUN: %target-swift-ide-test -range -pos=10:1 -end-pos 10:27 -source-filename %s | %FileCheck %s -check-prefix=CHECK3
// RUN: %target-swift-ide-test -range -pos=3:1 -end-pos=4:26 -source-filename %s | %FileCheck %s -check-prefix=CHECK4
// RUN: %target-swift-ide-test -range -pos=3:1 -end-pos=5:13 -source-filename %s | %FileCheck %s -check-prefix=CHECK5
// RUN: %target-swift-ide-test -range -pos=4:1 -end-pos=5:13 -source-filename %s | %FileCheck %s -check-prefix=CHECK6
// RUN: %target-swift-ide-test -range -pos=14:1 -end-pos=17:15 -source-filename %s | %FileCheck %s -check-prefix=CHECK7
// RUN: %target-swift-ide-test -range -pos=31:1 -end-pos=33:15 -source-filename %s | %FileCheck %s -check-prefix=CHECK8
// RUN: %target-swift-ide-test -range -pos=37:1 -end-pos=39:15 -source-filename %s | %FileCheck %s -check-prefix=CHECK9

// CHECK1: <Kind>SingleDecl</Kind>
// CHECK1-NEXT: <Content>func foo1() -> Int { return 0 }</Content>
// CHECK1-NEXT: <Declared>foo1</Declared>
// CHECK1-NEXT: <end>

// CHECK2: <Kind>SingleDecl</Kind>
// CHECK2-NEXT: <Content>class C { func foo() {} }</Content>
// CHECK2-NEXT: <Declared>C</Declared>
// CHECK2-NEXT: <end>

// CHECK3: <Kind>SingleDecl</Kind>
// CHECK3-NEXT: <Content>struct S { func foo() {} }</Content>
// CHECK3-NEXT: <Declared>S</Declared>
// CHECK3-NEXT: <end>

// CHECK4: <Kind>MultiStatement</Kind>
// CHECK4-NEXT: <Content>aaa = aaa + 3
// CHECK4-NEXT: if aaa == 3 { aaa = 4 }</Content>
// CHECK4-NEXT: <Referenced>aaa</Referenced><Type>@lvalue Int</Type>
// CHECK4-NEXT: <end>

// CHECK5: <Kind>MultiStatement</Kind>
// CHECK5-NEXT: <Content>aaa = aaa + 3
// CHECK5-NEXT: if aaa == 3 { aaa = 4 }
// CHECK5-NEXT: return aaa</Content>
// CHECK5-NEXT: <Referenced>aaa</Referenced><Type>@lvalue Int</Type>
// CHECK5-NEXT: <end>

// CHECK6: <Kind>MultiStatement</Kind>
// CHECK6-NEXT: if aaa == 3 { aaa = 4 }
// CHECK6-NEXT: return aaa</Content>
// CHECK6-NEXT: <Referenced>aaa</Referenced><Type>@lvalue Int</Type>
// CHECK6-NEXT: <end>

// CHECK7: <Kind>MultiStatement</Kind>
// CHECK7-NEXT: <Content>var b = a.bigEndian
// CHECK7-NEXT:   let c = a.byteSwapped
// CHECK7-NEXT:   b = b.bigEndian.bigEndian.byteSwapped
// CHECK7-NEXT:   print(b + c)</Content>
// CHECK7-NEXT: <Declared>b</Declared>
// CHECK7-NEXT: <Declared>c</Declared>
// CHECK7-NEXT: <Referenced>a</Referenced><Type>Int</Type>
// CHECK7-NEXT: <Referenced>b</Referenced><Type>@lvalue Int</Type>
// CHECK7-NEXT: <end>

// CHECK8: <Kind>MultiStatement</Kind>
// CHECK8-NEXT: <Content>let c = s.foo() + b
// CHECK8-NEXT:   s = s.increment()
// CHECK8-NEXT:   return c + b</Content>
// CHECK8-NEXT: <Declared>c</Declared>
// CHECK8-NEXT: <Referenced>s</Referenced><Type>@lvalue S1</Type>
// CHECK8-NEXT: <Referenced>foo</Referenced><Type>(S1) -> () -> Int</Type>
// CHECK8-NEXT: <Referenced>b</Referenced><Type>Int</Type>
// CHECK8-NEXT: <Referenced>increment</Referenced><Type>(inout S1) -> () -> S1</Type>
// CHECK8-NEXT: <end>

// CHECK9: <Kind>MultiStatement</Kind>
// CHECK9-NEXT: <Content>let b = s.a
// CHECK9-NEXT:   let c = s.foo() + b
// CHECK9-NEXT:   return c + b</Content>
// CHECK9-NEXT: <Declared>b</Declared>
// CHECK9-NEXT: <Declared>c</Declared>
// CHECK9-NEXT: <Referenced>s</Referenced><Type>S1</Type>
// CHECK9-NEXT: <Referenced>a</Referenced><Type>Int</Type>
// CHECK9-NEXT: <Referenced>foo</Referenced><Type>(S1) -> () -> Int</Type>
// CHECK9-NEXT: <Referenced>b</Referenced><Type>Int</Type>
// CHECK9-NEXT: <end>
