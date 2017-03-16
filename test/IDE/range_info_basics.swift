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

class C1 {
  func getC() -> C1 { return self }
  func take(another : C1) -> C1 {return another }
  let c = C1()
}

func foo5(c : C1) -> C1 {
  let a = c.c.getC().c.getC().getC().getC()
  let b = a.c.c.c.c.getC().getC()
  let d = a.c.getC().getC().c.c
  return a.take(another: b).take(another: d)
}

func foo6() -> Int {
  let a = { () -> Int in
    let a = 3
    var b = a.bigEndian
    let c = a.byteSwapped
    b = b.bigEndian.bigEndian.byteSwapped
    print(b + c)
    return { () -> Int in
      let a = 3
      var b = a.bigEndian
      let c = a.byteSwapped
      b = b.bigEndian.bigEndian.byteSwapped
      print(b + c)
      return 1
    }()
  }()
  return a
}

func foo7(a : Int) -> Int {
  switch a {
  case 1:
    return 0
  case 2:
    return 1
  default:
    return a
  }
}

func foo8(a : [Int]) {
  for v in a {
    if v > 3 {
      break
    }
    if v < 3 {
      continue
    }
  }
  var i : Int
  i = 0
  repeat {
    print(i)
    i = i + 1
    if i < 10 {
      continue
    }
    if i > 80 {
      break
    }
  } while i < 100
}

func foo9(_ a: Int, _ b: Int) -> Int {
  if a == b {
    return 0
  }
  switch a {
  case 1:
    foo9(1, 2)
    foo9(1, 2)
    return foo9(2, 4)
  default:
    foo9(1, 2)
    foo9(1, 2)
    return foo9(2, 4)
  }
  return 0
}

func testInout(_ a : inout Int) {
  var b = a + 1 + 1
  b = b + 1
  testInout(&b)
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
// RUN: %target-swift-ide-test -range -pos=49:1 -end-pos=50:34 -source-filename %s | %FileCheck %s -check-prefix=CHECK10
// RUN: %target-swift-ide-test -range -pos=49:1 -end-pos=51:32 -source-filename %s | %FileCheck %s -check-prefix=CHECK11
// RUN: %target-swift-ide-test -range -pos=49:1 -end-pos=52:45 -source-filename %s | %FileCheck %s -check-prefix=CHECK12
// RUN: %target-swift-ide-test -range -pos=57:1 -end-pos=61:17 -source-filename %s | %FileCheck %s -check-prefix=CHECK13
// RUN: %target-swift-ide-test -range -pos=57:1 -end-pos=69:8 -source-filename %s | %FileCheck %s -check-prefix=CHECK14
// RUN: %target-swift-ide-test -range -pos=63:1 -end-pos=66:44 -source-filename %s | %FileCheck %s -check-prefix=CHECK15
// RUN: %target-swift-ide-test -range -pos=63:1 -end-pos=68:15 -source-filename %s | %FileCheck %s -check-prefix=CHECK16
// RUN: %target-swift-ide-test -range -pos=67:1 -end-pos=67:19 -source-filename %s | %FileCheck %s -check-prefix=CHECK17
// RUN: %target-swift-ide-test -range -pos=76:1 -end-pos=79:13 -source-filename %s | %FileCheck %s -check-prefix=CHECK18
// RUN: %target-swift-ide-test -range -pos=76:1 -end-pos=77:13 -source-filename %s | %FileCheck %s -check-prefix=CHECK19
// RUN: %target-swift-ide-test -range -pos=78:1 -end-pos=81:13 -source-filename %s | %FileCheck %s -check-prefix=CHECK20
// RUN: %target-swift-ide-test -range -pos=87:1 -end-pos=89:6 -source-filename %s | %FileCheck %s -check-prefix=CHECK21
// RUN: %target-swift-ide-test -range -pos=90:1 -end-pos=92:6 -source-filename %s | %FileCheck %s -check-prefix=CHECK22
// RUN: %target-swift-ide-test -range -pos=99:1 -end-pos=101:6 -source-filename %s | %FileCheck %s -check-prefix=CHECK23
// RUN: %target-swift-ide-test -range -pos=102:1 -end-pos=104:6 -source-filename %s | %FileCheck %s -check-prefix=CHECK24
// RUN: %target-swift-ide-test -range -pos=87:1 -end-pos=92:6 -source-filename %s | %FileCheck %s -check-prefix=CHECK25
// RUN: %target-swift-ide-test -range -pos=97:1 -end-pos=104:6 -source-filename %s | %FileCheck %s -check-prefix=CHECK26
// RUN: %target-swift-ide-test -range -pos=109:6 -end-pos=111:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-INVALID
// RUN: %target-swift-ide-test -range -pos=114:1 -end-pos=115:15 -source-filename %s | %FileCheck %s -check-prefix=CHECK27
// RUN: %target-swift-ide-test -range -pos=118:1 -end-pos=119:15 -source-filename %s | %FileCheck %s -check-prefix=CHECK27
// RUN: %target-swift-ide-test -range -pos=126:11 -end-pos=126:12 -source-filename %s | %FileCheck %s -check-prefix=CHECK-INT
// RUN: %target-swift-ide-test -range -pos=126:11 -end-pos=126:20 -source-filename %s | %FileCheck %s -check-prefix=CHECK-INT
// RUN: %target-swift-ide-test -range -pos=127:7 -end-pos=127:8 -source-filename %s | %FileCheck %s -check-prefix=CHECK-INT
// RUN: %target-swift-ide-test -range -pos=127:3 -end-pos=127:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-INT-LVALUE
// RUN: %target-swift-ide-test -range -pos=128:13 -end-pos=128:15 -source-filename %s | %FileCheck %s -check-prefix=CHECK-INT-INOUT

// CHECK-INVALID: <Kind>Invalid</Kind>

// CHECK-INT: <Type>Int</Type>
// CHECK-INT-LVALUE: <Type>@lvalue Int</Type>
// CHECK-INT-INOUT: <Type>inout Int</Type>

// CHECK1: <Kind>SingleDecl</Kind>
// CHECK1-NEXT: <Content>func foo1() -> Int { return 0 }</Content>
// CHECK1-NEXT: <Context>swift_ide_test.(file)</Context>
// CHECK1-NEXT: <Declared>foo1</Declared><OutscopeReference>false</OutscopeReference>
// CHECK1-NEXT: <ASTNodes>1</ASTNodes>
// CHECK1-NEXT: <end>

// CHECK2: <Kind>SingleDecl</Kind>
// CHECK2-NEXT: <Content>class C { func foo() {} }</Content>
// CHECK2-NEXT: <Context>swift_ide_test.(file)</Context>
// CHECK2-NEXT: <Declared>C</Declared><OutscopeReference>false</OutscopeReference>
// CHECK2-NEXT: <Declared>foo</Declared><OutscopeReference>false</OutscopeReference>
// CHECK2-NEXT: <ASTNodes>1</ASTNodes>
// CHECK2-NEXT: <end>

// CHECK3: <Kind>SingleDecl</Kind>
// CHECK3-NEXT: <Content>struct S { func foo() {} }</Content>
// CHECK3-NEXT: <Context>swift_ide_test.(file)</Context>
// CHECK3-NEXT: <Declared>S</Declared><OutscopeReference>false</OutscopeReference>
// CHECK3-NEXT: <Declared>foo</Declared><OutscopeReference>false</OutscopeReference>
// CHECK3-NEXT: <ASTNodes>1</ASTNodes>
// CHECK3-NEXT: <end>

// CHECK4: <Kind>MultiStatement</Kind>
// CHECK4-NEXT: <Content>aaa = aaa + 3
// CHECK4-NEXT: if aaa == 3 { aaa = 4 }</Content>
// CHECK4-NEXT: <Type>Void</Type>
// CHECK4-NEXT: <Context>swift_ide_test.(file).foo()</Context>
// CHECK4-NEXT: <Referenced>aaa</Referenced><Type>@lvalue Int</Type>
// CHECK4-NEXT: <ASTNodes>2</ASTNodes>
// CHECK4-NEXT: <end>

// CHECK5: <Kind>MultiStatement</Kind>
// CHECK5-NEXT: <Content>aaa = aaa + 3
// CHECK5-NEXT: if aaa == 3 { aaa = 4 }
// CHECK5-NEXT: return aaa</Content>
// CHECK5-NEXT: <Type>Int</Type>
// CHECK5-NEXT: <Context>swift_ide_test.(file).foo()</Context>
// CHECK5-NEXT: <Referenced>aaa</Referenced><Type>@lvalue Int</Type>
// CHECK5-NEXT: <ASTNodes>3</ASTNodes>
// CHECK5-NEXT: <end>

// CHECK6: <Kind>MultiStatement</Kind>
// CHECK6-NEXT: if aaa == 3 { aaa = 4 }
// CHECK6-NEXT: return aaa</Content>
// CHECK6-NEXT: <Type>Int</Type>
// CHECK6-NEXT: <Context>swift_ide_test.(file).foo()</Context>
// CHECK6-NEXT: <Referenced>aaa</Referenced><Type>@lvalue Int</Type>
// CHECK6-NEXT: <ASTNodes>2</ASTNodes>
// CHECK6-NEXT: <end>

// CHECK7: <Kind>MultiStatement</Kind>
// CHECK7-NEXT: <Content>var b = a.bigEndian
// CHECK7-NEXT:   let c = a.byteSwapped
// CHECK7-NEXT:   b = b.bigEndian.bigEndian.byteSwapped
// CHECK7-NEXT:   print(b + c)</Content>
// CHECK7-NEXT: <Type>Void</Type>
// CHECK7-NEXT: <Context>swift_ide_test.(file).foo2()</Context>
// CHECK7-NEXT: <Declared>b</Declared><OutscopeReference>false</OutscopeReference>
// CHECK7-NEXT: <Declared>c</Declared><OutscopeReference>false</OutscopeReference>
// CHECK7-NEXT: <Referenced>a</Referenced><Type>Int</Type>
// CHECK7-NEXT: <Referenced>b</Referenced><Type>@lvalue Int</Type>
// CHECK7-NEXT: <Referenced>c</Referenced><Type>Int</Type>
// CHECK7-NEXT: <ASTNodes>4</ASTNodes>
// CHECK7-NEXT: <end>

// CHECK8: <Kind>MultiStatement</Kind>
// CHECK8-NEXT: <Content>let c = s.foo() + b
// CHECK8-NEXT:   s = s.increment()
// CHECK8-NEXT:   return c + b</Content>
// CHECK8-NEXT: <Type>Int</Type>
// CHECK8-NEXT: <Context>swift_ide_test.(file).foo3(s:)</Context>
// CHECK8-NEXT: <Declared>c</Declared><OutscopeReference>false</OutscopeReference>
// CHECK8-NEXT: <Referenced>s</Referenced><Type>@lvalue S1</Type>
// CHECK8-NEXT: <Referenced>b</Referenced><Type>Int</Type>
// CHECK8-NEXT: <Referenced>c</Referenced><Type>Int</Type>
// CHECK8-NEXT: <ASTNodes>3</ASTNodes>
// CHECK8-NEXT: <end>

// CHECK9: <Kind>MultiStatement</Kind>
// CHECK9-NEXT: <Content>let b = s.a
// CHECK9-NEXT:   let c = s.foo() + b
// CHECK9-NEXT:   return c + b</Content>
// CHECK9-NEXT: <Type>Int</Type>
// CHECK9-NEXT: <Context>swift_ide_test.(file).foo4(s:)</Context>
// CHECK9-NEXT: <Declared>b</Declared><OutscopeReference>false</OutscopeReference>
// CHECK9-NEXT: <Declared>c</Declared><OutscopeReference>false</OutscopeReference>
// CHECK9-NEXT: <Referenced>s</Referenced><Type>S1</Type>
// CHECK9-NEXT: <Referenced>b</Referenced><Type>Int</Type>
// CHECK9-NEXT: <Referenced>c</Referenced><Type>Int</Type>
// CHECK9-NEXT: <ASTNodes>3</ASTNodes>
// CHECK9-NEXT: <end>

// CHECK10: <Kind>MultiStatement</Kind>
// CHECK10-NEXT: <Content>let a = c.c.getC().c.getC().getC().getC()
// CHECK10-NEXT:   let b = a.c.c.c.c.getC().getC()</Content>
// CHECK10-NEXT: <Type>Void</Type>
// CHECK10-NEXT: <Context>swift_ide_test.(file).foo5(c:)</Context>
// CHECK10-NEXT: <Declared>a</Declared><OutscopeReference>true</OutscopeReference>
// CHECK10-NEXT: <Declared>b</Declared><OutscopeReference>true</OutscopeReference>
// CHECK10-NEXT: <Referenced>c</Referenced><Type>C1</Type>
// CHECK10-NEXT: <Referenced>a</Referenced><Type>C1</Type>
// CHECK10-NEXT: <ASTNodes>2</ASTNodes>
// CHECK10-NEXT: <end>

// CHECK11: <Kind>MultiStatement</Kind>
// CHECK11-NEXT: <Content>let a = c.c.getC().c.getC().getC().getC()
// CHECK11-NEXT:   let b = a.c.c.c.c.getC().getC()
// CHECK11-NEXT:   let d = a.c.getC().getC().c.c</Content>
// CHECK11-NEXT: <Type>Void</Type>
// CHECK11-NEXT: <Context>swift_ide_test.(file).foo5(c:)</Context>
// CHECK11-NEXT: <Declared>a</Declared><OutscopeReference>true</OutscopeReference>
// CHECK11-NEXT: <Declared>b</Declared><OutscopeReference>true</OutscopeReference>
// CHECK11-NEXT: <Declared>d</Declared><OutscopeReference>true</OutscopeReference>
// CHECK11-NEXT: <Referenced>c</Referenced><Type>C1</Type>
// CHECK11-NEXT: <Referenced>a</Referenced><Type>C1</Type>
// CHECK11-NEXT: <ASTNodes>3</ASTNodes>
// CHECK11-NEXT: <end>

// CHECK12: <Kind>MultiStatement</Kind>
// CHECK12-NEXT: <Content>let a = c.c.getC().c.getC().getC().getC()
// CHECK12-NEXT:   let b = a.c.c.c.c.getC().getC()
// CHECK12-NEXT:   let d = a.c.getC().getC().c.c
// CHECK12-NEXT:   return a.take(another: b).take(another: d)</Content>
// CHECK12-NEXT: <Type>C1</Type>
// CHECK12-NEXT: <Context>swift_ide_test.(file).foo5(c:)</Context>
// CHECK12-NEXT: <Declared>a</Declared><OutscopeReference>false</OutscopeReference>
// CHECK12-NEXT: <Declared>b</Declared><OutscopeReference>false</OutscopeReference>
// CHECK12-NEXT: <Declared>d</Declared><OutscopeReference>false</OutscopeReference>
// CHECK12-NEXT: <Referenced>c</Referenced><Type>C1</Type>
// CHECK12-NEXT: <Referenced>a</Referenced><Type>C1</Type>
// CHECK12-NEXT: <Referenced>b</Referenced><Type>C1</Type>
// CHECK12-NEXT: <Referenced>d</Referenced><Type>C1</Type>
// CHECK12-NEXT: <ASTNodes>4</ASTNodes>
// CHECK12-NEXT: <end>

// CHECK13: <Kind>MultiStatement</Kind>
// CHECK13-NEXT: <Content>let a = 3
// CHECK13-NEXT:     var b = a.bigEndian
// CHECK13-NEXT:     let c = a.byteSwapped
// CHECK13-NEXT:     b = b.bigEndian.bigEndian.byteSwapped
// CHECK13-NEXT:     print(b + c)</Content>
// CHECK13-NEXT: <Type>Void</Type>
// CHECK13-NEXT: <Context>swift_ide_test.(file).foo6().explicit closure discriminator=0</Context>
// CHECK13-NEXT: <Declared>a</Declared><OutscopeReference>false</OutscopeReference>
// CHECK13-NEXT: <Declared>b</Declared><OutscopeReference>false</OutscopeReference>
// CHECK13-NEXT: <Declared>c</Declared><OutscopeReference>false</OutscopeReference>
// CHECK13-NEXT: <Referenced>a</Referenced><Type>Int</Type>
// CHECK13-NEXT: <Referenced>b</Referenced><Type>@lvalue Int</Type>
// CHECK13-NEXT: <Referenced>c</Referenced><Type>Int</Type>
// CHECK13-NEXT: <ASTNodes>5</ASTNodes>
// CHECK13-NEXT: <end>

// CHECK14: <Kind>MultiStatement</Kind>
// CHECK14-NEXT: <Content>let a = 3
// CHECK14-NEXT:     var b = a.bigEndian
// CHECK14-NEXT:     let c = a.byteSwapped
// CHECK14-NEXT:     b = b.bigEndian.bigEndian.byteSwapped
// CHECK14-NEXT:     print(b + c)
// CHECK14-NEXT:     return { () -> Int in
// CHECK14-NEXT:       let a = 3
// CHECK14-NEXT:       var b = a.bigEndian
// CHECK14-NEXT:       let c = a.byteSwapped
// CHECK14-NEXT:       b = b.bigEndian.bigEndian.byteSwapped
// CHECK14-NEXT:       print(b + c)
// CHECK14-NEXT:       return 1
// CHECK14-NEXT:     }()</Content>
// CHECK14-NEXT: <Type>Int</Type>
// CHECK14-NEXT: <Context>swift_ide_test.(file).foo6().explicit closure discriminator=0</Context>
// CHECK14-NEXT: <Declared>a</Declared><OutscopeReference>false</OutscopeReference>
// CHECK14-NEXT: <Declared>b</Declared><OutscopeReference>false</OutscopeReference>
// CHECK14-NEXT: <Declared>c</Declared><OutscopeReference>false</OutscopeReference>
// CHECK14-NEXT: <Declared>a</Declared><OutscopeReference>false</OutscopeReference>
// CHECK14-NEXT: <Declared>b</Declared><OutscopeReference>false</OutscopeReference>
// CHECK14-NEXT: <Declared>c</Declared><OutscopeReference>false</OutscopeReference>
// CHECK14-NEXT: <Referenced>a</Referenced><Type>Int</Type>
// CHECK14-NEXT: <Referenced>b</Referenced><Type>@lvalue Int</Type>
// CHECK14-NEXT: <Referenced>c</Referenced><Type>Int</Type>
// CHECK14-NEXT: <Referenced>a</Referenced><Type>Int</Type>
// CHECK14-NEXT: <Referenced>b</Referenced><Type>@lvalue Int</Type>
// CHECK14-NEXT: <Referenced>c</Referenced><Type>Int</Type>
// CHECK14-NEXT: <ASTNodes>6</ASTNodes>
// CHECK14-NEXT: <end>

// CHECK15: <Kind>MultiStatement</Kind>
// CHECK15-NEXT: <Content>let a = 3
// CHECK15-NEXT:       var b = a.bigEndian
// CHECK15-NEXT:       let c = a.byteSwapped
// CHECK15-NEXT:       b = b.bigEndian.bigEndian.byteSwapped</Content>
// CHECK15-NEXT: <Type>Void</Type>
// CHECK15-NEXT: <Context>swift_ide_test.(file).foo6().explicit closure discriminator=0.explicit closure discriminator=0</Context>
// CHECK15-NEXT: <Declared>a</Declared><OutscopeReference>false</OutscopeReference>
// CHECK15-NEXT: <Declared>b</Declared><OutscopeReference>true</OutscopeReference>
// CHECK15-NEXT: <Declared>c</Declared><OutscopeReference>true</OutscopeReference>
// CHECK15-NEXT: <Referenced>a</Referenced><Type>Int</Type>
// CHECK15-NEXT: <Referenced>b</Referenced><Type>@lvalue Int</Type>
// CHECK15-NEXT: <ASTNodes>4</ASTNodes>
// CHECK15-NEXT: <end>

// CHECK16: <Kind>MultiStatement</Kind>
// CHECK16-NEXT: <Content>let a = 3
// CHECK16-NEXT:       var b = a.bigEndian
// CHECK16-NEXT:       let c = a.byteSwapped
// CHECK16-NEXT:       b = b.bigEndian.bigEndian.byteSwapped
// CHECK16-NEXT:       print(b + c)
// CHECK16-NEXT:       return 1</Content>
// CHECK16-NEXT: <Type>Int</Type>
// CHECK16-NEXT: <Context>swift_ide_test.(file).foo6().explicit closure discriminator=0.explicit closure discriminator=0</Context>
// CHECK16-NEXT: <Declared>a</Declared><OutscopeReference>false</OutscopeReference>
// CHECK16-NEXT: <Declared>b</Declared><OutscopeReference>false</OutscopeReference>
// CHECK16-NEXT: <Declared>c</Declared><OutscopeReference>false</OutscopeReference>
// CHECK16-NEXT: <Referenced>a</Referenced><Type>Int</Type>
// CHECK16-NEXT: <Referenced>b</Referenced><Type>@lvalue Int</Type>
// CHECK16-NEXT: <Referenced>c</Referenced><Type>Int</Type>
// CHECK16-NEXT: <ASTNodes>6</ASTNodes>
// CHECK16-NEXT: <end>

// CHECK17: <Kind>SingleExpression</Kind>
// CHECK17-NEXT: <Content>print(b + c)</Content>
// CHECK17-NEXT: <Type>()</Type>
// CHECK17-NEXT: <Context>swift_ide_test.(file).foo6().explicit closure discriminator=0.explicit closure discriminator=0</Context>
// CHECK17-NEXT: <Referenced>b</Referenced><Type>@lvalue Int</Type>
// CHECK17-NEXT: <Referenced>c</Referenced><Type>Int</Type>
// CHECK17-NEXT: <ASTNodes>1</ASTNodes>
// CHECK17-NEXT: <end>

// CHECK18: <Kind>MultiStatement</Kind>
// CHECK18-NEXT: <Content>case 1:
// CHECK18-NEXT:     return 0
// CHECK18-NEXT:   case 2:
// CHECK18-NEXT:     return 1</Content>
// CHECK18-NEXT: <Type>Void</Type>
// CHECK18-NEXT: <Context>swift_ide_test.(file).foo7(a:)</Context>
// CHECK18-NEXT: <Entry>Multi</Entry>
// CHECK18-NEXT: <ASTNodes>2</ASTNodes>
// CHECK18-NEXT: <end>

// CHECK19: <Kind>SingleStatement</Kind>
// CHECK19-NEXT: <Content>case 1:
// CHECK19-NEXT:     return 0</Content>
// CHECK19-NEXT: <Type>Void</Type>
// CHECK19-NEXT: <Context>swift_ide_test.(file).foo7(a:)</Context>
// CHECK19-NEXT: <ASTNodes>1</ASTNodes>
// CHECK19-NEXT: <end>

// CHECK20: <Kind>MultiStatement</Kind>
// CHECK20-NEXT: <Content>case 2:
// CHECK20-NEXT:     return 1
// CHECK20-NEXT:   default:
// CHECK20-NEXT:     return a</Content>
// CHECK20-NEXT: <Type>Void</Type>
// CHECK20-NEXT: <Context>swift_ide_test.(file).foo7(a:)</Context>
// CHECK20-NEXT: <Entry>Multi</Entry>
// CHECK20-NEXT: <ASTNodes>2</ASTNodes>
// CHECK20-NEXT: <end>

// CHECK21: <Orphan>Break</Orphan>
// CHECK22: <Orphan>Continue</Orphan>
// CHECK23: <Orphan>Continue</Orphan>
// CHECK24: <Orphan>Break</Orphan>
// CHECK25: <Orphan>Break</Orphan>
// CHECK26: <Orphan>Continue</Orphan>

// CHECK27: <Kind>MultiStatement</Kind>
// CHECK27-NEXT: <Content>foo9(1, 2)
// CHECK27-NEXT:     foo9(1, 2)</Content>
// CHECK27-NEXT: <Type>Void</Type>
// CHECK27-NEXT: <Context>swift_ide_test.(file).foo9(_:_:)</Context>
// CHECK27-NEXT: <Referenced>foo9</Referenced><Type>(Int, Int) -> Int</Type>
// CHECK27-NEXT: <ASTNodes>2</ASTNodes>
// CHECK27-NEXT: <end>
