func foo() -> Int{
  // some comments
  var aaa = 1 + 2
  aaa = aaa + 3
  if aaa == 3 { aaa = 4 }
  // some comments
  return aaa
}

func foo1() -> Int{
  /// some comments
  var aaa = 1 + 2
  aaa = aaa + 3
  if aaa == 3 { aaa = 4 }
  /// some comments
  return aaa
}

func foo2() -> Int{
  /* some comments*/
  var aaa = 1 + 2
  aaa = aaa + 3
  if aaa == 3 { aaa = 4 }
  /* some comments*/
  return aaa
}

// RUN: %target-swift-ide-test -range -pos=2:1 -end-pos 6:19 -source-filename %s | %FileCheck %s -check-prefix=CHECK1
// RUN: %target-swift-ide-test -range -pos=11:1 -end-pos 15:20 -source-filename %s | %FileCheck %s -check-prefix=CHECK-KIND
// RUN: %target-swift-ide-test -range -pos=20:1 -end-pos 24:21 -source-filename %s | %FileCheck %s -check-prefix=CHECK-KIND
// RUN: %target-swift-ide-test -range -pos=1:1 -end-pos 15:20 -source-filename %s | %FileCheck %s -check-prefix=CHECK-INVALID

// CHECK1: <Kind>MultiStatement</Kind>
// CHECK1-NEXT: <Content>// some comments
// CHECK1-NEXT:   var aaa = 1 + 2
// CHECK1-NEXT:   aaa = aaa + 3
// CHECK1-NEXT:   if aaa == 3 { aaa = 4 }</Content>
// CHECK1-NEXT: <Type>Void</Type>
// CHECK1-NEXT: <Context>swift_ide_test.(file).foo()</Context>
// CHECK1-NEXT: <Declared>aaa</Declared><OutscopeReference>true</OutscopeReference>
// CHECK1-NEXT: <Referenced>aaa</Referenced><Type>@lvalue Int</Type>
// CHECK1-NEXT: <ASTNodes>3</ASTNodes>
// CHECK1-NEXT: <end>

// CHECK-KIND: <Kind>MultiStatement</Kind>
// CHECK-INVALID: <Kind>Invalid</Kind>
