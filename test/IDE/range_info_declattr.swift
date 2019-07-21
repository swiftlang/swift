class ObjCBase {
  var foo: Int { return 1 }
}
@objc class ObjCClass : ObjCBase {
  override var foo: Int {
    return 42
  }
  @objc var bar = 12, baz = 13
}
class Derived : ObjCBase {
  @available(*, unavailable)
  override var quux: Int {
    @inlinable get { return 0 }
  }

  subscript(idx: Int) -> Int {
    @available(*, unavailable)
    get { return 0 }

    @available(*, unavailable)
    @inlineable
    set { }
  }
}

// RUN: %target-swift-ide-test -range -pos=4:1 -end-pos=9:2 -source-filename %s | %FileCheck %s -check-prefix=CHECK1
// RUN: %target-swift-ide-test -range -pos=5:3 -end-pos=7:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK2
// RUN: %target-swift-ide-test -range -pos=5:25 -end-pos=7:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK3
// RUN: %target-swift-ide-test -range -pos=8:3 -end-pos=8:31 -source-filename %s | %FileCheck %s -check-prefix=CHECK4
// RUN: %target-swift-ide-test -range -pos=13:5 -end-pos=13:32 -source-filename %s | %FileCheck %s -check-prefix=CHECK5
// RUN: %target-swift-ide-test -range -pos=13:16 -end-pos=13:32 -source-filename %s | %FileCheck %s -check-prefix=CHECK6
// RUN: %target-swift-ide-test -range -pos=12:26 -end-pos=14:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK7
// RUN: %target-swift-ide-test -range -pos=17:5 -end-pos=18:21 -source-filename %s | %FileCheck %s -check-prefix=CHECK8
// RUN: %target-swift-ide-test -range -pos=20:5 -end-pos=22:12 -source-filename %s | %FileCheck %s -check-prefix=CHECK9
// RUN: %target-swift-ide-test -range -pos=21:5 -end-pos=22:12 -source-filename %s | %FileCheck %s -check-prefix=CHECK10

// CHECK1: <Kind>SingleDecl</Kind>
// CHECK1-NEXT: <Content>@objc class ObjCClass : ObjCBase {
// CHECK1-NEXT:   override var foo: Int {
// CHECK1-NEXT:     return 42
// CHECK1-NEXT:   }
// CHECK1-NEXT:   @objc var bar = 12, baz = 13
// CHECK1-NEXT: }</Content>
// CHECK1-NEXT: <Context>swift_ide_test.(file)</Context>
// CHECK1-NEXT: <Declared>ObjCClass</Declared><OutscopeReference>false</OutscopeReference>
// CHECK1-NEXT: <Declared>foo</Declared><OutscopeReference>false</OutscopeReference>
// CHECK1-NEXT: <Declared>_</Declared><OutscopeReference>false</OutscopeReference>
// CHECK1-NEXT: <Declared>bar</Declared><OutscopeReference>false</OutscopeReference>
// CHECK1-NEXT: <Declared>baz</Declared><OutscopeReference>false</OutscopeReference>
// CHECK1-NEXT: <ASTNodes>1</ASTNodes>
// CHECK1-NEXT: <end>

// CHECK2: <Kind>SingleDecl</Kind>
// CHECK2-NEXT: <Content>override var foo: Int {
// CHECK2-NEXT:     return 42
// CHECK2-NEXT:   }</Content>
// CHECK2-NEXT: <Context>swift_ide_test.(file).ObjCClass</Context>
// CHECK2-NEXT: <Declared>foo</Declared><OutscopeReference>false</OutscopeReference>
// CHECK2-NEXT: <ASTNodes>1</ASTNodes>
// CHECK2-NEXT: <end>

// CHECK3: <Kind>SingleDecl</Kind>
// CHECK3-NEXT: <Content>{
// CHECK3-NEXT:     return 42
// CHECK3-NEXT:   }</Content>
// CHECK3-NEXT: <Context>swift_ide_test.(file).ObjCClass</Context>
// CHECK3-NEXT: <Declared>_</Declared><OutscopeReference>false</OutscopeReference>
// CHECK3-NEXT: <ASTNodes>1</ASTNodes>
// CHECK3-NEXT: <end>

// CHECK4: <Kind>SingleDecl</Kind>
// CHECK4-NEXT: <Content>@objc var bar = 12, baz = 13</Content>
// CHECK4-NEXT: <Context>swift_ide_test.(file).ObjCClass</Context>
// CHECK4-NEXT: <Declared>bar</Declared><OutscopeReference>false</OutscopeReference>
// CHECK4-NEXT: <Declared>baz</Declared><OutscopeReference>false</OutscopeReference>
// CHECK4-NEXT: <ASTNodes>1</ASTNodes>
// CHECK4-NEXT: <end>

// CHECK5: <Kind>SingleDecl</Kind>
// CHECK5-NEXT: <Content>@inlinable get { return 0 }</Content>
// CHECK5-NEXT: <Context>swift_ide_test.(file).Derived</Context>
// CHECK5-NEXT: <Declared>_</Declared><OutscopeReference>false</OutscopeReference>
// CHECK5-NEXT: <ASTNodes>1</ASTNodes>
// CHECK5-NEXT: <end>

// CHECK6: <Kind>Invalid</Kind>
// CHECK6-NEXT: <Content>get { return 0 }</Content>
// CHECK6-NEXT: <ASTNodes>0</ASTNodes>
// CHECK6-NEXT: <end>

// CHECK7: <Kind>Invalid</Kind>
// CHECK7-NEXT: <Content>{
// CHECK7-NEXT:     @inlinable get { return 0 }
// CHECK7-NEXT:   }</Content>
// CHECK7-NEXT: <ASTNodes>0</ASTNodes>
// CHECK7-NEXT: <end>

// CHECK8: <Kind>SingleDecl</Kind>
// CHECK8-NEXT: <Content>@available(*, unavailable)
// CHECK8-NEXT:     get { return 0 }</Content>
// CHECK8-NEXT: <Context>swift_ide_test.(file).Derived</Context>
// CHECK8-NEXT: <Declared>_</Declared><OutscopeReference>false</OutscopeReference>
// CHECK8-NEXT: <ASTNodes>1</ASTNodes>
// CHECK8-NEXT: <end>

// CHECK9: <Kind>SingleDecl</Kind>
// CHECK9-NEXT: <Content>@available(*, unavailable)
// CHECK9-NEXT:     @inlineable
// CHECK9-NEXT:     set { }</Content>
// CHECK9-NEXT: <Context>swift_ide_test.(file).Derived</Context>
// CHECK9-NEXT: <Declared>_</Declared><OutscopeReference>false</OutscopeReference>
// CHECK9-NEXT: <ASTNodes>1</ASTNodes>
// CHECK9-NEXT: <end>

// CHECK10: <Kind>Invalid</Kind>
// CHECK10-NEXT: <Content>@inlineable
// CHECK10-NEXT:     set { }</Content>
// CHECK10-NEXT: <ASTNodes>0</ASTNodes>
// CHECK10-NEXT: <end>
