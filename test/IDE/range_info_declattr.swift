class ObjCBase {
  var foo: Int { return 1 }
}
@objc class ObjCClass : ObjCBase {
  override var foo: Int {
    return 42
  }
  @objc var bar = 12, baz = 13
}

// RUN: %target-swift-ide-test -range -pos=4:1 -end-pos=9:2 -source-filename %s | %FileCheck %s -check-prefix=CHECK1
// RUN: %target-swift-ide-test -range -pos=5:3 -end-pos=7:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK2
// RUN: %target-swift-ide-test -range -pos=5:25 -end-pos=7:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK3
// RUN: %target-swift-ide-test -range -pos=8:3 -end-pos=8:31 -source-filename %s | %FileCheck %s -check-prefix=CHECK4

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
