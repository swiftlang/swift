// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/mods
// RUN: split-file --leading-lines %s %t

// RUN: %target-swift-frontend -module-name Foo -emit-module -emit-module-path %t/mods/Foo.swiftmodule -emit-module-doc -emit-module-doc-path %t/mods/Foo.swiftdoc -group-info-path %t/group.json %t/Foo.swift
// RUN: %target-swift-frontend -module-name Bar -emit-module -emit-module-path %t/mods/Bar.swiftmodule -emit-module-doc -emit-module-doc-path %t/mods/Bar.swiftdoc -I%t/mods %t/Bar.swift

//--- group.json
{
  "TestGroup": [
    "Foo.swift",
  ]
}

//--- Foo.swift
public protocol FooProto {
  associatedtype T
}

public extension FooProto {
  func fooExt() {}
}

public extension FooProto {
  func fooExt2() {}
}

public extension FooProto where T == Int {
  func fooIntExt() {}
}

public struct FooStruct: FooProto {
  public typealias T = Int
  public func foo() {}
}

//--- Bar.swift
import Foo

public extension FooProto {
  func barExt() {}
}

public extension FooProto where T == Int {
  func barIntExt() {}
}

//--- Baz.swift
import Foo
import Bar

// The generated interface for Foo will contain all the extensions as well
// as the "synthesized extension" on FooStruct itself, ie. the extension
// functions are added to FooStruct as if they were written there in the source.
//
// We prefer jumping to these declarations rather than the one in the extension,
// but also have to be careful not to attempt to do so for extensions outside
// of the original module - these will *not* have "synthesized extensions"
// in their generated interface.
func test(f: FooStruct) {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):5 %t/Baz.swift -- %t/Baz.swift -I %t/mods -target %target-triple | %FileCheck --check-prefix=CHECK-EXT %t/Baz.swift
  f.fooExt()
  // CHECK-EXT: 3Foo0A5ProtoPAAE6fooExtyyF::SYNTHESIZED::s:3Foo0A6StructV
  // CHECK-EXT: <Group>TestGroup</Group>

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):5 %t/Baz.swift -- %t/Baz.swift -I %t/mods -target %target-triple | %FileCheck --check-prefix=CHECK-EXT2 %t/Baz.swift
  f.fooExt2()
  // CHECK-EXT2: s:3Foo0A5ProtoPAAE7fooExt2yyF::SYNTHESIZED::s:3Foo0A6StructV
  // CHECK-EXT2: <Group>TestGroup</Group>

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):5 %t/Baz.swift -- %t/Baz.swift -I %t/mods -target %target-triple | %FileCheck --check-prefix=CHECK-INTEXT %t/Baz.swift
  f.fooIntExt()
  // CHECK-INTEXT: s:3Foo0A5ProtoPAASi1TRtzrlE9fooIntExtyyF::SYNTHESIZED::s:3Foo0A6StructV
  // CHECK-INTEXT: <Group>TestGroup</Group>

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):5 %t/Baz.swift -- %t/Baz.swift -I %t/mods -target %target-triple | %FileCheck --check-prefix=CHECK-BAREXT %t/Baz.swift
  f.barExt()
  // CHECK-BAREXT: s:3Foo0A5ProtoP3BarE6barExtyyF
  // CHECK-BAREXT-NOT: <Group>TestGroup</Group>

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):5 %t/Baz.swift -- %t/Baz.swift -I %t/mods -target %target-triple | %FileCheck --check-prefix=CHECK-BARINTEXT %t/Baz.swift
  f.barIntExt()
  // CHECK-BARINTEXT: s:3Foo0A5ProtoP3BarSi1TRtzrlE9barIntExtyyF
  // CHECK-BARINTEXT-NOT: <Group>TestGroup</Group>
}
