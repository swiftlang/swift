public struct MyStruct<T> {}
public typealias Alias<T> = MyStruct<T>
public typealias Aliased = Alias

// RUN: %sourcekitd-test -req=cursor -pos=3:18 %s -- %s -module-name AliasTest | %FileCheck %s

// CHECK: source.lang.swift.decl.typealias (3:18-3:25)
// CHECK-NEXT: Aliased
// CHECK-NEXT: s:9AliasTest7Aliaseda
// CHECK-NEXT: source.lang.swift
// CHECK-NEXT: Alias.Type
// CHECK-NEXT: $s9AliasTest0A0amD
// CHECK-NEXT: AliasTest{{$}}
// CHECK-NEXT: <Declaration>public typealias Aliased = <Type usr="s:9AliasTest0A0a">Alias</Type></Declaration>
// CHECK-NEXT: <decl.typealias><syntaxtype.keyword>public</syntaxtype.keyword> <syntaxtype.keyword>typealias</syntaxtype.keyword> <decl.name>Aliased</decl.name> = <ref.typealias usr="s:9AliasTest0A0a">Alias</ref.typealias></decl.typealias>
