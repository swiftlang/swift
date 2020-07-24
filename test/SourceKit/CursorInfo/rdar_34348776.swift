public struct MyStruct<T> {}
public typealias Alias<T> = MyStruct<T>
public typealias Aliased = Alias

// RUN: %sourcekitd-test -req=cursor -pos=3:18 %s -- %s | %FileCheck %s

// CHECK: source.lang.swift.decl.typealias (3:18-3:25)
// CHECK-NEXT: Aliased
// CHECK-NEXT: s:13rdar_343487767Aliaseda
// CHECK-NEXT: MyStruct<T>.Type
// CHECK-NEXT: $s13rdar_343487768MyStructVyxGmD
// CHECK-NEXT: <Declaration>public typealias Aliased&lt;T&gt; = <Type usr="s:13rdar_343487765Aliasa">Alias</Type>&lt;T&gt;</Declaration>
// CHECK-NEXT: <decl.typealias><syntaxtype.keyword>public</syntaxtype.keyword> <syntaxtype.keyword>typealias</syntaxtype.keyword> <decl.name>Aliased</decl.name>&lt;<decl.generic_type_param usr="s:13rdar_343487767Aliaseda1Txmfp"><decl.generic_type_param.name>T</decl.generic_type_param.name></decl.generic_type_param>&gt; = <ref.typealias usr="s:13rdar_343487765Aliasa">Alias</ref.typealias>&lt;T&gt;</decl.typealias>
