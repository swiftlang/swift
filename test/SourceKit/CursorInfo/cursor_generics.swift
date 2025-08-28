func testGenerics<T>(x: T) {
}

/// Doc Comment...
func testGenericsWithComment<T>(x: T) {
}

func someFunc <A>() -> A {
    fatalError()
}

// rdar://problem/36871908
class MyType<T> {
	let test: Bool = false
	let items: [Int] = []
	func myMethod() {
	  if test {}
	  for i in items {}
	}
}

// rdar://76750555
public protocol IP {
    init(networkBytes: Int)
}

public struct HostRecord<IPType: IP> {
    func foo() {
        let ipType = IPType(networkBytes: 42)
    }
}

public protocol Proto<Assoc> {
  associatedtype Assoc
}

struct HasValueGenericParam<let Param: Int> {
  func foo() {
    _ = Param
  }
}

// RUN: %sourcekitd-test -req=cursor -pos=1:10 %s -- %s | %FileCheck -check-prefix=CHECK1 %s
// CHECK1: <Declaration>func testGenerics&lt;T&gt;(x: <Type usr="s:15cursor_generics12testGenerics1xyx_tlF1TL_xmfp">T</Type>)</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=5:10 %s -- %s | %FileCheck -check-prefix=CHECK2 %s
// CHECK2: <Function
// CHECK2: <Declaration>func testGenericsWithComment&lt;T&gt;(x: T)</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=8:16 %s -- %s | %FileCheck -check-prefix=CHECK3 %s
// CHECK3: source.lang.swift.decl.generic_type_param
// CHECK3: <Declaration>A</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=17:8 %s -- %s | %FileCheck -check-prefix=CHECK4 %s
// CHECK4: source.lang.swift.ref.var.instance
// CHECK4: <Declaration>let test: <Type usr="s:Sb">Bool</Type></Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=18:14 %s -- %s | %FileCheck -check-prefix=CHECK5 %s
// CHECK5: source.lang.swift.ref.var.instance
// CHECK5: <Declaration>let items: [<Type usr="s:Si">Int</Type>]</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=29:22 %s -- %s | %FileCheck -check-prefix=CHECK_IP_TYPE %s
// CHECK_IP_TYPE: source.lang.swift.ref.generic_type_param
// CHECK_IP_TYPE: <Declaration>IPType : <Type usr="s:15cursor_generics2IPP">IP</Type></Declaration>

// RUN: %sourcekitd-test -req=cursor -cursor-action -pos=33:23 %s -- %s | %FileCheck --check-prefixes=CHECK_ASSOC_COMMON,CHECK_ASSOC_PRIMARY %s
// RUN: %sourcekitd-test -req=cursor -cursor-action -pos=34:18 %s -- %s | %FileCheck --check-prefixes=CHECK_ASSOC_COMMON,CHECK_ASSOC_DECL %s
// CHECK_ASSOC_PRIMARY: source.lang.swift.ref.associatedtype
// CHECK_ASSOC_DECL: source.lang.swift.decl.associatedtype
// CHECK_ASSOC_COMMON: Assoc
// CHECK_ASSOC_COMMON: Self.Assoc.Type
// CHECK_ASSOC_COMMON: <Declaration>associatedtype Assoc</Declaration>
// CHECK_ASSOC_COMMON: <decl.associatedtype><syntaxtype.keyword>associatedtype</syntaxtype.keyword> <decl.name>Assoc</decl.name></decl.associatedtype>
// CHECK_ASSOC_COMMON:      ACTIONS BEGIN
// CHECK_ASSOC_COMMON-NEXT: source.refactoring.kind.rename.global
// CHECK_ASSOC_COMMON-NEXT: Global Rename
// CHECK_ASSOC_COMMON-NEXT: ACTIONS END

// RUN: %sourcekitd-test -req=cursor -pos=39:9 %s -- %s | %FileCheck -check-prefix=CHECK_VALUE_GENERIC %s
// CHECK_VALUE_GENERIC: source.lang.swift.ref.generic_type_param (37:33-37:38)
// CHECK_VALUE_GENERIC: <Declaration>let Param : <Type usr="s:Si">Int</Type></Declaration>
