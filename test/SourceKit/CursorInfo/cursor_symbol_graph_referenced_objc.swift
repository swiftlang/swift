import Foo

struct Parent {
  func refObjCType(x: FooStruct1) {}
}

// REQUIRES: objc_interop
// RUN:  %sourcekitd-test -req=cursor -pos=4:8 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple -F %S/../Inputs/libIDE-mock-sdk | %FileCheck -check-prefixes=OBJC %s

// OBJC:      SYMBOL GRAPH BEGIN
// OBJC:      "declarationFragments": [
// OBJC-NEXT:     {
// OBJC-NEXT:       "kind": "keyword",
// OBJC-NEXT:       "spelling": "func"
// OBJC-NEXT:     },
// OBJC-NEXT:     {
// OBJC-NEXT:       "kind": "text",
// OBJC-NEXT:       "spelling": " "
// OBJC-NEXT:     },
// OBJC-NEXT:     {
// OBJC-NEXT:       "kind": "identifier",
// OBJC-NEXT:       "spelling": "refObjCType"
// OBJC-NEXT:     },
// OBJC-NEXT:     {
// OBJC-NEXT:       "kind": "text",
// OBJC-NEXT:       "spelling": "("
// OBJC-NEXT:     },
// OBJC-NEXT:     {
// OBJC-NEXT:       "kind": "externalParam",
// OBJC-NEXT:       "spelling": "x"
// OBJC-NEXT:     },
// OBJC-NEXT:     {
// OBJC-NEXT:       "kind": "text",
// OBJC-NEXT:       "spelling": ": "
// OBJC-NEXT:     },
// OBJC-NEXT:     {
// OBJC-NEXT:       "kind": "typeIdentifier",
// OBJC-NEXT:       "preciseIdentifier": "[[FooStruct1_USR:.*]]",
// OBJC-NEXT:       "spelling": "FooStruct1"
// OBJC-NEXT:     },
// OBJC-NEXT:     {
// OBJC-NEXT:       "kind": "text",
// OBJC-NEXT:       "spelling": ")"
// OBJC-NEXT:     }
// OBJC-NEXT:   ],
// OBJC:      SYMBOL GRAPH END
// OBJC:      REFERENCED DECLS BEGIN
// OBJC-NEXT: [[FooStruct1_USR]] | public | {{.*}}Foo.h | Foo | User | NonSPI | source.lang.objc
// OBJC-NEXT:   FooStruct1 swift.struct [[FooStruct1_USR]]
// OBJC-NEXT: REFERENCED DECLS END
