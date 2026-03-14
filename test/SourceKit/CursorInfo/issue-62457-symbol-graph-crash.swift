struct AnyError: Swift.Error {
  let error: Swift.Error
}

func test(lhs: AnyError) {
  // RUN: %sourcekitd-test -req=cursor -req-opts=retrieve_symbol_graph=1 -pos=%(line + 1):13 %s -- %s | %FileCheck %s
  lhs.error._code
}

// CHECK: SYMBOL GRAPH BEGIN

// CHECK:      "declarationFragments": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "keyword",
// CHECK-NEXT:     "spelling": "var"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "text",
// CHECK-NEXT:     "spelling": " "
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "identifier",
// CHECK-NEXT:     "spelling": "_code"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "text",
// CHECK-NEXT:     "spelling": ": "
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "typeIdentifier",
// CHECK-NEXT:     "preciseIdentifier": "s:Si",
// CHECK-NEXT:     "spelling": "Int"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "text",
// CHECK-NEXT:     "spelling": " { "
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "keyword",
// CHECK-NEXT:     "spelling": "get"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "text",
// CHECK-NEXT:     "spelling": " }"
// CHECK-NEXT:   }
// CHECK-NEXT: ]

// CHECK: SYMBOL GRAPH END
