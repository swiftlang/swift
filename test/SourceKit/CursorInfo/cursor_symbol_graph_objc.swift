import Foo

func callObjC() {
  fooFuncWithComment5()
}

// REQUIRES: objc_interop
// RUN: %sourcekitd-test -req=cursor -pos=4:3 -req-opts=retrieve_symbol_graph=1 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp -target %target-triple %s | %FileCheck %s
//
// CHECK: SYMBOL GRAPH BEGIN
// CHECK: {
// CHECK:   "metadata": {
// CHECK:     "formatVersion": {
// CHECK:       "major":
// CHECK:       "minor":
// CHECK:       "patch":
// CHECK:     },
// CHECK:     "generator":
// CHECK:   },
// CHECK:   "module": {
// CHECK:     "name": "Foo",
// CHECK:     "platform": {
// CHECK-NOT:   "architecture": "",
// CHECK:     }
// CHECK:   },
// CHECK:   "relationships": [],
// CHECK:   "symbols": [
// CHECK:     {
// CHECK:       "accessLevel": "public",
// CHECK:       "declarationFragments": [
// CHECK:         {
// CHECK:           "kind": "keyword",
// CHECK:           "spelling": "func"
// CHECK:         },
// CHECK:         {
// CHECK:           "kind": "text",
// CHECK:           "spelling": " "
// CHECK:         },
// CHECK:         {
// CHECK:           "kind": "identifier",
// CHECK:           "spelling": "fooFuncWithComment5"
// CHECK:         },
// CHECK:         {
// CHECK:           "kind": "text",
// CHECK:           "spelling": "()"
// CHECK:         }
// CHECK:       ],
// CHECK:       "functionSignature": {
// CHECK:         "returns": [
// CHECK:           {
// CHECK:             "kind": "typeIdentifier",
// CHECK:             "preciseIdentifier": "s:s4Voida",
// CHECK:             "spelling": "Void"
// CHECK:           }
// CHECK:         ]
// CHECK:       },
// CHECK:       "identifier": {
// CHECK:         "interfaceLanguage": "swift",
// CHECK:         "precise": "c:@F@fooFuncWithComment5"
// CHECK:       },
// CHECK:       "kind": {
// CHECK:         "displayName": "Function",
// CHECK:         "identifier": "swift.func"
// CHECK:       },
// CHECK:       "names": {
// CHECK:         "subHeading": [
// CHECK:           {
// CHECK:             "kind": "keyword",
// CHECK:             "spelling": "func"
// CHECK:           },
// CHECK:           {
// CHECK:             "kind": "text",
// CHECK:             "spelling": " "
// CHECK:           },
// CHECK:           {
// CHECK:             "kind": "identifier",
// CHECK:             "spelling": "fooFuncWithComment5"
// CHECK:           },
// CHECK:           {
// CHECK:             "kind": "text",
// CHECK:             "spelling": "()"
// CHECK:           }
// CHECK:         ],
// CHECK:         "title": "fooFuncWithComment5()"
// CHECK:       },
// CHECK:       "pathComponents": [
// CHECK:         "fooFuncWithComment5()"
// CHECK:       ]
// CHECK:     }
// CHECK:   ]
// CHECK: }
// CHECK: SYMBOL GRAPH END
