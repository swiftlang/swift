/// Something about x
let x = 1
_ = x

/**
 Something about y
 */
private var y: String { return "hello" }
_ = y

/// Something about
/// Foo
struct Foo<T> {
    /** Something about bar */
    func bar(x: T) where T: Equatable {}
}
Foo<Int>().bar(x: 1)
Foo<String>().bar(x: "hello")

extension Foo where T == Int {
    func blah() {
        bar(x: 4)
    }
}

/// MyEnum doc.
enum MyEnum {
    /// Text in a code line with no indicated semantic meaning.
    case someCase
}

// RUN:  %sourcekitd-test -req=cursor -pos=2:5 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=CHECKX %s
// RUN:  %sourcekitd-test -req=cursor -pos=3:5 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=CHECKX %s

// RUN:  %sourcekitd-test -req=cursor -pos=8:13 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=CHECKY %s
// RUN:  %sourcekitd-test -req=cursor -pos=9:5 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=CHECKY %s

// RUN:  %sourcekitd-test -req=cursor -pos=13:8 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=CHECKFOO %s
// RUN:  %sourcekitd-test -req=cursor -pos=17:1 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=CHECKFOO %s
// RUN:  %sourcekitd-test -req=cursor -pos=18:1 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=CHECKFOO %s
// RUN:  %sourcekitd-test -req=cursor -pos=20:11 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=CHECKFOO %s

// RUN:  %sourcekitd-test -req=cursor -pos=15:10 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefixes=CHECKBAR_ALL,CHECKBAR_GEN %s
// RUN:  %sourcekitd-test -req=cursor -pos=17:12 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefixes=CHECKBAR_ALL,CHECKBAR_INT %s
// RUN:  %sourcekitd-test -req=cursor -pos=22:9 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefixes=CHECKBAR_ALL,CHECKBAR_INT %s
// RUN:  %sourcekitd-test -req=cursor -pos=18:15 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefixes=CHECKBAR_ALL,CHECKBAR_STR %s

// RUN:  %sourcekitd-test -req=cursor -pos=29:10 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefixes=CHECKCASE %s


// CHECKX: SYMBOL GRAPH BEGIN
// CHECKX: {
// CHECKX:   "metadata": {
// CHECKX:     "formatVersion": {
// CHECKX:       "major":
// CHECKX:       "minor":
// CHECKX:       "patch":
// CHECKX:     },
// CHECKX:     "generator":
// CHECKX:   },
// CHECKX:   "module": {
// CHECKX:     "name": "cursor_symbol_graph",
// CHECKX:     "platform": {
// CHECKX:       "architecture":
// CHECKX:       "operatingSystem":
// CHECKX:       "vendor":
// CHECKX:     }
// CHECKX:   },
// CHECKX:   "relationships": [],
// CHECKX:   "symbols": [
// CHECKX:     {
// CHECKX:       "accessLevel": "internal",
// CHECKX:       "declarationFragments": [
// CHECKX:         {
// CHECKX:           "kind": "keyword",
// CHECKX:           "spelling": "let"
// CHECKX:         },
// CHECKX:         {
// CHECKX:           "kind": "text",
// CHECKX:           "spelling": " "
// CHECKX:         },
// CHECKX:         {
// CHECKX:           "kind": "identifier",
// CHECKX:           "spelling": "x"
// CHECKX:         },
// CHECKX:         {
// CHECKX:           "kind": "text",
// CHECKX:           "spelling": ": "
// CHECKX:         },
// CHECKX:         {
// CHECKX:           "kind": "typeIdentifier",
// CHECKX:           "preciseIdentifier": "s:Si",
// CHECKX:           "spelling": "Int"
// CHECKX:         }
// CHECKX:       ],
// CHECKX:       "docComment": {
// CHECKX:         "lines": [
// CHECKX:           {
// CHECKX:             "range": {
// CHECKX:               "end": {
// CHECKX:                 "character": 21,
// CHECKX:                 "line": 0
// CHECKX:               },
// CHECKX:               "start": {
// CHECKX:                 "character": 4,
// CHECKX:                 "line": 0
// CHECKX:               }
// CHECKX:             },
// CHECKX:             "text": "Something about x"
// CHECKX:           }
// CHECKX:         ]
// CHECKX:       },
// CHECKX:       "identifier": {
// CHECKX:         "interfaceLanguage": "swift",
// CHECKX:         "precise": "s:19cursor_symbol_graph1xSivp"
// CHECKX:       },
// CHECKX:       "kind": {
// CHECKX:         "displayName": "Global Variable",
// CHECKX:         "identifier": "swift.var"
// CHECKX:       },
// CHECKX:       "location": {
// CHECKX:         "position": {
// CHECKX:           "character": 4,
// CHECKX:           "line": 1
// CHECKX:         },
// CHECKX:         "uri": "{{.*}}cursor_symbol_graph.swift"
// CHECKX:       },
// CHECKX:       "names": {
// CHECKX:         "subHeading": [
// CHECKX:           {
// CHECKX:             "kind": "keyword",
// CHECKX:             "spelling": "let"
// CHECKX:           },
// CHECKX:           {
// CHECKX:             "kind": "text",
// CHECKX:             "spelling": " "
// CHECKX:           },
// CHECKX:           {
// CHECKX:             "kind": "identifier",
// CHECKX:             "spelling": "x"
// CHECKX:           },
// CHECKX:           {
// CHECKX:             "kind": "text",
// CHECKX:             "spelling": ": "
// CHECKX:           },
// CHECKX:           {
// CHECKX:             "kind": "typeIdentifier",
// CHECKX:             "preciseIdentifier": "s:Si",
// CHECKX:             "spelling": "Int"
// CHECKX:           }
// CHECKX:         ],
// CHECKX:         "title": "x"
// CHECKX:       },
// CHECKX:       "pathComponents": [
// CHECKX:         "x"
// CHECKX:       ]
// CHECKX:     }
// CHECKX:   ]
// CHECKX: }
// CHECKX: SYMBOL GRAPH END


// CHECKY: SYMBOL GRAPH BEGIN
// CHECKY: {
// CHECKY:   "metadata": {
// CHECKY:     "formatVersion": {
// CHECKY:       "major":
// CHECKY:       "minor":
// CHECKY:       "patch":
// CHECKY:     },
// CHECKY:     "generator":
// CHECKY:   },
// CHECKY:   "module": {
// CHECKY:     "name": "cursor_symbol_graph",
// CHECKY:     "platform": {
// CHECKY:       "architecture":
// CHECKY:       "operatingSystem":
// CHECKY:       "vendor":
// CHECKY:     }
// CHECKY:   },
// CHECKY:   "relationships": [],
// CHECKY:   "symbols": [
// CHECKY:     {
// CHECKY:       "accessLevel": "private",
// CHECKY:       "declarationFragments": [
// CHECKY:         {
// CHECKY:           "kind": "keyword",
// CHECKY:           "spelling": "private"
// CHECKY:         },
// CHECKY:         {
// CHECKY:           "kind": "text",
// CHECKY:           "spelling": " "
// CHECKY:         },
// CHECKY:         {
// CHECKY:           "kind": "keyword",
// CHECKY:           "spelling": "var"
// CHECKY:         },
// CHECKY:         {
// CHECKY:           "kind": "text",
// CHECKY:           "spelling": " "
// CHECKY:         },
// CHECKY:         {
// CHECKY:           "kind": "identifier",
// CHECKY:           "spelling": "y"
// CHECKY:         },
// CHECKY:         {
// CHECKY:           "kind": "text",
// CHECKY:           "spelling": ": "
// CHECKY:         },
// CHECKY:         {
// CHECKY:           "kind": "typeIdentifier",
// CHECKY:           "preciseIdentifier": "s:SS",
// CHECKY:           "spelling": "String"
// CHECKY:         },
// CHECKY:         {
// CHECKY:           "kind": "text",
// CHECKY:           "spelling": " { "
// CHECKY:         },
// CHECKY:         {
// CHECKY:           "kind": "keyword",
// CHECKY:           "spelling": "get"
// CHECKY:         },
// CHECKY:         {
// CHECKY:           "kind": "text",
// CHECKY:           "spelling": " }"
// CHECKY:         }
// CHECKY:       ],
// CHECKY:       "docComment": {
// CHECKY:         "lines": [
// CHECKY:           {
// CHECKY:             "range": {
// CHECKY:               "end": {
// CHECKY:                 "character": 18,
// CHECKY:                 "line": 5
// CHECKY:               },
// CHECKY:               "start": {
// CHECKY:                 "character": 1,
// CHECKY:                 "line": 5
// CHECKY:               }
// CHECKY:             },
// CHECKY:             "text": "Something about y"
// CHECKY:           },
// CHECKY:           {
// CHECKY:             "range": {
// CHECKY:               "end": {
// CHECKY:                 "character": 1,
// CHECKY:                 "line": 6
// CHECKY:               },
// CHECKY:               "start": {
// CHECKY:                 "character": 1,
// CHECKY:                 "line": 6
// CHECKY:               }
// CHECKY:             },
// CHECKY:             "text": ""
// CHECKY:           }
// CHECKY:         ]
// CHECKY:       },
// CHECKY:       "identifier": {
// CHECKY:         "interfaceLanguage": "swift",
// CHECKY:         "precise": "s:19cursor_symbol_graph1y33_153B1E8D9396FFABCE21DE5D3EC1833ALLSSvp"
// CHECKY:       },
// CHECKY:       "kind": {
// CHECKY:         "displayName": "Global Variable",
// CHECKY:         "identifier": "swift.var"
// CHECKY:       },
// CHECKY:       "location": {
// CHECKY:         "position": {
// CHECKY:           "character": 12,
// CHECKY:           "line": 7
// CHECKY:         },
// CHECKY:         "uri": "{{.*}}cursor_symbol_graph.swift"
// CHECKY:       },
// CHECKY:       "names": {
// CHECKY:         "subHeading": [
// CHECKY:           {
// CHECKY:             "kind": "keyword",
// CHECKY:             "spelling": "var"
// CHECKY:           },
// CHECKY:           {
// CHECKY:             "kind": "text",
// CHECKY:             "spelling": " "
// CHECKY:           },
// CHECKY:           {
// CHECKY:             "kind": "identifier",
// CHECKY:             "spelling": "y"
// CHECKY:           },
// CHECKY:           {
// CHECKY:             "kind": "text",
// CHECKY:             "spelling": ": "
// CHECKY:           },
// CHECKY:           {
// CHECKY:             "kind": "typeIdentifier",
// CHECKY:             "preciseIdentifier": "s:SS",
// CHECKY:             "spelling": "String"
// CHECKY:           }
// CHECKY:         ],
// CHECKY:         "title": "y"
// CHECKY:       },
// CHECKY:       "pathComponents": [
// CHECKY:         "y"
// CHECKY:       ]
// CHECKY:     }
// CHECKY:   ]
// CHECKY: }
// CHECKY: SYMBOL GRAPH END


// CHECKFOO: SYMBOL GRAPH BEGIN
// CHECKFOO: {
// CHECKFOO:   "metadata": {
// CHECKFOO:     "formatVersion": {
// CHECKFOO:       "major":
// CHECKFOO:       "minor":
// CHECKFOO:       "patch":
// CHECKFOO:     },
// CHECKFOO:     "generator":
// CHECKFOO:   },
// CHECKFOO:   "module": {
// CHECKFOO:     "name": "cursor_symbol_graph",
// CHECKFOO:     "platform": {
// CHECKFOO:       "architecture":
// CHECKFOO:       "operatingSystem":
// CHECKFOO:       "vendor":
// CHECKFOO:     }
// CHECKFOO:   },
// CHECKFOO:   "relationships": [
// CHECKFOO:       "target": "s:s8SendableP",
// CHECKFOO:   "symbols": [
// CHECKFOO:     {
// CHECKFOO:       "accessLevel": "internal",
// CHECKFOO:       "declarationFragments": [
// CHECKFOO:         {
// CHECKFOO:           "kind": "keyword",
// CHECKFOO:           "spelling": "struct"
// CHECKFOO:         },
// CHECKFOO:         {
// CHECKFOO:           "kind": "text",
// CHECKFOO:           "spelling": " "
// CHECKFOO:         },
// CHECKFOO:         {
// CHECKFOO:           "kind": "identifier",
// CHECKFOO:           "spelling": "Foo"
// CHECKFOO:         },
// CHECKFOO:         {
// CHECKFOO:           "kind": "text",
// CHECKFOO:           "spelling": "<"
// CHECKFOO:         },
// CHECKFOO:         {
// CHECKFOO:           "kind": "genericParameter",
// CHECKFOO:           "spelling": "T"
// CHECKFOO:         },
// CHECKFOO:         {
// CHECKFOO:           "kind": "text",
// CHECKFOO:           "spelling": ">"
// CHECKFOO:         }
// CHECKFOO:       ],
// CHECKFOO:       "docComment": {
// CHECKFOO:         "lines": [
// CHECKFOO:           {
// CHECKFOO:             "range": {
// CHECKFOO:               "end": {
// CHECKFOO:                 "character": 19,
// CHECKFOO:                 "line": 10
// CHECKFOO:               },
// CHECKFOO:               "start": {
// CHECKFOO:                 "character": 4,
// CHECKFOO:                 "line": 10
// CHECKFOO:               }
// CHECKFOO:             },
// CHECKFOO:             "text": "Something about"
// CHECKFOO:           },
// CHECKFOO:           {
// CHECKFOO:             "range": {
// CHECKFOO:               "end": {
// CHECKFOO:                 "character": 7,
// CHECKFOO:                 "line": 11
// CHECKFOO:               },
// CHECKFOO:               "start": {
// CHECKFOO:                 "character": 4,
// CHECKFOO:                 "line": 11
// CHECKFOO:               }
// CHECKFOO:             },
// CHECKFOO:             "text": "Foo"
// CHECKFOO:           }
// CHECKFOO:         ]
// CHECKFOO:       },
// CHECKFOO:       "identifier": {
// CHECKFOO:         "interfaceLanguage": "swift",
// CHECKFOO:         "precise": "s:19cursor_symbol_graph3FooV"
// CHECKFOO:       },
// CHECKFOO:       "kind": {
// CHECKFOO:         "displayName": "Structure",
// CHECKFOO:         "identifier": "swift.struct"
// CHECKFOO:       },
// CHECKFOO:       "location": {
// CHECKFOO:         "position": {
// CHECKFOO:           "character": 7,
// CHECKFOO:           "line": 12
// CHECKFOO:         },
// CHECKFOO:         "uri": "{{.*}}cursor_symbol_graph.swift"
// CHECKFOO:       },
// CHECKFOO:       "names": {
// CHECKFOO:         "navigator": [
// CHECKFOO:           {
// CHECKFOO:             "kind": "identifier",
// CHECKFOO:             "spelling": "Foo"
// CHECKFOO:           }
// CHECKFOO:         ],
// CHECKFOO:         "subHeading": [
// CHECKFOO:           {
// CHECKFOO:             "kind": "keyword",
// CHECKFOO:             "spelling": "struct"
// CHECKFOO:           },
// CHECKFOO:           {
// CHECKFOO:             "kind": "text",
// CHECKFOO:             "spelling": " "
// CHECKFOO:           },
// CHECKFOO:           {
// CHECKFOO:             "kind": "identifier",
// CHECKFOO:             "spelling": "Foo"
// CHECKFOO:           }
// CHECKFOO:         ],
// CHECKFOO:         "title": "Foo"
// CHECKFOO:       },
// CHECKFOO:       "pathComponents": [
// CHECKFOO:         "Foo"
// CHECKFOO:       ],
// CHECKFOO:       "swiftGenerics": {
// CHECKFOO:         "parameters": [
// CHECKFOO:           {
// CHECKFOO:             "depth": 0,
// CHECKFOO:             "index": 0,
// CHECKFOO:             "name": "T"
// CHECKFOO:           }
// CHECKFOO:         ]
// CHECKFOO:       }
// CHECKFOO:     }
// CHECKFOO:   ]
// CHECKFOO: }


// CHECKBAR_ALL: SYMBOL GRAPH BEGIN
// CHECKBAR_ALL: {
// CHECKBAR_ALL:   "metadata": {
// CHECKBAR_ALL:     "formatVersion": {
// CHECKBAR_ALL:       "major":
// CHECKBAR_ALL:       "minor":
// CHECKBAR_ALL:       "patch":
// CHECKBAR_ALL:     },
// CHECKBAR_ALL:     "generator":
// CHECKBAR_ALL:   },
// CHECKBAR_ALL:   "module": {
// CHECKBAR_ALL:     "name": "cursor_symbol_graph",
// CHECKBAR_ALL:     "platform": {
// CHECKBAR_ALL:       "architecture":
// CHECKBAR_ALL:       "operatingSystem":
// CHECKBAR_ALL:       "vendor":
// CHECKBAR_ALL:     }
// CHECKBAR_ALL:   },
// CHECKBAR_ALL:   "relationships": [
// CHECKBAR_ALL:     {
// CHECKBAR_ALL:       "kind": "memberOf",
// CHECKBAR_ALL:       "source": "s:19cursor_symbol_graph3FooV3bar1xyx_tSQRzlF",
// CHECKBAR_ALL:       "target": "s:19cursor_symbol_graph3FooV"
// CHECKBAR_ALL:     }
// CHECKBAR_ALL:   ],
// CHECKBAR_ALL:   "symbols": [
// CHECKBAR_ALL:     {
// CHECKBAR_ALL:       "accessLevel": "internal",
// CHECKBAR_ALL:       "declarationFragments": [
// CHECKBAR_ALL:         {
// CHECKBAR_ALL:           "kind": "keyword",
// CHECKBAR_ALL:           "spelling": "func"
// CHECKBAR_ALL:         },
// CHECKBAR_ALL:         {
// CHECKBAR_ALL:           "kind": "text",
// CHECKBAR_ALL:           "spelling": " "
// CHECKBAR_ALL:         },
// CHECKBAR_ALL:         {
// CHECKBAR_ALL:           "kind": "identifier",
// CHECKBAR_ALL:           "spelling": "bar"
// CHECKBAR_ALL:         },
// CHECKBAR_ALL:         {
// CHECKBAR_ALL:           "kind": "text",
// CHECKBAR_ALL:           "spelling": "("
// CHECKBAR_ALL:         },
// CHECKBAR_ALL:         {
// CHECKBAR_ALL:           "kind": "externalParam",
// CHECKBAR_ALL:           "spelling": "x"
// CHECKBAR_ALL:         },
// CHECKBAR_ALL:         {
// CHECKBAR_ALL:           "kind": "text",
// CHECKBAR_ALL:           "spelling": ": "
// CHECKBAR_ALL:         },
// CHECKBAR_ALL:         {
// CHECKBAR_ALL:           "kind": "typeIdentifier",
// CHECKBAR_GEN:           "spelling": "T"
// CHECKBAR_INT:           "preciseIdentifier": "s:Si",
// CHECKBAR_INT:           "spelling": "Int"
// CHECKBAR_STR:           "preciseIdentifier": "s:SS",
// CHECKBAR_STR:           "spelling": "String"
// CHECKBAR_ALL:         },
// CHECKBAR_ALL:         {
// CHECKBAR_ALL:           "kind": "text",
// CHECKBAR_GEN:           "spelling": ") "
// CHECKBAR_INT:           "spelling": ")"
// CHECKBAR_STR:           "spelling": ")"
// CHECKBAR_INT-NOT:       where
// CHECKBAR_STR-NOT:       where
// CHECKBAR_GEN:         },
// CHECKBAR_GEN:         {
// CHECKBAR_GEN:           "kind": "keyword",
// CHECKBAR_GEN:           "spelling": "where"
// CHECKBAR_GEN:         },
// CHECKBAR_GEN:         {
// CHECKBAR_GEN:           "kind": "text",
// CHECKBAR_GEN:           "spelling": " "
// CHECKBAR_GEN:         },
// CHECKBAR_GEN:         {
// CHECKBAR_GEN:           "kind": "typeIdentifier",
// CHECKBAR_GEN:           "spelling": "T"
// CHECKBAR_GEN:         },
// CHECKBAR_GEN:         {
// CHECKBAR_GEN:           "kind": "text",
// CHECKBAR_GEN:           "spelling": " : "
// CHECKBAR_GEN:         },
// CHECKBAR_GEN:         {
// CHECKBAR_GEN:           "kind": "typeIdentifier",
// CHECKBAR_GEN:           "preciseIdentifier": "s:SQ",
// CHECKBAR_GEN:           "spelling": "Equatable"
// CHECKBAR_ALL:         }
// CHECKBAR_ALL:       ],
// CHECKBAR_ALL:       "docComment": {
// CHECKBAR_ALL:         "lines": [
// CHECKBAR_ALL:           {
// CHECKBAR_ALL:             "range": {
// CHECKBAR_ALL:               "end": {
// CHECKBAR_ALL:                 "character": 28,
// CHECKBAR_ALL:                 "line": 13
// CHECKBAR_ALL:               },
// CHECKBAR_ALL:               "start": {
// CHECKBAR_ALL:                 "character": 8,
// CHECKBAR_ALL:                 "line": 13
// CHECKBAR_ALL:               }
// CHECKBAR_ALL:             },
// CHECKBAR_ALL:             "text": "Something about bar "
// CHECKBAR_ALL:           }
// CHECKBAR_ALL:         ]
// CHECKBAR_ALL:       },
// CHECKBAR_ALL:       "functionSignature": {
// CHECKBAR_ALL:         "parameters": [
// CHECKBAR_ALL:           {
// CHECKBAR_ALL:             "declarationFragments": [
// CHECKBAR_ALL:               {
// CHECKBAR_ALL:                 "kind": "identifier",
// CHECKBAR_ALL:                 "spelling": "x"
// CHECKBAR_ALL:               },
// CHECKBAR_ALL:               {
// CHECKBAR_ALL:                 "kind": "text",
// CHECKBAR_ALL:                 "spelling": ": "
// CHECKBAR_ALL:               },
// CHECKBAR_ALL:               {
// CHECKBAR_ALL:                 "kind": "typeIdentifier",
// CHECKBAR_GEN:                 "spelling": "T"
// CHECKBAR_INT:                 "preciseIdentifier": "s:Si",
// CHECKBAR_INT:                 "spelling": "Int"
// CHECKBAR_STR:                 "preciseIdentifier": "s:SS",
// CHECKBAR_STR:                 "spelling": "String"
// CHECKBAR_ALL:               }
// CHECKBAR_ALL:             ],
// CHECKBAR_ALL:             "name": "x"
// CHECKBAR_ALL:           }
// CHECKBAR_ALL:         ],
// CHECKBAR_ALL:         "returns": [
// CHECKBAR_ALL:           {
// CHECKBAR_ALL:             "kind": "text",
// CHECKBAR_ALL:             "spelling": "()"
// CHECKBAR_ALL:           }
// CHECKBAR_ALL:         ]
// CHECKBAR_ALL:       },
// CHECKBAR_ALL:       "identifier": {
// CHECKBAR_ALL:         "interfaceLanguage": "swift",
// CHECKBAR_ALL:         "precise": "s:19cursor_symbol_graph3FooV3bar1xyx_tSQRzlF"
// CHECKBAR_ALL:       },
// CHECKBAR_ALL:       "kind": {
// CHECKBAR_ALL:         "displayName": "Instance Method",
// CHECKBAR_ALL:         "identifier": "swift.method"
// CHECKBAR_ALL:       },
// CHECKBAR_ALL:       "location": {
// CHECKBAR_ALL:         "position": {
// CHECKBAR_ALL:           "character": 9,
// CHECKBAR_ALL:           "line": 14
// CHECKBAR_ALL:         },
// CHECKBAR_ALL:         "uri": "{{.*}}cursor_symbol_graph.swift"
// CHECKBAR_ALL:       },
// CHECKBAR_ALL:       "names": {
// CHECKBAR_ALL:         "subHeading": [
// CHECKBAR_ALL:           {
// CHECKBAR_ALL:             "kind": "keyword",
// CHECKBAR_ALL:             "spelling": "func"
// CHECKBAR_ALL:           },
// CHECKBAR_ALL:           {
// CHECKBAR_ALL:             "kind": "text",
// CHECKBAR_ALL:             "spelling": " "
// CHECKBAR_ALL:           },
// CHECKBAR_ALL:           {
// CHECKBAR_ALL:             "kind": "identifier",
// CHECKBAR_ALL:             "spelling": "bar"
// CHECKBAR_ALL:           },
// CHECKBAR_ALL:           {
// CHECKBAR_ALL:             "kind": "text",
// CHECKBAR_ALL:             "spelling": "("
// CHECKBAR_ALL:           },
// CHECKBAR_ALL:           {
// CHECKBAR_ALL:             "kind": "externalParam",
// CHECKBAR_ALL:             "spelling": "x"
// CHECKBAR_ALL:           },
// CHECKBAR_ALL:           {
// CHECKBAR_ALL:             "kind": "text",
// CHECKBAR_ALL:             "spelling": ": "
// CHECKBAR_ALL:           },
// CHECKBAR_ALL:           {
// CHECKBAR_ALL:             "kind": "typeIdentifier",
// CHECKBAR_GEN:             "spelling": "T"
// CHECKBAR_INT:             "preciseIdentifier": "s:Si",
// CHECKBAR_INT:             "spelling": "Int"
// CHECKBAR_STR:             "preciseIdentifier": "s:SS",
// CHECKBAR_STR:             "spelling": "String"
// CHECKBAR_ALL:           },
// CHECKBAR_ALL:           {
// CHECKBAR_ALL:             "kind": "text",
// CHECKBAR_ALL:             "spelling": ")"
// CHECKBAR_ALL:           }
// CHECKBAR_ALL:         ],
// CHECKBAR_ALL:         "title": "bar(x:)"
// CHECKBAR_ALL:       },
// CHECKBAR_ALL:       "pathComponents": [
// CHECKBAR_ALL:         "Foo",
// CHECKBAR_ALL:         "bar(x:)"
// CHECKBAR_GEN:       ],
// CHECKBAR_INT-NOT:   "swiftGenerics":
// CHECKBAR_STR-NOT:   "swiftGenerics":
// CHECKBAR_GEN:       "swiftGenerics": {
// CHECKBAR_GEN:         "constraints": [
// CHECKBAR_GEN:           {
// CHECKBAR_GEN:             "kind": "conformance",
// CHECKBAR_GEN:             "lhs": "T",
// CHECKBAR_GEN:             "rhs": "Equatable"
// CHECKBAR_GEN:           }
// CHECKBAR_GEN:         ],
// CHECKBAR_GEN:         "parameters": [
// CHECKBAR_GEN:           {
// CHECKBAR_GEN:             "depth": 0,
// CHECKBAR_GEN:             "index": 0,
// CHECKBAR_GEN:             "name": "T"
// CHECKBAR_GEN:           }
// CHECKBAR_GEN:         ]
// CHECKBAR_GEN:       }
// CHECKBAR_ALL:     }
// CHECKBAR_ALL:   ]
// CHECKBAR_ALL: }


// CHECKCASE: SYMBOL GRAPH BEGIN
// CHECKCASE: {
// CHECKCASE:   "metadata": {
// CHECKCASE:     "formatVersion": {
// CHECKCASE:       "major":
// CHECKCASE:       "minor":
// CHECKCASE:       "patch":
// CHECKCASE:     },
// CHECKCASE:     "generator":
// CHECKCASE:   },
// CHECKCASE:   "module": {
// CHECKCASE:     "name": "cursor_symbol_graph",
// CHECKCASE:     "platform":
// CHECKCASE:   },
// CHECKCASE:   "relationships": [
// CHECKCASE:     {
// CHECKCASE:       "kind": "memberOf",
// CHECKCASE:       "source": "s:19cursor_symbol_graph6MyEnumO8someCaseyA2CmF",
// CHECKCASE:       "target": "s:19cursor_symbol_graph6MyEnumO"
// CHECKCASE:     }
// CHECKCASE:   ],
// CHECKCASE:   "symbols": [
// CHECKCASE:     {
// CHECKCASE:       "accessLevel": "internal",
// CHECKCASE:       "declarationFragments": [
// CHECKCASE:         {
// CHECKCASE:           "kind": "keyword",
// CHECKCASE:           "spelling": "case"
// CHECKCASE:         },
// CHECKCASE:         {
// CHECKCASE:           "kind": "text",
// CHECKCASE:           "spelling": " "
// CHECKCASE:         },
// CHECKCASE:         {
// CHECKCASE:           "kind": "identifier",
// CHECKCASE:           "spelling": "someCase"
// CHECKCASE:         }
// CHECKCASE:       ],
// CHECKCASE:       "docComment": {
// CHECKCASE:         "lines": [
// CHECKCASE:           {
// CHECKCASE:             "range": {
// CHECKCASE:               "end": {
// CHECKCASE:                 "character": 63,
// CHECKCASE:                 "line": 27
// CHECKCASE:               },
// CHECKCASE:               "start": {
// CHECKCASE:                 "character": 8,
// CHECKCASE:                 "line": 27
// CHECKCASE:               }
// CHECKCASE:             },
// CHECKCASE:             "text": "Text in a code line with no indicated semantic meaning."
// CHECKCASE:           }
// CHECKCASE:         ]
// CHECKCASE:       },
// CHECKCASE:       "identifier": {
// CHECKCASE:         "interfaceLanguage": "swift",
// CHECKCASE:         "precise": "s:19cursor_symbol_graph6MyEnumO8someCaseyA2CmF"
// CHECKCASE:       },
// CHECKCASE:       "kind": {
// CHECKCASE:         "displayName": "Case",
// CHECKCASE:         "identifier": "swift.enum.case"
// CHECKCASE:       },
// CHECKCASE:       "location": {
// CHECKCASE:         "position": {
// CHECKCASE:           "character": 9,
// CHECKCASE:           "line": 28
// CHECKCASE:         },
// CHECKCASE:         "uri": "{{.*}}cursor_symbol_graph.swift"
// CHECKCASE:       },
// CHECKCASE:       "names": {
// CHECKCASE:         "subHeading": [
// CHECKCASE:           {
// CHECKCASE:             "kind": "keyword",
// CHECKCASE:             "spelling": "case"
// CHECKCASE:           },
// CHECKCASE:           {
// CHECKCASE:             "kind": "text",
// CHECKCASE:             "spelling": " "
// CHECKCASE:           },
// CHECKCASE:           {
// CHECKCASE:             "kind": "identifier",
// CHECKCASE:             "spelling": "someCase"
// CHECKCASE:           }
// CHECKCASE:         ],
// CHECKCASE:         "title": "MyEnum.someCase"
// CHECKCASE:       },
// CHECKCASE:       "pathComponents": [
// CHECKCASE:         "MyEnum",
// CHECKCASE:         "someCase"
// CHECKCASE:       ]
// CHECKCASE:     }
// CHECKCASE:   ]
// CHECKCASE: }
// CHECKCASE: SYMBOL GRAPH END
