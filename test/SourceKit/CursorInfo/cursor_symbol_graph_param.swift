func foo(_ fn: (Int) -> Void) {}

foo { bar in
}

// RUN: %empty-directory(%t)
// RUN:  %sourcekitd-test -req=cursor -pos=1:12 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=CHECK_FN %s
// RUN:  %sourcekitd-test -req=cursor -pos=3:7 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=CHECK_BAR %s

// CHECK_FN: SYMBOL GRAPH BEGIN
// CHECK_FN: {
// CHECK_FN:   "metadata": {
// CHECK_FN:     "formatVersion": {
// CHECK_FN:       "major": 0,
// CHECK_FN:       "minor": 6,
// CHECK_FN:       "patch": 0
// CHECK_FN:     },
// CHECK_FN:     "generator":
// CHECK_FN:   },
// CHECK_FN:   "module": {
// CHECK_FN:     "name": "cursor_symbol_graph_param",
// CHECK_FN:     "platform": {
// CHECK_FN:       "architecture": "arm64",
// CHECK_FN:       "operatingSystem": {
// CHECK_FN:         "minimumVersion": {
// CHECK_FN:           "major": 10,
// CHECK_FN:           "minor": 13
// CHECK_FN:         },
// CHECK_FN:         "name": "macosx"
// CHECK_FN:       },
// CHECK_FN:       "vendor": "apple"
// CHECK_FN:     }
// CHECK_FN:   },
// CHECK_FN:   "relationships": [],
// CHECK_FN:   "symbols": [
// CHECK_FN:     {
// CHECK_FN:       "accessLevel": "private",
// CHECK_FN:       "declarationFragments": [
// CHECK_FN:         {
// CHECK_FN:           "kind": "identifier",
// CHECK_FN:           "spelling": "fn"
// CHECK_FN:         },
// CHECK_FN:         {
// CHECK_FN:           "kind": "text",
// CHECK_FN:           "spelling": ": ("
// CHECK_FN:         },
// CHECK_FN:         {
// CHECK_FN:           "kind": "typeIdentifier",
// CHECK_FN:           "preciseIdentifier": "s:Si",
// CHECK_FN:           "spelling": "Int"
// CHECK_FN:         },
// CHECK_FN:         {
// CHECK_FN:           "kind": "text",
// CHECK_FN:           "spelling": ") -> "
// CHECK_FN:         },
// CHECK_FN:         {
// CHECK_FN:           "kind": "typeIdentifier",
// CHECK_FN:           "preciseIdentifier": "s:s4Voida",
// CHECK_FN:           "spelling": "Void"
// CHECK_FN:         }
// CHECK_FN:       ],
// CHECK_FN:       "identifier": {
// CHECK_FN:         "interfaceLanguage": "swift",
// CHECK_FN:         "precise": "s:25cursor_symbol_graph_param3fooyyySiXEF2fnL_yySiXEvp"
// CHECK_FN:       },
// CHECK_FN:       "kind": {
// CHECK_FN:         "displayName": "Global Variable",
// CHECK_FN:         "identifier": "swift.var"
// CHECK_FN:       },
// CHECK_FN:       "location": {
// CHECK_FN:         "position": {
// CHECK_FN:           "character": 11,
// CHECK_FN:           "line": 0
// CHECK_FN:         },
// CHECK_FN:         "uri": "{{.*}}/cursor_symbol_graph_param.swift"
// CHECK_FN:       },
// CHECK_FN:       "names": {
// CHECK_FN:         "subHeading": [
// CHECK_FN:           {
// CHECK_FN:             "kind": "identifier",
// CHECK_FN:             "spelling": "fn"
// CHECK_FN:           },
// CHECK_FN:           {
// CHECK_FN:             "kind": "text",
// CHECK_FN:             "spelling": ": ("
// CHECK_FN:           },
// CHECK_FN:           {
// CHECK_FN:             "kind": "typeIdentifier",
// CHECK_FN:             "preciseIdentifier": "s:Si",
// CHECK_FN:             "spelling": "Int"
// CHECK_FN:           },
// CHECK_FN:           {
// CHECK_FN:             "kind": "text",
// CHECK_FN:             "spelling": ") -> "
// CHECK_FN:           },
// CHECK_FN:           {
// CHECK_FN:             "kind": "typeIdentifier",
// CHECK_FN:             "preciseIdentifier": "s:s4Voida",
// CHECK_FN:             "spelling": "Void"
// CHECK_FN:           }
// CHECK_FN:         ],
// CHECK_FN:         "title": "fn"
// CHECK_FN:       },
// CHECK_FN:       "pathComponents": [
// CHECK_FN:         "foo(_:)",
// CHECK_FN:         "fn"
// CHECK_FN:       ]
// CHECK_FN:     }
// CHECK_FN:   ]
// CHECK_FN: }
// CHECK_FN: SYMBOL GRAPH END

// CHECK_BAR: SYMBOL GRAPH BEGIN
// CHECK_BAR: {
// CHECK_BAR:   "metadata": {
// CHECK_BAR:     "formatVersion": {
// CHECK_BAR:       "major": 0,
// CHECK_BAR:       "minor": 6,
// CHECK_BAR:       "patch": 0
// CHECK_BAR:     },
// CHECK_BAR:     "generator":
// CHECK_BAR:   },
// CHECK_BAR:   "module": {
// CHECK_BAR:     "name": "cursor_symbol_graph_param",
// CHECK_BAR:     "platform": {
// CHECK_BAR:       "architecture": "arm64",
// CHECK_BAR:       "operatingSystem": {
// CHECK_BAR:         "minimumVersion": {
// CHECK_BAR:           "major": 10,
// CHECK_BAR:           "minor": 13
// CHECK_BAR:         },
// CHECK_BAR:         "name": "macosx"
// CHECK_BAR:       },
// CHECK_BAR:       "vendor": "apple"
// CHECK_BAR:     }
// CHECK_BAR:   },
// CHECK_BAR:   "relationships": [],
// CHECK_BAR:   "symbols": [
// CHECK_BAR:     {
// CHECK_BAR:       "accessLevel": "fileprivate",
// CHECK_BAR:       "declarationFragments": [
// CHECK_BAR:         {
// CHECK_BAR:           "kind": "identifier",
// CHECK_BAR:           "spelling": "bar"
// CHECK_BAR:         },
// CHECK_BAR:         {
// CHECK_BAR:           "kind": "text",
// CHECK_BAR:           "spelling": ": "
// CHECK_BAR:         },
// CHECK_BAR:         {
// CHECK_BAR:           "kind": "typeIdentifier",
// CHECK_BAR:           "preciseIdentifier": "s:Si",
// CHECK_BAR:           "spelling": "Int"
// CHECK_BAR:         }
// CHECK_BAR:       ],
// CHECK_BAR:       "identifier": {
// CHECK_BAR:         "interfaceLanguage": "swift",
// CHECK_BAR:         "precise": "s:25cursor_symbol_graph_paramySiXEfU_3barL_Sivp"
// CHECK_BAR:       },
// CHECK_BAR:       "kind": {
// CHECK_BAR:         "displayName": "Global Variable",
// CHECK_BAR:         "identifier": "swift.var"
// CHECK_BAR:       },
// CHECK_BAR:       "location": {
// CHECK_BAR:         "position": {
// CHECK_BAR:           "character": 6,
// CHECK_BAR:           "line": 2
// CHECK_BAR:         },
// CHECK_BAR:         "uri": "{{.*}}/cursor_symbol_graph_param.swift"
// CHECK_BAR:       },
// CHECK_BAR:       "names": {
// CHECK_BAR:         "subHeading": [
// CHECK_BAR:           {
// CHECK_BAR:             "kind": "identifier",
// CHECK_BAR:             "spelling": "bar"
// CHECK_BAR:           },
// CHECK_BAR:           {
// CHECK_BAR:             "kind": "text",
// CHECK_BAR:             "spelling": ": "
// CHECK_BAR:           },
// CHECK_BAR:           {
// CHECK_BAR:             "kind": "typeIdentifier",
// CHECK_BAR:             "preciseIdentifier": "s:Si",
// CHECK_BAR:             "spelling": "Int"
// CHECK_BAR:           }
// CHECK_BAR:         ],
// CHECK_BAR:         "title": "bar"
// CHECK_BAR:       },
// CHECK_BAR:       "pathComponents": [
// CHECK_BAR:         "bar"
// CHECK_BAR:       ]
// CHECK_BAR:     }
// CHECK_BAR:   ]
// CHECK_BAR: }
// CHECK_BAR: SYMBOL GRAPH END
