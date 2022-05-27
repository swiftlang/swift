// REQUIRES: objc_interop

// TODO: Add some way to specify extra options to symbolgraph-extract and then
//       split this test into parts under the SymbolGraph directory. The SK
//       tests should just check that we run the generation on 1. the correct
//       symbol and 2. with the correct options.

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

//--- use.swift
import MyMod

func test(s: ObjCStruct) {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):3 -req-opts=retrieve_symbol_graph=1 %t/use.swift -- -I %t/mod -target %target-triple %t/use.swift | %FileCheck -check-prefix=CHECK-FUNC %s
  someFunc()

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):3 -req-opts=retrieve_symbol_graph=1 %t/use.swift -- -I %t/mod -target %target-triple %t/use.swift | %FileCheck -check-prefix=CHECK-DIRECTIVE-FUNC %s
  funcUnderDirective()

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):9 -req-opts=retrieve_symbol_graph=1 %t/use.swift -- -I %t/mod -target %target-triple %t/use.swift | %FileCheck -check-prefix=CHECK-NO %s
  _ = s.noDoc

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):9 -req-opts=retrieve_symbol_graph=1 %t/use.swift -- -I %t/mod -target %target-triple %t/use.swift | %FileCheck -check-prefix=CHECK-REG %s
  _ = s.regComments

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):9 -req-opts=retrieve_symbol_graph=1 %t/use.swift -- -I %t/mod -target %target-triple %t/use.swift | %FileCheck -check-prefix=CHECK-SINGLE1 %s
  _ = s.simpleSingle1

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):9 -req-opts=retrieve_symbol_graph=1 %t/use.swift -- -I %t/mod -target %target-triple %t/use.swift | %FileCheck -check-prefix=CHECK-SINGLE2 %s
  _ = s.simpleSingle2

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):9 -req-opts=retrieve_symbol_graph=1 %t/use.swift -- -I %t/mod -target %target-triple %t/use.swift | %FileCheck -check-prefix=CHECK-BLOCK1 %s
  _ = s.simpleBlock1

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):9 -req-opts=retrieve_symbol_graph=1 %t/use.swift -- -I %t/mod -target %target-triple %t/use.swift | %FileCheck -check-prefix=CHECK-BLOCK2 %s
  _ = s.simpleBlock2

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):9 -req-opts=retrieve_symbol_graph=1 %t/use.swift -- -I %t/mod -target %target-triple %t/use.swift | %FileCheck -check-prefix=CHECK-ART %s
  _ = s.artBlock

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):9 -req-opts=retrieve_symbol_graph=1 %t/use.swift -- -I %t/mod -target %target-triple %t/use.swift | %FileCheck -check-prefix=CHECK-MIXED-TYPE %s
  _ = s.mixedType

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):9 -req-opts=retrieve_symbol_graph=1 %t/use.swift -- -I %t/mod -target %target-triple %t/use.swift | %FileCheck -check-prefix=CHECK-MIXED-DOC %s
  _ = s.mixedDoc
}

// CHECK-FUNC: SYMBOL GRAPH BEGIN
// CHECK-FUNC: {
// CHECK-FUNC:   "metadata": {
// CHECK-FUNC:     "formatVersion": {
// CHECK-FUNC:       "major":
// CHECK-FUNC:       "minor":
// CHECK-FUNC:       "patch":
// CHECK-FUNC:     },
// CHECK-FUNC:     "generator":
// CHECK-FUNC:   },
// CHECK-FUNC:   "module": {
// CHECK-FUNC:     "name": "MyMod",
// CHECK-FUNC:     "platform": {
// CHECK-FUNC-NOT:   "architecture": "",
// CHECK-FUNC:     }
// CHECK-FUNC:   },
// CHECK-FUNC:   "relationships": [],
// CHECK-FUNC:   "symbols": [
// CHECK-FUNC:     {
// CHECK-FUNC:       "accessLevel": "public",
// CHECK-FUNC:       "declarationFragments": [
// CHECK-FUNC:         {
// CHECK-FUNC:           "kind": "keyword",
// CHECK-FUNC:           "spelling": "func"
// CHECK-FUNC:         },
// CHECK-FUNC:         {
// CHECK-FUNC:           "kind": "text",
// CHECK-FUNC:           "spelling": " "
// CHECK-FUNC:         },
// CHECK-FUNC:         {
// CHECK-FUNC:           "kind": "identifier",
// CHECK-FUNC:           "spelling": "someFunc"
// CHECK-FUNC:         },
// CHECK-FUNC:         {
// CHECK-FUNC:           "kind": "text",
// CHECK-FUNC:           "spelling": "()"
// CHECK-FUNC:         }
// CHECK-FUNC:       ],
// CHECK-FUNC:       "docComment": {
// CHECK-FUNC:         "lines": [
// CHECK-FUNC:           {
// CHECK-FUNC:             "text": "someFunc doc"
// CHECK-FUNC:           }
// CHECK-FUNC:         ],
// CHECK-FUNC:         "module": "MyMod",
// CHECK-FUNC:         "uri": "file://{{.*}}mod{{\\\\|/}}M.h"
// CHECK-FUNC:       },
// CHECK-FUNC:       "functionSignature": {
// CHECK-FUNC:         "returns": [
// CHECK-FUNC:           {
// CHECK-FUNC:             "kind": "typeIdentifier",
// CHECK-FUNC:             "preciseIdentifier": "s:s4Voida",
// CHECK-FUNC:             "spelling": "Void"
// CHECK-FUNC:           }
// CHECK-FUNC:         ]
// CHECK-FUNC:       },
// CHECK-FUNC:       "identifier": {
// CHECK-FUNC:         "interfaceLanguage": "swift",
// CHECK-FUNC:         "precise": "c:@F@someFunc"
// CHECK-FUNC:       },
// CHECK-FUNC:       "kind": {
// CHECK-FUNC:         "displayName": "Function",
// CHECK-FUNC:         "identifier": "swift.func"
// CHECK-FUNC:       },
// CHECK-FUNC:       "location": {
// CHECK-FUNC:         "uri": "file://{{.*}}mod{{\\\\|/}}M.h"
// CHECK-FUNC:       },
// CHECK-FUNC:       "names": {
// CHECK-FUNC:         "subHeading": [
// CHECK-FUNC:           {
// CHECK-FUNC:             "kind": "keyword",
// CHECK-FUNC:             "spelling": "func"
// CHECK-FUNC:           },
// CHECK-FUNC:           {
// CHECK-FUNC:             "kind": "text",
// CHECK-FUNC:             "spelling": " "
// CHECK-FUNC:           },
// CHECK-FUNC:           {
// CHECK-FUNC:             "kind": "identifier",
// CHECK-FUNC:             "spelling": "someFunc"
// CHECK-FUNC:           },
// CHECK-FUNC:           {
// CHECK-FUNC:             "kind": "text",
// CHECK-FUNC:             "spelling": "()"
// CHECK-FUNC:           }
// CHECK-FUNC:         ],
// CHECK-FUNC:         "title": "someFunc()"
// CHECK-FUNC:       },
// CHECK-FUNC:       "pathComponents": [
// CHECK-FUNC:         "someFunc()"
// CHECK-FUNC:       ]
// CHECK-FUNC:     }
// CHECK-FUNC:   ]
// CHECK-FUNC: }
// CHECK-FUNC: SYMBOL GRAPH END

//--- mod/module.modulemap
module MyMod {
  header "M.h"
}

//--- mod/M.h
/// someFunc doc
void someFunc(void);

struct ObjCStruct {
  int noDoc;
  // CHECK-NO-NOT: "docComment"

  // regular comment
  int regComments;
  // CHECK-REG-NOT: "docComment"

  /// single line doc
  int simpleSingle1;
  // CHECK-SINGLE1: "docComment": {
  // CHECK-SINGLE1-NEXT:    "lines": [
  // CHECK-SINGLE1-NEXT:      {
  // CHECK-SINGLE1-NEXT:        "text": "single line doc"
  // CHECK-SINGLE1-NEXT:      }
  // CHECK-SINGLE1-NEXT:    ],
  // CHECK-SINGLE1-NEXT:    "module": "MyMod",
  // CHECK-SINGLE1-NEXT:    "uri": "file://{{.*}}mod{{\\\\|/}}M.h"
  // CHECK-SINGLE1-NEXT:  }

  //! single line doc
  int simpleSingle2;
  // CHECK-SINGLE2: "docComment": {
  // CHECK-SINGLE2-NEXT:    "lines": [
  // CHECK-SINGLE2-NEXT:      {
  // CHECK-SINGLE2-NEXT:        "text": "single line doc"
  // CHECK-SINGLE2-NEXT:      }
  // CHECK-SINGLE2-NEXT:    ],
  // CHECK-SINGLE2-NEXT:    "module": "MyMod",
  // CHECK-SINGLE2-NEXT:    "uri": "file://{{.*}}mod{{\\\\|/}}M.h"
  // CHECK-SINGLE2-NEXT:  }

  /** single line block doc */
  int simpleBlock1;
  // CHECK-BLOCK1: "docComment": {
  // CHECK-BLOCK1-NEXT:    "lines": [
  // CHECK-BLOCK1-NEXT:      {
  // CHECK-BLOCK1-NEXT:        "text": "single line block doc "
  // CHECK-BLOCK1-NEXT:      }
  // CHECK-BLOCK1-NEXT:    ],
  // CHECK-BLOCK1-NEXT:    "module": "MyMod",
  // CHECK-BLOCK1-NEXT:    "uri": "file://{{.*}}mod{{\\\\|/}}M.h"
  // CHECK-BLOCK1-NEXT:  }

  /*! single line block doc */
  int simpleBlock2;
  // CHECK-BLOCK2: "docComment": {
  // CHECK-BLOCK2-NEXT:    "lines": [
  // CHECK-BLOCK2-NEXT:      {
  // CHECK-BLOCK2-NEXT:        "text": "single line block doc "
  // CHECK-BLOCK2-NEXT:      }
  // CHECK-BLOCK2-NEXT:    ],
  // CHECK-BLOCK2-NEXT:    "module": "MyMod",
  // CHECK-BLOCK2-NEXT:    "uri": "file://{{.*}}mod{{\\\\|/}}M.h"
  // CHECK-BLOCK2-NEXT:  }

  /**
   * docs with art prefix
   *   indent
   * last
   */
  int artBlock;
  // CHECK-ART: "docComment": {
  // CHECK-ART-NEXT:    "lines": [
  // CHECK-ART-NEXT:      {
  // CHECK-ART-NEXT:        "text": ""
  // CHECK-ART-NEXT:      },
  // CHECK-ART-NEXT:      {
  // CHECK-ART-NEXT:        "text": " docs with art prefix"
  // CHECK-ART-NEXT:      },
  // CHECK-ART-NEXT:      {
  // CHECK-ART-NEXT:        "text": "   indent"
  // CHECK-ART-NEXT:      },
  // CHECK-ART-NEXT:      {
  // CHECK-ART-NEXT:        "text": " last"
  // CHECK-ART-NEXT:      },
  // DISABLED-CHECK-ART-NEXT:      {
  // DISABLED-CHECK-ART-NEXT:        "text": "   "
  // DISABLED-CHECK-ART-NEXT:      }
  // DISABLED-CHECK-ART-NEXT:    ],
  // DISABLED-CHECK-ART-NEXT:    "module": "MyMod",
  // DISABLED-CHECK-ART-NEXT:    "uri": "file://{{.*}}mod{{\\\\|/}}M.h"
  // DISABLED-CHECK-ART-NEXT:  }

  /// doc1
  // reg1
  /// doc2 first
  /// doc2 last
  // reg2
  int mixedType;
  // CHECK-MIXED-TYPE: "docComment": {
  // CHECK-MIXED-TYPE-NEXT:    "lines": [
  // CHECK-MIXED-TYPE-NEXT:      {
  // CHECK-MIXED-TYPE-NEXT:        "text": "doc2 first"
  // CHECK-MIXED-TYPE-NEXT:      },
  // CHECK-MIXED-TYPE-NEXT:      {
  // CHECK-MIXED-TYPE-NEXT:        "text": "doc2 last"
  // CHECK-MIXED-TYPE-NEXT:      }
  // CHECK-MIXED-TYPE-NEXT:    ],
  // CHECK-MIXED-TYPE-NEXT:    "module": "MyMod",
  // CHECK-MIXED-TYPE-NEXT:    "uri": "file://{{.*}}mod{{\\\\|/}}M.h"
  // CHECK-MIXED-TYPE-NEXT:  }

  /// doc1
  /**
      doc2 first
        doc2 indented
      doc2 last
   */
  /// doc3
  int mixedDoc;
  // CHECK-MIXED-DOC: "docComment": {
  // CHECK-MIXED-DOC-NEXT:    "lines": [
  // CHECK-MIXED-DOC-NEXT:      {
  // CHECK-MIXED-DOC-NEXT:        "text": "doc1"
  // CHECK-MIXED-DOC-NEXT:      },
  // CHECK-MIXED-DOC-NEXT:      {
  // CHECK-MIXED-DOC-NEXT:        "text": ""
  // CHECK-MIXED-DOC-NEXT:      },
  // CHECK-MIXED-DOC-NEXT:      {
  // CHECK-MIXED-DOC-NEXT:        "text": "doc2 first"
  // CHECK-MIXED-DOC-NEXT:      },
  // CHECK-MIXED-DOC-NEXT:      {
  // CHECK-MIXED-DOC-NEXT:        "text": "  doc2 indented"
  // CHECK-MIXED-DOC-NEXT:      },
  // CHECK-MIXED-DOC-NEXT:      {
  // CHECK-MIXED-DOC-NEXT:        "text": "doc2 last"
  // CHECK-MIXED-DOC-NEXT:      },
  // CHECK-MIXED-DOC-NEXT:      {
  // CHECK-MIXED-DOC-NEXT:        "text": ""
  // CHECK-MIXED-DOC-NEXT:      },
  // DISABLED-CHECK-MIXED-DOC-NEXT:      {
  // DISABLED-CHECK-MIXED-DOC-NEXT:        "text": ""
  // DISABLED-CHECK-MIXED-DOC-NEXT:      },
  // DISABLED-CHECK-MIXED-DOC-NEXT:      {
  // DISABLED-CHECK-MIXED-DOC-NEXT:        "text": "doc3"
  // DISABLED-CHECK-MIXED-DOC-NEXT:      }
  // DISABLED-CHECK-MIXED-DOC-NEXT:    ],
  // DISABLED-CHECK-MIXED-DOC-NEXT:    "module": "MyMod",
  // DISABLED-CHECK-MIXED-DOC-NEXT:    "uri": "file://{{.*}}mod{{\\\\|/}}M.h"
  // DISABLED-CHECK-MIXED-DOC-NEXT:  }
};

#line 10 "other.h"
void funcUnderDirective(void);
// CHECK-DIRECTIVE-FUNC:  "location": {
// CHECK-DIRECTIVE-FUNC:    "uri": "file://{{.*}}other.h"
// CHECK-DIRECTIVE-FUNC:  }
