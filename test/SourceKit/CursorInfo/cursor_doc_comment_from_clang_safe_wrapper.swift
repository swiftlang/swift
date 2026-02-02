// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

//--- header.h

/** This comment contains `markup`.
 *
 * - And a list
 */
void testCDecl(const int * __attribute__((__counted_by__(len))) __attribute__((noescape)) p, int len);

//--- module.modulemap

module MyClangModule { header "header.h" }

//--- test.swift

import MyClangModule

func test(_ s: Span<CInt>) {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line + 1):4 -req-opts=retrieve_symbol_graph=1 %s -- %s -I %t -enable-experimental-feature SafeInteropWrappers | %FileCheck %s
  testCDecl(s)
}

// CHECK-LABEL: DOC COMMENT
// CHECK: This comment contains `markup`.
// CHECK: - And a list
// CHECK-LABEL: DOC COMMENT XML
// CHECK: <Function file="@__swiftmacro_So9testCDecl15_SwiftifyImportfMp_.swift" line="6" column="134"><Name>testCDecl(_:)</Name><USR>s:SC9testCDeclyys4SpanVys5Int32VGF</USR><Declaration>@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: public func testCDecl(_ p: Span&lt;Int32&gt;)</Declaration><CommentParts><Abstract><Para>This comment contains <codeVoice>markup</codeVoice>. *</Para></Abstract><Discussion><List-Bullet><Item><List-Bullet><Item><Para>And a list</Para></Item></List-Bullet></Item></List-Bullet><Para>This is an auto-generated wrapper for safer interop</Para></Discussion></CommentParts></Function>

// CHECK-LABEL: SYMBOL GRAPH BEGIN
// CHECK:      "docComment": {
// CHECK-NEXT:   "lines": [
// CHECK-NEXT:     {
// CHECK-NEXT:       "range": {
// CHECK-NEXT:         "end": {
// CHECK-NEXT:           "character": 35,
// CHECK-NEXT:           "line": 0
// CHECK-NEXT:         },
// CHECK-NEXT:         "start": {
// CHECK-NEXT:           "character": 4,
// CHECK-NEXT:           "line": 0
// CHECK-NEXT:         }
// CHECK-NEXT:       },
// CHECK-NEXT:       "text": "This comment contains `markup`."
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "range": {
// CHECK-NEXT:         "end": {
// CHECK-NEXT:           "character": 2,
// CHECK-NEXT:           "line": 1
// CHECK-NEXT:         },
// CHECK-NEXT:         "start": {
// CHECK-NEXT:           "character": 1,
// CHECK-NEXT:           "line": 1
// CHECK-NEXT:         }
// CHECK-NEXT:       },
// CHECK-NEXT:       "text": "*"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "range": {
// CHECK-NEXT:         "end": {
// CHECK-NEXT:           "character": 15,
// CHECK-NEXT:           "line": 2
// CHECK-NEXT:         },
// CHECK-NEXT:         "start": {
// CHECK-NEXT:           "character": 1,
// CHECK-NEXT:           "line": 2
// CHECK-NEXT:         }
// CHECK-NEXT:       },
// CHECK-NEXT:       "text": "* - And a list"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "range": {
// CHECK-NEXT:         "end": {
// CHECK-NEXT:           "character": 1,
// CHECK-NEXT:           "line": 3
// CHECK-NEXT:         },
// CHECK-NEXT:         "start": {
// CHECK-NEXT:           "character": 1,
// CHECK-NEXT:           "line": 3
// CHECK-NEXT:         }
// CHECK-NEXT:       },
// CHECK-NEXT:       "text": ""
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "range": {
// CHECK-NEXT:         "end": {
// CHECK-NEXT:           "character": 55,
// CHECK-NEXT:           "line": 4
// CHECK-NEXT:         },
// CHECK-NEXT:         "start": {
// CHECK-NEXT:           "character": 4,
// CHECK-NEXT:           "line": 4
// CHECK-NEXT:         }
// CHECK-NEXT:       },
// CHECK-NEXT:       "text": "This is an auto-generated wrapper for safer interop"
// CHECK-NEXT:     }
// CHECK-NEXT:   ],
// CHECK-NEXT:   "module": "MyClangModule",
// CHECK-NEXT:   "uri": "file://@__swiftmacro_So9testCDecl15_SwiftifyImportfMp_.swift"
// CHECK-NEXT: }
