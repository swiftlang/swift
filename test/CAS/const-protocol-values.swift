// UNSUPPORTED: OS=windows-msvc
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/test.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas -const-gather-protocols-file %t/protocols.json \
// RUN:   -scanner-prefix-map-paths %t /^tmp -I %t/include

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json A casFSRootID > %t/A.casid
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-include-tree-list @%t/A.casid | %FileCheck %s --check-prefix=A-FS
// A-FS-NOT: protocols.json

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json Test casFSRootID > %t/test.casid
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-include-tree-list @%t/test.casid | %FileCheck %s --check-prefix=TEST-FS
// TEST-FS: protocols.json

// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps.json -o %t/MyApp.cmd

// RUN: %target-swift-frontend \
// RUN:   -typecheck -cache-compile-job -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -module-name Test -cache-replay-prefix-map /^tmp %t -const-gather-protocols-file /^tmp/protocols.json \
// RUN:   /^tmp/test.swift @%t/MyApp.cmd -emit-const-values-path %t/main.module.swiftconstvalues

// RUN: %FileCheck %s < %t/main.module.swiftconstvalues

// RUN: %target-swift-frontend \
// RUN:   -typecheck -cache-compile-job -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -module-name Test -cache-replay-prefix-map /^tmp %t -const-gather-protocols-file /^tmp/protocols.json \
// RUN:   /^tmp/test.swift @%t/MyApp.cmd -emit-const-values-path %t/main.module2.swiftconstvalues -Rcache-compile-job 2>&1 | %FileCheck %s --check-prefix=HIT

// HIT: remark: replay output file 'TMP_DIR/main.module2.swiftconstvalues'

// RUN: %FileCheck %s < %t/main.module2.swiftconstvalues

// CHECK: [
// CHECK-NEXT:   {
// CHECK-NEXT:     "allConformances": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "conformanceDefiningModule": "Test",
// CHECK-NEXT:         "protocolName": "Test.Foo"
// CHECK-NEXT:       }
// CHECK-NEXT:     ],
// CHECK-NEXT:     "associatedTypeAliases": [],
// CHECK-NEXT:     "conformances": [
// CHECK-NEXT:       "Test.Foo"
// CHECK-NEXT:     ],
// CHECK-NEXT:     "file": "TMP_DIR/test.swift",
// CHECK-NEXT:     "kind": "struct",
// CHECK-NEXT:     "line": 14,
// CHECK-NEXT:     "mangledTypeName": "4Test5MyFooV",
// CHECK-NEXT:     "properties": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "file": "TMP_DIR/test.swift",
// CHECK-NEXT:         "isComputed": "false",
// CHECK-NEXT:         "isStatic": "false",
// CHECK-NEXT:         "label": "init1",
// CHECK-NEXT:         "line": 15,
// CHECK-NEXT:         "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:         "type": "Swift.String",
// CHECK-NEXT:         "value": "Value",
// CHECK-NEXT:         "valueKind": "RawLiteral"
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "file": "TMP_DIR/test.swift",
// CHECK-NEXT:         "isComputed": "false",
// CHECK-NEXT:         "isStatic": "false",
// CHECK-NEXT:         "label": "_value",
// CHECK-NEXT:         "line": 18,
// CHECK-NEXT:         "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:         "type": "Test.Zero",
// CHECK-NEXT:         "value": {
// CHECK-NEXT:           "arguments": [],
// CHECK-NEXT:           "type": "Test.Zero"
// CHECK-NEXT:         },
// CHECK-NEXT:         "valueKind": "InitCall"
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "file": "TMP_DIR/test.swift",
// CHECK-NEXT:         "isComputed": "false",
// CHECK-NEXT:         "isStatic": "true",
// CHECK-NEXT:         "label": "init2",
// CHECK-NEXT:         "line": 16,
// CHECK-NEXT:         "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:         "type": "Swift.String",
// CHECK-NEXT:         "value": "Value2",
// CHECK-NEXT:         "valueKind": "RawLiteral"
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "file": "TMP_DIR/test.swift",
// CHECK-NEXT:         "isComputed": "true",
// CHECK-NEXT:         "isStatic": "false",
// CHECK-NEXT:         "label": "value",
// CHECK-NEXT:         "line": 18,
// CHECK-NEXT:         "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:         "propertyWrappers": [
// CHECK-NEXT:           {
// CHECK-NEXT:             "arguments": [],
// CHECK-NEXT:             "file": "TMP_DIR/test.swift",
// CHECK-NEXT:             "line": 18,
// CHECK-NEXT:             "type": "Test.Zero"
// CHECK-NEXT:           }
// CHECK-NEXT:         ],
// CHECK-NEXT:         "type": "Swift.Int",
// CHECK-NEXT:         "valueKind": "Runtime"
// CHECK-NEXT:       }
// CHECK-NEXT:     ],
// CHECK-NEXT:     "typeName": "Test.MyFoo"
// CHECK-NEXT:   }
// CHECK-NEXT: ]

//--- include/A.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name A -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib

//--- test.swift
import A

protocol Foo {}

@propertyWrapper
struct Zero {
    private var number = 0
    var wrappedValue: Int {
        get { number }
        set { number = 0 }
    }
}

public struct MyFoo : Foo {
  let init1 = "Value"
  static let init2 = "Value2"

  @Zero var value: Int
}

//--- protocols.json
["Foo"]
