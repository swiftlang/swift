// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: not %target-swift-frontend -typecheck %t/test.swift \
// RUN:   -explicit-swift-module-map-file %t/invalid_root_object.json \
// RUN:   2>&1 | %FileCheck %s -check-prefix=CHECK-ROOT

// CHECK-ROOT: malformed: invalid JSON root object

// RUN: not %target-swift-frontend -typecheck %t/test.swift \
// RUN:   -explicit-swift-module-map-file %t/invalid_entry_type.json \
// RUN:   2>&1 | %FileCheck %s -check-prefix=CHECK-ENTRY-TYPE

// CHECK-ENTRY-TYPE: malformed: incorrect entry type

// RUN: not %target-swift-frontend -typecheck %t/test.swift \
// RUN:   -explicit-swift-module-map-file %t/missing_module_name.json \
// RUN:   2>&1 | %FileCheck %s -check-prefix=CHECK-NAME

// CHECK-NAME: malformed: entry is missing module name

// RUN: not %target-swift-frontend -typecheck %t/test.swift \
// RUN:   -explicit-swift-module-map-file %t/duplicate_swift_module.json \
// RUN:   2>&1 | %FileCheck %s -check-prefix=CHECK-DUP-SWIFT

// CHECK-DUP-SWIFT: malformed: duplicate Swift module with name SwiftMod

// RUN: not %target-swift-frontend -typecheck %t/test.swift \
// RUN:   -explicit-swift-module-map-file %t/duplicate_clang_module.json \
// RUN:   2>&1 | %FileCheck %s -check-prefix=CHECK-DUP-CLANG

// CHECK-DUP-CLANG: malformed: duplicate Clang module with name ClangMod

//--- invalid_root_object.json
{
    "some_key": "some_val"
}
//--- invalid_entry_type.json
[
    [
        {"some_key": "some_val"}
    ]
]
//--- missing_module_name.json
[
    {
        "isFramework": false,
        "modulePath": "/some/path"
    }
]
//--- duplicate_swift_module.json
[
    {
        "isFramework": false,
        "moduleName": "SwiftMod",
        "modulePath": "/some/path"
    },
    {
        "isFramework": false,
        "moduleName": "SwiftMod",
        "modulePath": "/some/path"
    }
]
//--- duplicate_clang_module.json
[
    {
        "isFramework": false,
        "moduleName": "ClangMod",
        "clangModulePath": "/some/path"
    },
    {
        "isFramework": false,
        "moduleName": "ClangMod",
        "clangModulePath": "/some/path"
    }
]
//--- test.swift
import Swift
