// RUN: rm -rf %t && mkdir -p %t

// RUN: %target-swift-frontend -emit-module -o %t -module-name HasPrivateAccess %S/Inputs/HasPrivateAccess1.swift %S/Inputs/HasPrivateAccess2.swift
// RUN: %target-swift-ide-test -print-ast-typechecked -source-filename %s -I %t -sdk "" -disable-access-control 2>&1 | FileCheck -check-prefix=CHECK-ERROR %s
// RUN: %target-swift-ide-test -print-ast-typechecked -source-filename %s -I %t -sdk "" -disable-access-control -explode-pattern-binding-decls -debug-client-discriminator _5AB3F657DD2A7E5E793501C5FA480C3D | FileCheck -check-prefix=CHECK-INT %s
// RUN: %target-swift-ide-test -print-ast-typechecked -source-filename %s -I %t -sdk "" -disable-access-control -explode-pattern-binding-decls -debug-client-discriminator _0D6EC78101B0986747C7103C2739A767 | FileCheck -check-prefix=CHECK-STRING %s

import HasPrivateAccess

// CHECK-ERROR: ambiguous use of 'global'
// CHECK-INT: let unqualified: Int
// CHECK-STRING: let unqualified: String
let unqualified = global

// CHECK-ERROR: ambiguous use of 'global'
// CHECK-INT: let qualified: Int
// CHECK-STRING: let qualified: String
let qualified = HasPrivateAccess.global

// CHECK-ERROR: ambiguous use of 'method'
// CHECK-INT: let result: Int?
// CHECK-STRING: let result: String?
let result = HasPrivateAccess.MyStruct.method()
