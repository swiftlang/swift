// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/CrossImport %t/CrossImport
// RUN: %{python} %S/../CrossImport/Inputs/rewrite-module-triples.py %t/CrossImport %module-target-triple

// RUN: %target-swift-ide-test -print-indexed-symbols -enable-cross-import-overlays -source-filename %s -F %t/CrossImport > %t/out
// RUN: %FileCheck %s --check-prefix=CHECK-NEGATIVE -input-file=%t/out
//
// CHECK-NEGATIVE-NOT: module | user | _ABAdditions

// RUN: %FileCheck %s -input-file=%t/out
//
// CHECK: index_cross_import.swift
// CHECK: ------------
// CHECK-DAG: module | user | A | {{.*}}{{/|\\}}A.swiftmodule{{/|\\}}{{.*}}
// CHECK-DAG: module | user | A | {{.*}}{{/|\\}}_ABAdditions.swiftmodule{{/|\\}}{{.*}}
// CHECK-DAG: module | user | A | {{.*}}{{/|\\}}__ABAdditionsCAdditions.swiftmodule{{/|\\}}{{.*}}
// CHECK-DAG: module | user | B | {{.*}}{{/|\\}}B.swiftmodule{{/|\\}}{{.*}}
// CHECK-DAG: module | user | C | {{.*}}{{/|\\}}C.swiftmodule{{/|\\}}{{.*}}
// CHECK: ------------

import A
// CHECK: [[@LINE-1]]:8 | module{{/|\\}}Swift | A | {{.*}} | Ref | rel: 0
import B
// CHECK: [[@LINE-1]]:8 | module{{/|\\}}Swift | B | {{.*}} | Ref | rel: 0
import C
// CHECK: [[@LINE-1]]:8 | module{{/|\\}}Swift | C | {{.*}} | Ref | rel: 0

from_ABAdditions()
// CHECK: [[@LINE-1]]:1 | function{{/|\\}}Swift | from_ABAdditions() | {{.*}} | Ref,Call | rel: 0
from__ABAdditionsCAdditions()
// CHECK: [[@LINE-1]]:1 | function{{/|\\}}Swift | from__ABAdditionsCAdditions() | {{.*}} | Ref,Call | rel: 0
