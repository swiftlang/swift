// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/mcp)
// RUN: cp -r %S/../Inputs/CrossImport %t/CrossImport
// RUN: %{python} %S/../../CrossImport/Inputs/rewrite-module-triples.py %t/CrossImport %module-target-triple

// RUN: %target-swift-frontend -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -c -index-store-path %t/idx -module-cache-path %t/mcp -index-system-modules -index-ignore-stdlib -enable-cross-import-overlays %s -Fsystem %t/CrossImport -o %t/file1.o -module-name cross_import_overlay -sdk %t
// RUN: c-index-test core -print-unit %t/idx > %t/units

import A
import B
import C

fromA()
fromB()
from_ABAdditions()
from__ABAdditionsCAdditions()

// Check the overlay modules pick up the name of their underlying module.
//
// FIXME: the units are sorted by their unit name, which for the frameworks in
// this test end up just being the target triple + a hash of the output file
// path which changes depending on where this test is run. We should fix this
// properly, but for now work around it by checking for each unit in a separate
// pass and do our best to make sure we don't match across unit boundaries.
//
// RUN: %FileCheck %s --input-file %t/units --check-prefix=MAIN
// MAIN:      module-name: cross_import_overlay
// MAIN:      out-file: {{.*}}{{/|\\}}file1.o
// MAIN-NEXT: target: {{.*}}
// MAIN-NEXT: is-debug: 1
// MAIN-NEXT: DEPEND START
// MAIN-NEXT: Unit | system | Swift | {{.*}}{{/|\\}}Swift.swiftmodule
// MAIN-NEXT: Unit | system | B | {{.*}}{{/|\\}}B.swiftmodule{{/|\\}}{{.*}}
// MAIN-NEXT: Unit | system | C | {{.*}}{{/|\\}}C.swiftmodule{{/|\\}}{{.*}}
// MAIN-NEXT: Unit | system | A | {{.*}}{{/|\\}}__ABAdditionsCAdditions.swiftmodule{{/|\\}}{{.*}}
// MAIN-NEXT: Record | user | {{.*}}{{/|\\}}cross-import-overlay.swift
// MAIN-NEXT: DEPEND END
//
// RUN: %FileCheck %s --input-file %t/units --check-prefix=AB_OVERLAY
// AB_OVERLAY:      module-name: A
// AB_OVERLAY:      out-file:{{.*}}{{/|\\}}_ABAdditions.swiftmodule{{/|\\}}{{.*}}
// AB_OVERLAY-NEXT: target: {{.*}}
// AB_OVERLAY-NEXT: is-debug: 1
// AB_OVERLAY-NEXT: DEPEND START
// AB_OVERLAY-NEXT: Unit | system | A | {{.*}}{{/|\\}}A.swiftmodule{{/|\\}}{{.*}}
// AB_OVERLAY-NEXT: Unit | system | B | {{.*}}{{/|\\}}B.swiftmodule{{/|\\}}{{.*}}
// AB_OVERLAY-NEXT: Unit | system | Swift | {{.*}}{{/|\\}}Swift.swiftmodule
// AB_OVERLAY-NEXT: Record | system | A | {{.*}}{{/|\\}}_ABAdditions.swiftmodule{{/|\\}}{{.*}}
// AB_OVERLAY-NEXT: DEPEND END
//
// RUN: %FileCheck %s --input-file %t/units --check-prefix=ABC_OVERLAY
// ABC_OVERLAY:      module-name: A
// ABC_OVERLAY:      out-file: {{.*}}{{/|\\}}__ABAdditionsCAdditions.swiftmodule{{/|\\}}{{.*}}
// ABC_OVERLAY-NEXT: target: {{.*}}
// ABC_OVERLAY-NEXT: is-debug: 1
// ABC_OVERLAY-NEXT: DEPEND START
// ABC_OVERLAY-NEXT: Unit | system | C | {{.*}}{{/|\\}}C.swiftmodule{{/|\\}}{{.*}}
// ABC_OVERLAY-NEXT: Unit | system | Swift | {{.*}}{{/|\\}}Swift.swiftmodule
// ABC_OVERLAY-NEXT: Unit | system | A | {{.*}}{{/|\\}}_ABAdditions.swiftmodule{{/|\\}}{{.*}}
// ABC_OVERLAY-NEXT: Record | system | A | {{.*}}{{/|\\}}__ABAdditionsCAdditions.swiftmodule{{/|\\}}{{.*}}
// ABC_OVERLAY-NEXT: DEPEND END
//
// RUN: %FileCheck %s --input-file %t/units --check-prefix=C_MODULE
// C_MODULE:      module-name: C
// C_MODULE:      out-file: {{.*}}{{/|\\}}C.swiftmodule{{/|\\}}{{.*}}
// C_MODULE-NEXT: target: {{.*}}
// C_MODULE-NEXT: is-debug: 1
// C_MODULE-NEXT: DEPEND START
// C_MODULE-NEXT: Unit | system | Swift | {{.*}}{{/|\\}}Swift.swiftmodule
// C_MODULE-NEXT: Record | system | C | {{.*}}{{/|\\}}C.swiftmodule{{/|\\}}{{.*}}
// C_MODULE-NEXT: DEPEND END
//
// RUN: %FileCheck %s --input-file %t/units --check-prefix=B_MODULE
// B_MODULE:      module-name: B
// B_MODULE:      out-file: {{.*}}{{/|\\}}B.swiftmodule{{/|\\}}{{.*}}
// B_MODULE-NEXT: target: {{.*}}
// B_MODULE-NEXT: is-debug: 1
// B_MODULE-NEXT: DEPEND START
// B_MODULE-NEXT: Unit | system | Swift | {{.*}}{{/|\\}}Swift.swiftmodule
// B_MODULE-NEXT: Record | system | B | {{.*}}{{/|\\}}B.swiftmodule{{/|\\}}{{.*}}
// B_MODULE-NEXT: DEPEND END
//
// RUN: %FileCheck %s --input-file %t/units --check-prefix=A_MODULE
// A_MODULE:      module-name: A
// A_MODULE: out-file: {{.*}}{{/|\\}}A.swiftmodule{{/|\\}}{{.*}}
// A_MODULE-NEXT: target: {{.*}}
// A_MODULE-NEXT: is-debug: 1
// A_MODULE-NEXT: DEPEND START
// A_MODULE-NEXT: Unit | system | Swift | {{.*}}{{/|\\}}Swift.swiftmodule
// A_MODULE-NEXT: Record | system | A | {{.*}}{{/|\\}}A.swiftmodule{{/|\\}}{{.*}}
// A_MODULE-NEXT: DEPEND END


// Make sure there are three units with module-name 'A' (A, _ABAdditions and
// __ABAdditionsCAdditions) in case we matched across unit boundaries above
// due to the nondeterministic ordering.
//
// RUN: %FileCheck %s --input-file %t/units --check-prefix=UNITS
// UNITS-COUNT-3: module-name: A

// Make sure we aren't leaking the underscored overlay names anywhere and don't
// regress test performance by indexing the stdlib.
//
// RUN: %FileCheck %s --input-file %t/units --check-prefix=UNITS-NEGATIVE
// UNITS-NEGATIVE-NOT: Unit | {{.*}} | _ABAdditions
// UNITS-NEGATIVE-NOT: Record | {{.*}} | _ABAdditions
// UNITS-NEGATIVE-NOT: Unit | {{.*}} | __ABAdditionsCAdditions
// UNITS-NEGATIVE-NOT: Record | {{.*}} | __ABAdditionsCAdditions
// UNITS-NEGATIVE-NOT: Record | system | Swift

