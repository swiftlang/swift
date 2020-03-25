// RUN: %empty-directory(%t)
// RUN: cp -r %S/../Inputs/CrossImport %t/CrossImport
// RUN: %{python} %S/../../CrossImport/Inputs/rewrite-module-triples.py %t/CrossImport %module-target-triple

// RUN: %target-swift-frontend -c -index-store-path %t/idx -index-system-modules -index-ignore-stdlib -enable-cross-import-overlays %s -Fsystem %t/CrossImport -o %t/file1.o -module-name cross_import_overlay
// RUN: c-index-test core -print-unit %t/idx > %t/units
// RUN: %FileCheck %s --input-file %t/units --check-prefix=UNIT
// RUN: %FileCheck %s --input-file %t/units --check-prefix=UNIT-NEGATIVE

import A
import B
import C

fromA()
fromB()
from_ABAdditions()
from__ABAdditionsCAdditions()

// Check the overlay modules' names match the names of their underlying modules.
//
// UNIT: module-name: cross_import_overlay
// UNIT: main-path: {{.*}}/cross-import-overlay.swift
// UNIT: DEPEND START
// UNIT: Unit | system | B | {{.*}}/B.swiftmodule/{{.*}}
// UNIT: Unit | system | C | {{.*}}/C.swiftmodule/{{.*}}
// UNIT: Unit | system | A | {{.*}}/__ABAdditionsCAdditions.swiftmodule/{{.*}}
// UNIT: Record | user | {{.*}}/cross-import-overlay.swift | cross-import-overlay.swift-{{.*}}
// UNIT: DEPEND END
// UNIT: --------
// UNIT: module-name: A
// UNIT: out-file: {{.*}}/_ABAdditions.swiftmodule/{{.*}}
// UNIT: DEPEND START
// UNIT: Unit | system | A | {{.*}}/A.swiftmodule/{{.*}}
// UNIT: Unit | system | B | {{.*}}/B.swiftmodule/{{.*}}
// UNIT: Record | system | A | {{.*}}/_ABAdditions.swiftmodule/{{.*}}
// UNIT: DEPEND END
// UNIT: --------
// UNIT: module-name: A
// UNIT: out-file: {{.*}}/__ABAdditionsCAdditions.swiftmodule/{{.*}}
// UNIT: DEPEND START
// UNIT: Unit | system | C | {{.*}}/C.swiftmodule/{{.*}}
// UNIT: Unit | system | A | {{.*}}/_ABAdditions.swiftmodule/{{.*}}
// UNIT: Record | system | A | {{.*}}/__ABAdditionsCAdditions.swiftmodule/{{.*}}
// UNIT: DEPEND END
// UNIT: --------
// UNIT: module-name: C
// UNIT: out-file: {{.*}}/C.swiftmodule/{{.*}}
// UNIT: DEPEND START
// UNIT: Record | system | C | {{.*}}/C.swiftmodule/{{.*}}
// UNIT: DEPEND END
// UNIT: --------
// UNIT: module-name: B
// UNIT: out-file: {{.*}}/B.swiftmodule/{{.*}}
// UNIT: DEPEND START
// UNIT: Record | system | B | {{.*}}/B.swiftmodule/{{.*}}
// UNIT: DEPEND END
// UNIT: --------
// UNIT: module-name: A
// UNIT: out-file: {{.*}}/A.swiftmodule/{{.*}}
// UNIT: DEPEND START
// UNIT: Record | system | A | {{.*}}/A.swiftmodule/{{.*}}
// UNIT: DEPEND END

// Make sure we aren't leaking the underscored overlay names anywhere
//
// UNIT-NEGATIVE-NOT: Unit | {{.*}} | _ABAdditions |
// UNIT-NEGATIVE-NOT: Record | {{.*}} | _ABAdditions |
// UNIT-NEGATIVE-NOT: Unit | {{.*}} | __ABAdditionsCAdditions |
// UNIT-NEGATIVE-NOT: Record | {{.*}} | __ABAdditionsCAdditions |

// Make sure we don't regress test performance by indexing the stdlib.
//
// UNIT-NEGATIVE-NOT: Record | system | Swift |

