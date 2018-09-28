// RUN: rm -rf %t
// RUN: %target-swift-frontend -index-store-path %t/idx -primary-file %s -o %t/s1.o -I %S/Inputs -typecheck -module-cache-path %t/mcp
// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s -check-prefix=FILE1

// If the module cache already exists, the pcm gets indexed.
// RUN: rm -rf %t/idx
// RUN: %target-swift-frontend -index-store-path %t/idx -primary-file %s -o %t/s1.o -I %S/Inputs -typecheck -module-cache-path %t/mcp
// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s -check-prefix=FILE1

// FIXME: index the bridging header!

// RUN: %empty-directory(%t)
// RUN: echo 'import ClangModuleA' > %t/s2.swift
// RUN: %target-swift-frontend -index-store-path %t/idx %s %t/s2.swift -o %t/s1.o -o %t/s2.o -I %S/Inputs -c -emit-module -module-name main -emit-module-path %t/main.swiftmodule -module-cache-path %t/mcp
// RUN: c-index-test core -print-unit %t/idx > %t/both.txt
// RUN: %FileCheck %s -check-prefix=FILE1 < %t/both.txt
// RUN: %FileCheck %s -check-prefix=FILE2 < %t/both.txt


import ClangModuleB
import ClangModuleC.Sub1
import ClangModuleC.Sub2

func test() {
  funcA()
  funcB()
}

// FILE1: ClangModuleA-
// FILE1: --------
// FILE1: is-system: 0
// FILE1: has-main: 0
// FILE1: DEPEND START
// FILE1: Record | user | {{.*}}ClangModuleA.h | ClangModuleA.h-
// FILE1: DEPEND END

// FILE1: ClangModuleB-
// FILE1: --------
// FILE1: is-system: 0
// FILE1: has-main: 0
// FILE1: DEPEND START
// FILE1: Unit | user | ClangModuleA | {{.*}}ClangModuleA-{{.*}}.pcm | ClangModuleA-{{.*}}.pcm-
// FILE1: Record | user | {{.*}}ClangModuleB.h | ClangModuleB.h-
// FILE1: DEPEND END

// FILE1: s1.o-
// FILE1: --------
// FILE1: has-main: 1
// FILE1: DEPEND START
// FILE1-NOT: ClangModuleA.h
// FILE1-NOT: Unit |{{.*}}ClangModuleA
// FILE1: Unit | system | Swift | {{.*}}Swift.swiftmodule
// FILE1-NOT: Unit |{{.*}}ClangModuleA
// FILE1: Unit | user | ClangModuleB | {{.*}}ClangModuleB-{{[A-Z0-9]*}}.pcm | ClangModuleB-{{[A-Z0-9]*}}.pcm-
// FILE1: Unit | user | ClangModuleC | {{.*}}ClangModuleC-{{[A-Z0-9]*}}.pcm | ClangModuleC-{{[A-Z0-9]*}}.pcm-
// FILE1-NOT: Unit |{{.*}}ClangModuleA
// FILE1: Record | user | {{.*}}unit-pcm-dependency.swift | unit-pcm-dependency.swift-
// FILE1-NOT: Unit |{{.*}}ClangModuleA
// FILE1: DEPEND END (4)

// FILE2-NOT: main.swiftmodule-

// FILE2: s2.o-
// FILE2: --------
// FILE2: has-main: 1
// FILE2: out-file: {{.*}}s2.o
// FILE2: DEPEND START
// FILE2-NOT: ClangModuleB.h
// FILE2-NOT: Unit |{{.*}}ClangModuleB
// FILE2-NOT: Record
// FILE2: Unit | system | Swift | {{.*}}Swift.swiftmodule
// FILE2-NOT: Unit |{{.*}}ClangModuleB
// FILE2-NOT: Record
// FILE2: Unit | user | ClangModuleA | {{.*}}ClangModuleA-{{[A-Z0-9]*}}.pcm | ClangModuleA-{{[A-Z0-9]*}}.pcm-
// FILE2: Record | user | {{.*}}s2.swift | s2.swift-
// FILE2-NOT: Unit |{{.*}}ClangModuleB
// FILE2-NOT: Record
// FILE2: DEPEND END
