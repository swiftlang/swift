// RUN: rm -rf %t
// RUN: mkdir -p %t/BUILDROOT && cd %t/BUILDROOT
// RUN: %target-swift-frontend -disable-implicit-concurrency-module-import -index-store-path %t/idx -file-prefix-map %t=REMAPPED_OUT_DIR -primary-file %s -o %t/s1.o -I %S/Inputs -typecheck -module-cache-path %t/mcp -enable-objc-interop
// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s -check-prefix=FILE1 --dump-input-filter all

// If the module cache already exists, the pcm gets indexed.
// RUN: rm -rf %t/idx
// RUN: %target-swift-frontend -disable-implicit-concurrency-module-import -index-store-path %t/idx -file-prefix-map %t=REMAPPED_OUT_DIR -primary-file %s -o %t/s1.o -I %S/Inputs -typecheck -module-cache-path %t/mcp -enable-objc-interop
// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s -check-prefix=FILE1 --dump-input-filter all

// FIXME: index the bridging header!

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/BUILDROOT && cd %t/BUILDROOT
// RUN: echo 'import ClangModuleA' > %t/s2.swift
// RUN: %target-swift-frontend -disable-implicit-concurrency-module-import -index-store-path %t/idx -file-prefix-map %S=REMAPPED_SRC_DIR -file-prefix-map %t=REMAPPED_OUT_DIR %s %t/s2.swift -o %t/s1.o -o %t/s2.o -I %S/Inputs -c -emit-module -module-name main -emit-module-path %t/main.swiftmodule -module-cache-path %t/mcp -enable-objc-interop
// RUN: c-index-test core -print-unit %t/idx > %t/both.txt
// RUN: %FileCheck %s -check-prefixes=FILE1,FILE1-ABSOLUTE < %t/both.txt --dump-input-filter all
// RUN: %FileCheck %s -check-prefixes=FILE2,FILE2-ABSOLUTE < %t/both.txt --dump-input-filter all

//===--- Same as above, but with relative paths for the source file inputs,
//===--- which we give from the %t test dir.

// RUN: %empty-directory(%t)
// RUN: cd %t
// RUN: cp %s %t/
// RUN: echo 'import ClangModuleA' > %t/s2.swift
// RUN: %target-swift-frontend -disable-implicit-concurrency-module-import -index-store-path idx -file-prefix-map %t=REMAPPED_OUT_DIR unit-pcm-dependency-remapped.swift s2.swift -o s1.o -o s2.o -I %S/Inputs -c -emit-module -module-name main -emit-module-path main.swiftmodule -module-cache-path mcp -enable-objc-interop
// RUN: c-index-test core -print-unit %t/idx > %t/both.txt
// RUN: %FileCheck %s -check-prefixes=FILE1,FILE1-RELATIVE < %t/both.txt --dump-input-filter all
// RUN: %FileCheck %s -check-prefixes=FILE2,FILE2-RELATIVE < %t/both.txt --dump-input-filter all


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
// FILE1: out-file: REMAPPED_OUT_DIR{{.*}}ClangModuleA-{{.*}}.pcm
// FILE1: DEPEND START
// FILE1: Record | user | {{.*}}ClangModuleA.h | ClangModuleA.h-
// FILE1: DEPEND END

// FILE1: ClangModuleB-
// FILE1: --------
// FILE1: is-system: 0
// FILE1: has-main: 0
// FILE1: out-file: REMAPPED_OUT_DIR{{.*}}ClangModuleB-{{.*}}.pcm
// FILE1: DEPEND START
// FILE1: Unit | user | ClangModuleA | REMAPPED_OUT_DIR{{/|\\}}mcp{{.*}}ClangModuleA-{{.*}}.pcm | ClangModuleA-{{.*}}.pcm-
// FILE1: Record | user | {{.*}}ClangModuleB.h | ClangModuleB.h-
// FILE1: DEPEND END

// FILE1: s1.o-
// FILE1: --------
// FILE1: has-main: 1
// FILE1-ABSOLUTE: main-path: REMAPPED_SRC_DIR{{.*}}unit-pcm-dependency-remapped.swift
// FILE1-ABSOLUTE: work-dir: REMAPPED_OUT_DIR{{/|\\}}BUILDROOT
// FILE1-RELATIVE: main-path: REMAPPED_OUT_DIR{{.*}}unit-pcm-dependency-remapped.swift
// FILE1-RELATIVE: work-dir: REMAPPED_OUT_DIR
// FILE1: out-file: REMAPPED_OUT_DIR{{/|\\}}s1.o
// FILE1: DEPEND START
// FILE1-NOT: ClangModuleA.h
// FILE1-NOT: Unit |{{.*}}ClangModuleA
// FILE1: Unit | system | Swift | {{BUILD_DIR|.*lib\\swift\\windows}}{{.*}}Swift.swiftmodule
// FILE1-NOT: Unit |{{.*}}ClangModuleA
// FILE1: Unit | user | ClangModuleB | REMAPPED_OUT_DIR{{/|\\}}mcp{{.*}}ClangModuleB-{{[A-Z0-9]*}}.pcm | ClangModuleB-{{[A-Z0-9]*}}.pcm-
// FILE1: Unit | user | ClangModuleC | REMAPPED_OUT_DIR{{/|\\}}mcp{{.*}}ClangModuleC-{{[A-Z0-9]*}}.pcm | ClangModuleC-{{[A-Z0-9]*}}.pcm-
// FILE1-NOT: Unit |{{.*}}ClangModuleA
// FILE1: Record | user | {{.*}}unit-pcm-dependency-remapped.swift | unit-pcm-dependency-remapped.swift-
// FILE1-NOT: Unit |{{.*}}ClangModuleA
// FILE1: DEPEND END

// FILE2-NOT: main.swiftmodule-

// FILE2: s2.o-
// FILE2: --------
// FILE2: has-main: 1
// FILE2: main-path: REMAPPED_OUT_DIR{{.*}}s2.swift
// FILE2: out-file: {{.*}}s2.o
// FILE2: DEPEND START
// FILE2-NOT: ClangModuleB.h
// FILE2-NOT: Unit |{{.*}}ClangModuleB
// FILE2-NOT: Record
// FILE2: Unit | system | Swift | {{BUILD_DIR|.*lib\\swift\\windows}}{{.*}}Swift.swiftmodule
// FILE2-NOT: Unit |{{.*}}ClangModuleB
// FILE2-NOT: Record
// FILE2: Unit | user | ClangModuleA | REMAPPED_OUT_DIR{{/|\\}}mcp{{.*}}ClangModuleA-{{[A-Z0-9]*}}.pcm | ClangModuleA-{{[A-Z0-9]*}}.pcm-
// FILE2: Record | user | {{.*}}s2.swift | s2.swift-
// FILE2-NOT: Unit |{{.*}}ClangModuleB
// FILE2-NOT: Record
// FILE2: DEPEND END
