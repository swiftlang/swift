// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// All the compile jobs in a batch build share all the references in the base
/// cache key. The command-line arguments that are not part of the input files
/// must also stay the same after editing a source file. If this test fails, a
/// newly added option that is different for each job or changes when a source
/// file is edited needs to be handled in Frontend/CompileJobCacheKey.cpp.

// RUN: cd %t && %target-swiftc_driver -### -c -module-name Test -parse-stdlib \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -enable-batch-mode -driver-batch-count 3 -explicit-module-build -cache-compile-job -cas-path %t/cas \
// RUN:   -g -emit-module -emit-module-path %t/Test.swiftmodule -Xcc -DFOO %t/a.swift %t/b.swift %t/c.swift %t/d.swift %t/e.swift %t/f.swift > %t/jobs1.txt
// RUN: %{python} %S/Inputs/PrintBaseKeyRefs.py %cache-tool llvm-cas %t/cas %t/jobs1.txt > %t/refs1.txt
// RUN: %FileCheck %s --check-prefix=SHARED < %t/refs1.txt

// SHARED: jobs: 3
// SHARED-NEXT: unique: 1
// SHARED-NEXT: llvmcas://

/// Edit a source file.
// RUN: echo "func a2() {}" >> %t/a.swift
// RUN: cd %t && %target-swiftc_driver -### -c -module-name Test -parse-stdlib \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -enable-batch-mode -driver-batch-count 3 -explicit-module-build -cache-compile-job -cas-path %t/cas \
// RUN:   -g -emit-module -emit-module-path %t/Test.swiftmodule -Xcc -DFOO %t/a.swift %t/b.swift %t/c.swift %t/d.swift %t/e.swift %t/f.swift > %t/jobs2.txt
// RUN: %{python} %S/Inputs/PrintBaseKeyRefs.py %cache-tool llvm-cas %t/cas %t/jobs2.txt > %t/refs2.txt
// RUN: %FileCheck %s --check-prefix=SHARED < %t/refs2.txt

/// The base keys change since the include tree file list (the 5th reference)
/// changes, but the version, the command-line arguments, the clang arguments,
/// the unused include tree root, and the input list (the 6th reference, since
/// the inputs come first on the command-line) are reused. The references are
/// printed from the 3rd line.
// RUN: not diff %t/refs1.txt %t/refs2.txt
// RUN: sed -n '3,6p;8p' %t/refs1.txt > %t/shared1.txt
// RUN: sed -n '3,6p;8p' %t/refs2.txt > %t/shared2.txt
// RUN: diff %t/shared1.txt %t/shared2.txt
// RUN: sed -n 7p %t/refs1.txt > %t/filelist1.txt
// RUN: sed -n 7p %t/refs2.txt > %t/filelist2.txt
// RUN: not diff %t/filelist1.txt %t/filelist2.txt

//--- a.swift
func a() {}

//--- b.swift
func b() {}

//--- c.swift
func c() {}

//--- d.swift
func d() {}

//--- e.swift
func e() {}

//--- f.swift
func f() {}
