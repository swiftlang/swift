// This test checks that we produce autolink entries into the expected places
// when performing multi-threaded IR generation.

// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend -emit-module -o %t -module-name empty -module-link-name empty %S/../Inputs/empty.swift

// RUN: %target-swift-frontend -emit-ir %t/src/A.swift %t/src/B.swift -I %t -Fsystem %S/Inputs/Frameworks -o %t/A.ll -o %t/B.ll -num-threads 2 -O -g -module-name test
// RUN: %FileCheck --check-prefix=CHECK-A %s <%t/A.ll
// RUN: %FileCheck --check-prefix=CHECK-B %s <%t/B.ll

// Linux uses a different autolinking mechanism, based on
// swift-autolink-extract. This file tests the Darwin mechanism.
// UNSUPPORTED: autolink-extract
// REQUIRES: OS=macosx


//--- A.swift
import empty

public func f() -> String { "hello" }

// CHECK-A: !llvm.linker.options = !{
// CHECK-A: !{{[0-9]+}} = !{!{{"-lempty"|"/DEFAULTLIB:empty.lib"}}}

//--- B.swift
import LinkFramework

public func useLibrarySym() {
  let _ = LinkFramework.IComeFromLinkFramework
}

// CHECK-B: !llvm.linker.options = !{
// CHECK-B: !{{[0-9]+}} = !{!"-framework", !"LinkFramework"}
