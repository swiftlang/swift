// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// DEFINE: %{same-file-function}=                 $s4Test6calleryys4SpanVys5Int32VGF
// DEFINE: %{same-module-different-file-function}=$s4Test7caller2yys4SpanVys5Int32VGF
// DEFINE: %{different-module-function}=          $s6Caller6calleryys4SpanVys5Int32VGF

// RUN: %target-swift-frontend -emit-silgen -module-name Test -plugin-path %swift-plugin-dir -I %t \
// RUN:   -enable-experimental-feature SafeInteropImplementations %t/test.swift | %FileCheck %s --check-prefix=SIL -DFUNCTION=%{same-file-function}
// RUN: %target-swift-frontend -emit-ir -module-name Test -plugin-path %swift-plugin-dir -I %t \
// RUN:   -enable-experimental-feature SafeInteropImplementations %t/test.swift | %FileCheck %s --check-prefix=IR -DFUNCTION=%{same-file-function}

// Same check with an additional caller in a different file of the same module.
// RUN: %target-swift-frontend -emit-silgen -module-name Test -plugin-path %swift-plugin-dir -I %t \
// RUN:   -enable-experimental-feature SafeInteropImplementations %t/test.swift %t/caller2.swift | %FileCheck %s --check-prefix=SIL -DFUNCTION=%{same-module-different-file-function}
// RUN: %target-swift-frontend -emit-ir -module-name Test -plugin-path %swift-plugin-dir -I %t \
// RUN:   -enable-experimental-feature SafeInteropImplementations %t/test.swift %t/caller2.swift | %FileCheck %s --check-prefix=IR -DFUNCTION=%{same-module-different-file-function}

// Same as above, with the order of the input files flipped.
// RUN: %target-swift-frontend -emit-silgen -module-name Test -plugin-path %swift-plugin-dir -I %t \
// RUN:   -enable-experimental-feature SafeInteropImplementations %t/caller2.swift %t/test.swift | %FileCheck %s --check-prefix=SIL -DFUNCTION=%{same-module-different-file-function}
// RUN: %target-swift-frontend -emit-ir -module-name Test -plugin-path %swift-plugin-dir -I %t \
// RUN:   -enable-experimental-feature SafeInteropImplementations %t/caller2.swift %t/test.swift | %FileCheck %s --check-prefix=IR -DFUNCTION=%{same-module-different-file-function}

// Same check, compiling only one of the two files (the other is non-primary).
// RUN: %target-swift-frontend -emit-silgen -module-name Test -plugin-path %swift-plugin-dir -I %t \
// RUN:   -enable-experimental-feature SafeInteropImplementations -primary-file %t/caller2.swift %t/test.swift | %FileCheck %s --check-prefix=SIL -DFUNCTION=%{same-module-different-file-function}
// RUN: %target-swift-frontend -emit-silgen -module-name Test -plugin-path %swift-plugin-dir -I %t \
// RUN:   -enable-experimental-feature SafeInteropImplementations -primary-file %t/test.swift %t/caller2.swift | %FileCheck %s --check-prefix=SIL -DFUNCTION=%{same-file-function}

// Same check with the implementation and the caller in different modules, both
// of which import the C header that declares `foo`.
// RUN: %target-swift-frontend -emit-module -module-name Test -plugin-path %swift-plugin-dir -I %t \
// RUN:   -enable-experimental-feature SafeInteropImplementations %t/test.swift -o %t/Test.swiftmodule
// RUN: %target-swift-frontend -emit-silgen -module-name Caller -plugin-path %swift-plugin-dir -I %t \
// RUN:   -enable-experimental-feature SafeInteropImplementations %t/caller.swift | %FileCheck %s --check-prefix=SIL -DFUNCTION=%{different-module-function}
// RUN: %target-swift-frontend -emit-ir -module-name Caller -plugin-path %swift-plugin-dir -I %t \
// RUN:   -enable-experimental-feature SafeInteropImplementations %t/caller.swift | %FileCheck %s --check-prefix=IR -DFUNCTION=%{different-module-function}

// A Swift caller of a safe `@c @implementation` function must call the Swift
// function directly with the Swift calling convention. It should not resolve to
// the safe wrapper synthesized for the C declaration, which would round-trip
// through the C entry point (`foo(ptr, len)`) and back into the implementation.

//--- test.swift
import CHeader

@c @implementation
public func foo(_ s: Span<CInt>) {}

func caller(_ s: Span<CInt>) {
  foo(s)
}

// SIL: sil hidden [ossa] @[[FUNCTION]] : $@convention(thin) (@guaranteed Span<Int32>) -> () {
// SIL:         function_ref @$s4Test3fooyys4SpanVys5Int32VGF : $@convention(thin) (@guaranteed Span<Int32>) -> ()
// SIL-NEXT:    apply
// SIL-NOT:     convention(c)
// SIL-NOT:     function_ref {{.*}}foo
// SIL:       } // end sil function '[[FUNCTION]]'

// IR: define hidden swiftcc void @"[[FUNCTION]]"
// IR:         call swiftcc void @"$s4Test3fooyys4SpanVys5Int32VGF"
// IR-NOT:     call {{.*}}foo
// IR:       }

//--- caller.swift
import CHeader
import Test

func caller(_ s: Span<CInt>) {
  foo(s)
}

//--- caller2.swift
import CHeader

func caller2(_ s: Span<CInt>) {
  foo(s)
}

//--- test.h
#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __noescape __attribute__((noescape))

void foo(const int * _Nonnull __counted_by(len) __noescape x, int len);

//--- module.modulemap
module CHeader {
  header "test.h"
}
