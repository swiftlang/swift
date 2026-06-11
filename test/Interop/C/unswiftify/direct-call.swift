// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-silgen -module-name main -plugin-path %swift-plugin-dir -I %t \
// RUN:   -enable-experimental-feature SafeInteropImplementations %t/test.swift | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend -emit-ir -module-name main -plugin-path %swift-plugin-dir -I %t \
// RUN:   -enable-experimental-feature SafeInteropImplementations %t/test.swift | %FileCheck %s --check-prefix=IR

// Same check with the implementation and the caller in different modules, both
// of which import the C header that declares `foo`.
// RUN: %target-swift-frontend -emit-module -module-name Test -plugin-path %swift-plugin-dir -I %t \
// RUN:   -enable-experimental-feature SafeInteropImplementations %t/test.swift -o %t/Test.swiftmodule
// RUN: %target-swift-frontend -emit-silgen -module-name main -plugin-path %swift-plugin-dir -I %t \
// RUN:   -enable-experimental-feature SafeInteropImplementations %t/caller.swift | %FileCheck %s --check-prefix=XSIL
// RUN: %target-swift-frontend -emit-ir -module-name main -plugin-path %swift-plugin-dir -I %t \
// RUN:   -enable-experimental-feature SafeInteropImplementations %t/caller.swift | %FileCheck %s --check-prefix=XIR

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

// SIL-LABEL: sil hidden [ossa] @$s4main6calleryys4SpanVys5Int32VGF :
// SIL:         function_ref @$s4main3fooyys4SpanVys5Int32VGF : $@convention(thin) (@guaranteed Span<Int32>) -> ()
// SIL-NEXT:    apply
// SIL-NOT:     function_ref @$sSC3foo
// SIL-NOT:     convention(c)
// SIL-NOT:     function_ref @foo
// SIL-NOT:     function_ref @$s4main3fooyySPys5Int32VG_ADtF
// SIL:       } // end sil function '$s4main6calleryys4SpanVys5Int32VGF'

// IR-LABEL: define hidden swiftcc void @"$s4main6calleryys4SpanVys5Int32VGF"
// IR:         call swiftcc void @"$s4main3fooyys4SpanVys5Int32VGF"
// IR-NOT:     call {{.*}}@"$sSC3foo
// IR-NOT:     call {{.*}}@foo(
// IR-NOT:     call {{.*}}@"$s4main3fooyySPys5Int32VG_ADtF
// IR:       }

//--- caller.swift
import CHeader
import Test

func caller(_ s: Span<CInt>) {
  foo(s)
}

// XSIL-LABEL: sil hidden [ossa] @$s4main6calleryys4SpanVys5Int32VGF :
// XSIL:         function_ref @$s4Test3fooyys4SpanVys5Int32VGF : $@convention(thin) (@guaranteed Span<Int32>) -> ()
// XSIL-NEXT:    apply
// XSIL-NOT:     function_ref @$sSC3foo
// XSIL-NOT:     convention(c)
// XSIL-NOT:     function_ref @foo
// XSIL:       } // end sil function '$s4main6calleryys4SpanVys5Int32VGF'

// XIR-LABEL: define hidden swiftcc void @"$s4main6calleryys4SpanVys5Int32VGF"
// XIR:         call swiftcc void @"$s4Test3fooyys4SpanVys5Int32VGF"
// XIR-NOT:     call {{.*}}@"$sSC3foo
// XIR-NOT:     call {{.*}}@foo(
// XIR:       }

//--- test.h
#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __noescape __attribute__((noescape))

void foo(const int * _Nonnull __counted_by(len) __noescape x, int len);

//--- module.modulemap
module CHeader {
  header "test.h"
}
