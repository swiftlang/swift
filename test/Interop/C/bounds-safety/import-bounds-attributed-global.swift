// RUN: %target-swift-ide-test -Xcc -fexperimental-bounds-safety-attributes -print-module -module-to-print=BoundsAttributedGlobal -I %S/Inputs -source-filename=x | %FileCheck %s --check-prefixes=CHECK,C-ONLY
// RUN: %target-swift-ide-test -Xcc -fexperimental-bounds-safety-attributes -print-module -module-to-print=BoundsAttributedGlobal -I %S/Inputs -source-filename=x -cxx-interoperability-mode=swift-6 -Xcc -std=c++20 | %FileCheck %s
// RUN: %target-swift-ide-test -Xcc -fbounds-safety -disable-objc-interop -print-module -module-to-print=BoundsAttributedGlobal -I %S/Inputs -source-filename=x | %FileCheck %s --check-prefixes=CHECK,BOUNDS-SAFETY,C-ONLY

// This test case checks that ClangImporter can import declarations using various bounds attributes,
// rather than being marked unavailable because of an unknown type.

// CHECK:      var len: Int32
// CHECK-NEXT: var a: <<error type>>
// CHECK-NEXT: var b: UnsafePointer<CChar>!
// CHECK-NEXT: var c: UnsafeMutablePointer<UnsafeMutablePointer<Int32>?>!
// BOUNDS-SAFETY-NEXT: var d: UnsafeMutablePointer<Int32>!


// RUN: %target-swift-frontend -Xcc -fexperimental-bounds-safety-attributes -emit-module -plugin-path %swift-plugin-dir -I %S/Inputs %s
// RUN: %target-swift-frontend -Xcc -fexperimental-bounds-safety-attributes -emit-module -plugin-path %swift-plugin-dir -I %S/Inputs %s -cxx-interoperability-mode=swift-6
// RUN: %target-swift-frontend -Xcc -fbounds-safety -disable-objc-interop -emit-module -plugin-path %swift-plugin-dir -I %S/Inputs %s -D BOUNDS_SAFETY

// RUN: %target-swift-frontend -Xcc -fexperimental-bounds-safety-attributes -emit-module -plugin-path %swift-plugin-dir -I %S/Inputs %s -verify -verify-additional-file %S/Inputs/bounds-attributed-global.h -D VERIFY
// RUN: %target-swift-frontend -Xcc -fexperimental-bounds-safety-attributes -emit-module -plugin-path %swift-plugin-dir -I %S/Inputs %s -cxx-interoperability-mode=swift-6 -verify -verify-additional-file %S/Inputs/bounds-attributed-global.h -D VERIFY
// RUN: %target-swift-frontend -Xcc -fbounds-safety -disable-objc-interop -emit-module -plugin-path %swift-plugin-dir -I %S/Inputs %s -verify -verify-additional-file %S/Inputs/bounds-attributed-global.h -D BOUNDS_SAFETY -D VERIFY

import BoundsAttributedGlobal

func access() {
#if VERIFY
// rdar://152293598 ([ClangImporter] Importing global array errors on macOS and Linux, but not on Windows)
// XFAIL: OS=windows-msvc
    let _ = a // expected-error{{cannot reference invalid declaration 'a'}} rdar://151665752
#endif
    let _ = b.pointee
    let _ = c.pointee!.pointee
#if BOUNDS_SAFETY
    let _ = d.pointee
#endif
}
