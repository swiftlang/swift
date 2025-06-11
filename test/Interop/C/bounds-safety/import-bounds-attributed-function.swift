// RUN: %target-swift-ide-test -Xcc -fexperimental-bounds-safety-attributes -print-module -module-to-print=BoundsAttributedFunction -I %S/Inputs -source-filename=x | %FileCheck %s --check-prefixes=CHECK,C-ONLY
// RUN: %target-swift-ide-test -Xcc -fexperimental-bounds-safety-attributes -print-module -module-to-print=BoundsAttributedFunction -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default | %FileCheck %s
// RUN: %target-swift-ide-test -Xcc -fbounds-safety -disable-objc-interop -print-module -module-to-print=BoundsAttributedFunction -I %S/Inputs -source-filename=x | %FileCheck %s --check-prefixes=CHECK,BOUNDS-SAFETY,C-ONLY

// This test case checks that ClangImporter can import declarations using various bounds attributes,
// rather than being marked unavailable because of an unknown type.

// CHECK:      func a(_ p: UnsafeMutablePointer<Int{{[0-9]+}}>!, _ len: Int{{[0-9]+}}) -> UnsafeMutablePointer<Int{{[0-9]+}}>!
// CHECK-NEXT: func b(_ p: UnsafeMutablePointer<CChar>!, _ len: Int{{[0-9]+}}) -> UnsafeMutablePointer<CChar>!
// CHECK-NEXT: func c(_ p: UnsafeMutablePointer<CChar>!, _ len: Int{{[0-9]+}}) -> UnsafeMutablePointer<CChar>!
// CHECK-NEXT: func d(_ p: UnsafeMutableRawPointer!, _ len: Int{{[0-9]+}}) -> UnsafeMutableRawPointer!
// CHECK-NEXT: func e(_ p: UnsafeMutablePointer<Int{{[0-9]+}}>!, _ len: Int{{[0-9]+}}) -> UnsafeMutablePointer<Int{{[0-9]+}}>!
// CHECK-NEXT: func f(_ p: UnsafeMutablePointer<Int{{[0-9]+}}>!, _ end: UnsafeMutablePointer<Int{{[0-9]+}}>!) -> UnsafeMutablePointer<Int{{[0-9]+}}>!
// CHECK-NEXT: func g(_ p: UnsafeMutableRawPointer!, _ end: UnsafeMutableRawPointer!) -> UnsafeMutableRawPointer!
// CHECK-NEXT: func h(_ p: UnsafeMutablePointer<CChar>!) -> UnsafeMutablePointer<CChar>!
// CHECK-NEXT: func i(_ p: UnsafePointer<CChar>!) -> UnsafePointer<CChar>!
// CHECK-NEXT: func j(_ p: UnsafeMutablePointer<CChar>!) -> UnsafeMutablePointer<CChar>!
// CHECK-NEXT: func k(_ p: UnsafeMutableRawPointer!) -> UnsafeMutableRawPointer!

// BOUNDS-SAFETY-NEXT: func l(_ p: UnsafeMutablePointer<CChar>!) -> UnsafeMutablePointer<CChar>!
// BOUNDS-SAFETY-NEXT: func m(_ p: UnsafeMutableRawPointer!) -> UnsafeMutableRawPointer!
// BOUNDS-SAFETY-NEXT: func n(_ p: UnsafeMutablePointer<CChar>!) -> UnsafeMutablePointer<CChar>!
// BOUNDS-SAFETY-NEXT: func o(_ p: UnsafeMutableRawPointer!) -> UnsafeMutableRawPointer!

// C-ONLY-NEXT: func p(_ len: Int{{[0-9]+}}, _ p: UnsafeMutablePointer<Int{{[0-9]+}}>!)

// CHECK-NEXT: func q(_ p: UnsafeMutablePointer<Int{{[0-9]+}}>!, _ len: Int{{[0-9]+}})
// CHECK-NEXT: func r(_ p: UnsafeMutablePointer<UnsafeMutablePointer<Int{{[0-9]+}}>?>!, _ len: UnsafeMutablePointer<Int{{[0-9]+}}>!)
// CHECK-NEXT: func s(_ p: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>!) -> UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>!
// CHECK-NEXT: func t(_ p: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>!) -> UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>!
// CHECK-NEXT: let len1: Int{{[0-9]+}}
// CHECK-NEXT: func u(_ p: UnsafeMutablePointer<Int{{[0-9]+}}>!) -> UnsafeMutablePointer<Int{{[0-9]+}}>!
// CHECK-NEXT: var len2: Int{{[0-9]+}}
// CHECK-NEXT: func v(_ p: UnsafeMutablePointer<Int{{[0-9]+}}>!) -> UnsafeMutablePointer<Int{{[0-9]+}}>!


// RUN: %target-swift-frontend -Xcc -fexperimental-bounds-safety-attributes -emit-module -plugin-path %swift-plugin-dir -I %S/Inputs %s -D C_ONLY
// RUN: %target-swift-frontend -Xcc -fexperimental-bounds-safety-attributes -emit-module -plugin-path %swift-plugin-dir -I %S/Inputs %s -cxx-interoperability-mode=default
// RUN: %target-swift-frontend -Xcc -fbounds-safety -disable-objc-interop -emit-module -plugin-path %swift-plugin-dir -I %S/Inputs %s -D C_ONLY -D BOUNDS_SAFETY

import BoundsAttributedFunction

func call(_ mutIntPtr: UnsafeMutablePointer<CInt>,
          _ mutCharPtr: UnsafeMutablePointer<CChar>,
          _ mutRawPtr: UnsafeMutableRawPointer,
          _ constCharPtr: UnsafePointer<CChar>,
          _ mutMutIntPtrPtr: UnsafeMutablePointer<UnsafeMutablePointer<CInt>?>!,
          _ mutMutCharPtrPtr: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>!,
          _ int: CInt,
          _ args: CVaListPointer) {
    let _ = a(mutIntPtr, int)
    let _ = b(mutCharPtr, int)
    let _ = c(mutCharPtr, int)
    let _ = d(mutRawPtr, int)
    let _ = e(mutIntPtr, int)
    let _ = f(mutIntPtr, mutIntPtr)
    let _ = g(mutRawPtr, mutRawPtr)
    let _ = h(mutCharPtr)
    let _ = i(constCharPtr)
    let _ = j(mutCharPtr)
    let _ = k(mutRawPtr)

#if BOUNDS_SAFETY
    let _ = l(mutIntPtr)
    let _ = m(mutRawPtr)
    let _ = n(mutIntPtr)
    let _ = o(mutRawPtr)
#endif

#if C_ONLY
    let _ = p(int, mutIntPtr)
#endif

    let _ = q(mutIntPtr, int)
    let _ = r(mutMutIntPtrPtr, mutIntPtr)
    let _ = s(mutMutCharPtrPtr)
    let _ = t(mutMutCharPtrPtr)
    let _ = len1
    let _ = u(mutIntPtr)
    let _ = len2
    len2 = 37
    let _ = v(mutIntPtr)

    let _ = w(args)
}
