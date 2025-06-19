// RUN: %target-swift-ide-test -Xcc -fexperimental-bounds-safety-attributes -print-module -module-to-print=BoundsAttributedStruct -I %S/Inputs -source-filename=x | %FileCheck %s
// RUN: %target-swift-ide-test -Xcc -fexperimental-bounds-safety-attributes -print-module -module-to-print=BoundsAttributedStruct -I %S/Inputs -source-filename=x -cxx-interoperability-mode=swift-6 -Xcc -std=c++20 | %FileCheck %s
// RUN: %target-swift-ide-test -Xcc -fbounds-safety -disable-objc-interop -print-module -module-to-print=BoundsAttributedStruct -I %S/Inputs -source-filename=x | %FileCheck %s --check-prefixes=CHECK,BOUNDS-SAFETY

// This test case checks that ClangImporter can import declarations using various bounds attributes,
// rather than being marked unavailable because of an unknown type.

// CHECK:      struct a {
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(a: UnsafeMutablePointer<Int{{[0-9]+}}>!, len: Int{{[0-9]+}})
// CHECK-NEXT:   var a: UnsafeMutablePointer<Int{{[0-9]+}}>!
// CHECK-NEXT:   var len: Int{{[0-9]+}}
// CHECK-NEXT: }
// CHECK-NEXT: func a(_: a) -> UnsafeMutablePointer<a>!
// CHECK-NEXT: struct b {
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(a: UnsafeMutablePointer<Int{{[0-9]+}}>!, len: Int{{[0-9]+}})
// CHECK-NEXT:   var a: UnsafeMutablePointer<Int{{[0-9]+}}>!
// CHECK-NEXT:   var len: Int{{[0-9]+}}
// CHECK-NEXT: }
// CHECK-NEXT: func b(_: b) -> UnsafeMutablePointer<b>!
// CHECK-NEXT: struct c {
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(a: UnsafeMutablePointer<CChar>!, len: Int{{[0-9]+}})
// CHECK-NEXT:   var a: UnsafeMutablePointer<CChar>!
// CHECK-NEXT:   var len: Int{{[0-9]+}}
// CHECK-NEXT: }
// CHECK-NEXT: func c(_: c) -> UnsafeMutablePointer<c>!
// CHECK-NEXT: struct d {
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(a: UnsafeMutableRawPointer!, len: Int{{[0-9]+}})
// CHECK-NEXT:   var a: UnsafeMutableRawPointer!
// CHECK-NEXT:   var len: Int{{[0-9]+}}
// CHECK-NEXT: }
// CHECK-NEXT: func d(_: d) -> UnsafeMutablePointer<d>!
// CHECK-NEXT: struct e {
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(a: UnsafeMutableRawPointer!, b: UnsafeMutablePointer<Int{{[0-9]+}}>!)
// CHECK-NEXT:   var a: UnsafeMutableRawPointer!
// CHECK-NEXT:   var b: UnsafeMutablePointer<Int{{[0-9]+}}>!
// CHECK-NEXT: }
// CHECK-NEXT: func e(_: e) -> UnsafeMutablePointer<e>!
// CHECK-NEXT: struct f {
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(a: UnsafePointer<CChar>!, b: UnsafeMutablePointer<CChar>!)
// CHECK-NEXT:   var a: UnsafePointer<CChar>!
// CHECK-NEXT:   var b: UnsafeMutablePointer<CChar>!
// CHECK-NEXT: }
// CHECK-NEXT: func f(_: f) -> UnsafeMutablePointer<f>!

// BOUNDS-SAFETY-NEXT: struct g {
// BOUNDS-SAFETY-NEXT:  init()
// BOUNDS-SAFETY-NEXT:  init(a: UnsafeMutableRawPointer!, b: UnsafeMutablePointer<Int32>!)
// BOUNDS-SAFETY-NEXT:  var a: UnsafeMutableRawPointer!
// BOUNDS-SAFETY-NEXT:  var b: UnsafeMutablePointer<Int32>!
// BOUNDS-SAFETY-NEXT: }
// BOUNDS-SAFETY-NEXT: func g(_: g) -> UnsafeMutablePointer<g>!
// BOUNDS-SAFETY-NEXT: struct h {
// BOUNDS-SAFETY-NEXT:  init()
// BOUNDS-SAFETY-NEXT:  init(a: UnsafeMutableRawPointer!, b: UnsafeMutablePointer<Int32>!)
// BOUNDS-SAFETY-NEXT:  var a: UnsafeMutableRawPointer!
// BOUNDS-SAFETY-NEXT:  var b: UnsafeMutablePointer<Int32>!
// BOUNDS-SAFETY-NEXT: }
// BOUNDS-SAFETY-NEXT: func h(_: h) -> UnsafeMutablePointer<h>!

// CHECK-NEXT: struct i {
// CHECK-NEXT:   init()
// CHECK-NEXT:   var len: Int{{[0-9]+}}
// CHECK-NEXT: }
// CHECK-NEXT: func i(_: UnsafeMutablePointer<i>!) -> UnsafeMutablePointer<i>!
// CHECK-NEXT: var len1: Int{{[0-9]+}} { get }
// CHECK-NEXT: struct j {
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(a: UnsafeMutablePointer<Int{{[0-9]+}}>!, b: UnsafeMutableRawPointer!)
// CHECK-NEXT:   var a: UnsafeMutablePointer<Int{{[0-9]+}}>!
// CHECK-NEXT:   var b: UnsafeMutableRawPointer!
// CHECK-NEXT: }
// CHECK-NEXT: func j(_: j) -> UnsafeMutablePointer<j>!
// CHECK-NEXT: var len2: Int{{[0-9]+}}
// CHECK-NEXT: struct k {
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(a: UnsafeMutablePointer<Int{{[0-9]+}}>!, b: UnsafeMutableRawPointer!)
// CHECK-NEXT:   var a: UnsafeMutablePointer<Int{{[0-9]+}}>!
// CHECK-NEXT:   var b: UnsafeMutableRawPointer!
// CHECK-NEXT: }
// CHECK-NEXT: func k(_: k) -> UnsafeMutablePointer<k>!

// RUN: %target-swift-frontend -Xcc -fexperimental-bounds-safety-attributes -emit-module -plugin-path %swift-plugin-dir -I %S/Inputs %s
// RUN: %target-swift-frontend -Xcc -fexperimental-bounds-safety-attributes -emit-module -plugin-path %swift-plugin-dir -I %S/Inputs %s -cxx-interoperability-mode=swift-6
// RUN: %target-swift-frontend -Xcc -fbounds-safety -disable-objc-interop -emit-module -plugin-path %swift-plugin-dir -I %S/Inputs %s -D BOUNDS_SAFETY

// RUN: %target-swift-frontend -Xcc -fexperimental-bounds-safety-attributes -emit-module -plugin-path %swift-plugin-dir -I %S/Inputs %s -verify -verify-additional-file %S/Inputs/bounds-attributed-struct.h -D VERIFY
// RUN: %target-swift-frontend -Xcc -fexperimental-bounds-safety-attributes -emit-module -plugin-path %swift-plugin-dir -I %S/Inputs %s -cxx-interoperability-mode=swift-6 -verify -verify-additional-file %S/Inputs/bounds-attributed-struct.h -D VERIFY
// RUN: %target-swift-frontend -Xcc -fbounds-safety -disable-objc-interop -emit-module -plugin-path %swift-plugin-dir -I %S/Inputs %s -verify -verify-additional-file %S/Inputs/bounds-attributed-struct.h -D BOUNDS_SAFETY -D VERIFY

import BoundsAttributedStruct

func call(aa: a, bb: b, cc: c, dd: d, ee: e, ff: f, ii: UnsafeMutablePointer<i>, jj: j, kk: k, ll: l, mm: m) {
    let _ = aa.a
    let _ = a(aa)

    let _ = bb.a
    let _ = b(bb)

    let _ = cc.a
    let _ = c(cc)

    let _ = dd.a
    let _ = d(dd)

    let _ = ee.a
    let _ = e(ee)

    let _ = ff.a
    let _ = f(ff)

    let _ = unsafe ii.pointee.len
#if VERIFY
    // rdar://152293598 ([ClangImporter] Importing global array errors on macOS and Linux, but not on Windows)
    // XFAIL: OS=windows-msvc
    // flexible array member not imported rdar://151665752
    let _ = i.a // expected-error{{type 'i' has no member 'a'}}
#endif
    let _ = i(ii)

    let _ = jj.a
    let _ = jj.b
    let _ = j(jj)

    let _ = kk.a
    let _ = kk.b
    let _ = k(kk)

    let _ = ll.a
    let _ = ll.end
    let _ = l(ll)

    let _ = mm.a
    let _ = mm.end
    let _ = m(mm)
}

#if BOUNDS_SAFETY
func boundsSafe(gg: g, hh: h) {
    let _ = gg.a
    let _ = gg.b
    let _ = g(gg)

    let _ = hh.a
    let _ = hh.b
    let _ = h(hh)
}
#endif
