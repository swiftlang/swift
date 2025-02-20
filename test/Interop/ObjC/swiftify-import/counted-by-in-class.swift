// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-ide-test -plugin-path %swift-plugin-dir -I %t/Inputs -enable-experimental-feature SafeInteropWrappers -print-module -module-to-print=Method -source-filename=x | %FileCheck %s
// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -I %t/Inputs -enable-experimental-feature SafeInteropWrappers %t/method.swift -dump-macro-expansions -typecheck -verify

// REQUIRES: objc_interop

// CHECK:      class Foo {
// CHECK-NEXT:   class func swiftAttr(_ p: UnsafeMutablePointer<Float>!, count len: Int32)
// CHECK-NEXT:   @_alwaysEmitIntoClient public func swiftAttr(_ p: UnsafeMutableBufferPointer<Float>)
// CHECK-NEXT:   func swiftAttr(_ p: UnsafeMutablePointer<Float>!, count len: Int32)
// CHECK-NEXT: }

//--- Inputs/module.modulemap
module Method {
    header "method.h"
}

//--- Inputs/method.h

@interface Foo
-(void)swiftAttr:(float *)p count:(int)len __attribute__((swift_attr("@_SwiftifyImport(.countedBy(pointer: .param(1), count: \"len\"))")));
@end

#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __noescape __attribute__((__noescape__))
#define __lifetimebound __attribute__((__lifetimebound__))

@interface Bar
 - (void) simple:(int)len :(int * __counted_by(len))p;
 - (void) shared:(int)len :(int * __counted_by(len))p1 :(int * __counted_by(len))p2;
 - (void) complexExpr:(int)len :(int) offset :(int * __counted_by(len - offset))p;
 - (void) nullUnspecified:(int)len :(int * __counted_by(len) _Null_unspecified)p;
 - (void) nonnull:(int)len :(int * __counted_by(len) _Nonnull)p;
 - (void) nullable:(int)len :(int * __counted_by(len) _Nullable)p;
 - (int * __counted_by(len)) returnPointer:(int)len;

 + (void) staticMethod:(int)len :(int * __counted_by(len))p;
@end

//--- method.swift
import Method

func testFoo(foo: Foo, s: UnsafeMutableBufferPointer<Float>) {
  foo.swiftAttr(s)
}

public func testBar(p: UnsafeMutableBufferPointer<CInt>, x: CInt, y: CInt, a: Bar) {
  a.simple(p)
  a.shared(x, p, p)
  a.complexExpr(x, y, p)
  a.nullUnspecified(p)
  a.nonnull(p)
  a.nullable(p)
  let _: UnsafeMutableBufferPointer<CInt> = a.returnPointer(x)
  let r = a.returnPointer(x)
  let _: UnsafeMutablePointer<CInt>? = r // make sure the original is the favored overload
}
