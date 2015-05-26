// RUN: rm -rf %t && mkdir %t
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -primary-file %s -emit-ir | FileCheck %s

// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

import Foundation
import gizmo

// CHECK: [[NSRECT:%VSC6NSRect]] = type <{ [[NSPOINT:%VSC7NSPoint]], [[NSSIZE:%VSC6NSSize]] }>
// CHECK: [[NSPOINT]] = type <{ [[DOUBLE:%Sd]], [[DOUBLE]] }>
// CHECK: [[DOUBLE]] = type <{ double }>
// CHECK: [[NSSIZE]] = type <{ [[DOUBLE]], [[DOUBLE]] }>
// CHECK: [[GIZMO:%CSo5Gizmo]] = type opaque
// CHECK: [[NSSTRING:%CSo8NSString]] = type opaque
// CHECK: [[NSVIEW:%CSo6NSView]] = type opaque

// CHECK: define hidden void @_TF12objc_structs8getFrame{{.*}}([[NSRECT]]* noalias sret, [[GIZMO]]*) {{.*}} {
func getFrame(g: Gizmo) -> NSRect {
  // CHECK: load i8*, i8** @"\01L_selector(frame)"
  // CHECK: call void bitcast (void ()* @objc_msgSend_stret to void ([[NSRECT]]*, [[OPAQUE0:.*]]*, i8*)*)([[NSRECT]]* noalias sret {{.*}}, [[OPAQUE0:.*]]* {{.*}}, i8* {{.*}})
  return g.frame()
}
// CHECK: }

// CHECK: define hidden void @_TF12objc_structs8setFrame{{.*}}(%CSo5Gizmo*, %VSC6NSRect* byval align {{4|8}}) {{.*}} {
func setFrame(g: Gizmo, frame: NSRect) {
  // CHECK: load i8*, i8** @"\01L_selector(setFrame:)"
  // CHECK: call void bitcast (void ()* @objc_msgSend to void ([[OPAQUE0:.*]]*, i8*, [[NSRECT]]*)*)([[OPAQUE0:.*]]* {{.*}}, i8* {{.*}}, [[NSRECT]]* byval align 8 {{.*}})
  g.setFrame(frame)
}
// CHECK: }

// CHECK: define hidden void @_TF12objc_structs8makeRect{{.*}}([[NSRECT]]* noalias sret, double, double, double, double)
func makeRect(a: Double, b: Double, c: Double, d: Double) -> NSRect {
  // CHECK: call void @NSMakeRect([[NSRECT]]* noalias sret {{.*}}, double {{.*}}, double {{.*}}, double {{.*}}, double {{.*}})
  return NSMakeRect(a,b,c,d)
}
// CHECK: }

// CHECK: define hidden [[stringLayout:[^@]*]] @_TF12objc_structs14stringFromRect{{.*}}(%VSC6NSRect* byval align {{4|8}}) {{.*}} {
func stringFromRect(r: NSRect) -> String {
  // CHECK: call [[OPAQUE0:.*]]* @NSStringFromRect([[NSRECT]]* byval align 8 {{.*}})
  return NSStringFromRect(r)
}
// CHECK: }

// CHECK: define hidden void @_TF12objc_structs9insetRect{{.*}}([[NSRECT]]* noalias sret, %VSC6NSRect* byval align {{4|8}}, double, double)
func insetRect(r: NSRect, x: Double, y: Double) -> NSRect {
  // CHECK: call void @NSInsetRect([[NSRECT]]* noalias sret {{.*}}, [[NSRECT]]* byval align 8 {{.*}}, double {{.*}}, double {{.*}})
  return NSInsetRect(r, x, y)
}
// CHECK: }

// CHECK: define hidden void @_TF12objc_structs19convertRectFromBase{{.*}}([[NSRECT]]* noalias sret, [[NSVIEW]]*, [[NSRECT]]* byval align {{4|8}})
func convertRectFromBase(v: NSView, r: NSRect) -> NSRect {
  // CHECK: load i8*, i8** @"\01L_selector(convertRectFromBase:)", align 8
  // CHECK: call void bitcast (void ()* @objc_msgSend_stret to void ([[NSRECT]]*, [[OPAQUE0:.*]]*, i8*, [[NSRECT]]*)*)([[NSRECT]]* noalias sret {{.*}}, [[OPAQUE0:.*]]* {{.*}}, i8* {{.*}}, [[NSRECT]]* byval align 8 {{.*}})
  return v.convertRectFromBase(r)
}
// CHECK: }

// CHECK: define hidden void @_TF12objc_structs20useStructOfNSStringsFVSC17StructOfNSStringsS0_(%VSC17StructOfNSStrings* noalias sret, %VSC17StructOfNSStrings* byval align 8)
// CHECK:   call void @useStructOfNSStringsInObjC(%VSC17StructOfNSStrings* noalias sret {{%.*}}, %VSC17StructOfNSStrings* byval align 8 {{%.*}})
func useStructOfNSStrings(s: StructOfNSStrings) -> StructOfNSStrings {
  return useStructOfNSStringsInObjC(s)
}
