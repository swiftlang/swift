// RUN: %empty-directory(%t)
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -primary-file %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

import Foundation
import gizmo

// CHECK: [[NSRECT:%TSo6NSRectV]] = type <{ [[NSPOINT:%TSo7NSPointV]], [[NSSIZE:%TSo6NSSizeV]] }>
// CHECK: [[NSPOINT]] = type <{ [[DOUBLE:%TSd]], [[DOUBLE]] }>
// CHECK: [[DOUBLE]] = type <{ double }>
// CHECK: [[NSSIZE]] = type <{ [[DOUBLE]], [[DOUBLE]] }>

// CHECK: define hidden swiftcc { double, double, double, double } @"$s12objc_structs8getFrame{{[_0-9a-zA-Z]*}}F"(ptr %0) {{.*}} {
func getFrame(_ g: Gizmo) -> NSRect {
  // CHECK: load ptr, ptr @"\01L_selector(frame)"
  // CHECK: call void @objc_msgSend_stret(ptr noalias{{( nocapture)?}} sret({{.*}}){{( captures\(none\))?}} {{.*}}, ptr {{.*}}, ptr {{.*}})
  return g.frame()
}
// CHECK: }

// CHECK: define hidden swiftcc void @"$s12objc_structs8setFrame{{[_0-9a-zA-Z]*}}F"(ptr %0, double %1, double %2, double %3, double %4) {{.*}} {
func setFrame(_ g: Gizmo, frame: NSRect) {
  // CHECK: load ptr, ptr @"\01L_selector(setFrame:)"
  // CHECK: call void @objc_msgSend(ptr {{.*}}, ptr {{.*}}, ptr byval({{.*}}) align 8 {{.*}})
  g.setFrame(frame)
}
// CHECK: }

// CHECK: define hidden swiftcc { double, double, double, double } @"$s12objc_structs8makeRect{{[_0-9a-zA-Z]*}}F"(double %0, double %1, double %2, double %3)
func makeRect(_ a: Double, b: Double, c: Double, d: Double) -> NSRect {
  // CHECK: call void @NSMakeRect(ptr noalias{{( nocapture)?}} sret({{.*}}){{( captures\(none\))?}} {{.*}}, double {{.*}}, double {{.*}}, double {{.*}}, double {{.*}})
  return NSMakeRect(a,b,c,d)
}
// CHECK: }

// CHECK: define hidden swiftcc [[stringLayout:[^@]*]] @"$s12objc_structs14stringFromRect{{[_0-9a-zA-Z]*}}F"(double %0, double %1, double %2, double %3) {{.*}} {
func stringFromRect(_ r: NSRect) -> String {
  // CHECK: call ptr @NSStringFromRect(ptr byval({{.*}}) align 8 {{.*}})
  return NSStringFromRect(r)
}
// CHECK: }

// CHECK: define hidden swiftcc { double, double, double, double } @"$s12objc_structs9insetRect{{[_0-9a-zA-Z]*}}F"(double %0, double %1, double %2, double %3, double %4, double %5)
func insetRect(_ r: NSRect, x: Double, y: Double) -> NSRect {
  // CHECK: call void @NSInsetRect(ptr noalias{{( nocapture)?}} sret({{.*}}){{( captures\(none\))?}} {{.*}}, ptr byval({{.*}}) align 8 {{.*}}, double {{.*}}, double {{.*}})
  return NSInsetRect(r, x, y)
}
// CHECK: }

// CHECK: define hidden swiftcc { double, double, double, double } @"$s12objc_structs19convertRectFromBase{{[_0-9a-zA-Z]*}}F"(ptr %0, double %1, double %2, double %3, double %4)
func convertRectFromBase(_ v: NSView, r: NSRect) -> NSRect {
  // CHECK: load ptr, ptr @"\01L_selector(convertRectFromBase:)", align 8
  // CHECK: call void @objc_msgSend_stret(ptr noalias{{( nocapture)?}} sret({{.*}}){{( captures\(none\))?}} {{.*}}, ptr {{.*}}, ptr {{.*}}, ptr byval({{.*}}) align 8 {{.*}})
  return v.convertRect(fromBase: r)
}
// CHECK: }

// CHECK: define hidden swiftcc { ptr, ptr, ptr, ptr } @"$s12objc_structs20useStructOfNSStringsySo0deF0VADF"(ptr %0, ptr %1, ptr %2, ptr %3)
// CHECK:   call void @useStructOfNSStringsInObjC(ptr noalias{{( nocapture)?}} sret({{.*}}){{( captures\(none\))?}} {{%.*}}, ptr byval({{.*}}) align 8 {{%.*}})
func useStructOfNSStrings(_ s: StructOfNSStrings) -> StructOfNSStrings {
  return useStructOfNSStringsInObjC(s)
}
