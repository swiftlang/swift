// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -triple x86_64-apple-darwin10 -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-llvm | FileCheck %s
import gizmo

// CHECK: [[NSRECT:%V6NSRect]] = type { [[NSPOINT:%V7NSPoint]], [[NSSIZE:%V6NSSize]] }
// CHECK: [[NSPOINT]] = type { [[DOUBLE:%Sd]], [[DOUBLE]] }
// CHECK: [[DOUBLE]] = type { double }
// CHECK: [[NSSIZE]] = type { [[DOUBLE]], [[DOUBLE]] }
// CHECK: [[GIZMO:%CSo5Gizmo]] = type opaque
// CHECK: [[NSSTRING:%CSo8NSString]] = type opaque

// CHECK: define void @_T12objc_structs8getFrameFT1gCSo5Gizmo_V6NSRect([[NSRECT]]* noalias sret, [[GIZMO]]* {{.*}}) {
func getFrame(g:Gizmo) -> NSRect {
  // CHECK: load i8** @"\01L_selector(frame)"
  // CHECK: call void bitcast (void ()* @objc_msgSend_stret to void ([[NSRECT]]*, [[GIZMO]]*, i8*)*)([[NSRECT]]* noalias sret {{.*}}, [[GIZMO]]* {{.*}}, i8* {{.*}})
  return g.frame()
}
// CHECK: }

// CHECK: define void @_T12objc_structs8setFrameFT1gCSo5Gizmo5frameV6NSRect_T_(%CSo5Gizmo* {{.*}}, double {{.*}}, double {{.*}}, double {{.*}}, double {{.*}}) {
func setFrame(g:Gizmo, frame:NSRect) {
  // CHECK: load i8** @"\01L_selector(setFrame:)"
  // CHECK: call void bitcast (void ()* @objc_msgSend to void ([[GIZMO]]*, i8*, [[NSRECT]]*)*)([[GIZMO]]* {{.*}}, i8* {{.*}}, [[NSRECT]]* align 8 byval {{.*}})
  g.setFrame(frame)
}
// CHECK: }

// CHECK: define void @_T12objc_structs8makeRectFT1aSd1bSd1cSd1dSd_V6NSRect([[NSRECT]]* noalias sret, double {{.*}}, double {{.*}}, double {{.*}}, double {{.*}})
func makeRect(a:Double, b:Double, c:Double, d:Double) -> NSRect {
  // CHECK: call void @NSMakeRect([[NSRECT]]* noalias sret {{.*}}, double {{.*}}, double {{.*}}, double {{.*}}, double {{.*}})
  return NSMakeRect(a,b,c,d)
}

// CHECK: define [[NSSTRING]]* @_T12objc_structs14stringFromRectFT1rV6NSRect_CSo8NSString(double %r.0, double %r.1, double %r.2, double %r.3)
func stringFromRect(r:NSRect) -> NSString {
  // TODO use C convention for imported decls
  // C/HECK: call [[NSSTRING]]* @NSStringFromRect([[NSRECT]]* align 8 byval {{.*}})
  return NSStringFromRect(r)
}
