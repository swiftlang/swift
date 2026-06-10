// RUN: %target-swift-emit-sil -I %S/Inputs -cxx-interoperability-mode=default -disable-availability-checking -diagnostic-style llvm %s -validate-tbd-against-ir=none -Xcc -fignore-exceptions | %FileCheck %s

import FRTReferenceReturns

// Test that the ownership annotations work correctly for reference returns

func useAll() {
  let _ = NoAnnotations.getRefCountedByRef()
  // CHECK: function_ref @{{.*}} : $@convention(c) () -> NoAnnotations.RefCountedType
  // Note: create/copy rule (default without ownership annotations) does not apply if return type is & to frt
  let _ = NoAnnotations.createRefCountedByRef()
  // CHECK: function_ref @{{.*}} : $@convention(c) () -> NoAnnotations.RefCountedType
  let _ = NoAnnotations.copyRefCountedByRef()
  // CHECK: function_ref @{{.*}} : $@convention(c) () -> NoAnnotations.RefCountedType

  // APIAnnotations - explicit ownership
  let _ = APIAnnotations.createByRef()
  // CHECK: function_ref @{{.*}} : $@convention(c) () -> @owned APIAnnotations.RefCountedType
  let _ = APIAnnotations.getByRef()
  // CHECK: function_ref @{{.*}} : $@convention(c) () -> APIAnnotations.RefCountedType

  // TypeAnnotation - all unretained due to type default
  let _ = TypeAnnotation.getByRef()
  // CHECK: function_ref @{{.*}} : $@convention(c) () -> TypeAnnotation.RefCountedType
  let _ = TypeAnnotation.createByRef()
  // CHECK: function_ref @{{.*}} : $@convention(c) () -> TypeAnnotation.RefCountedType
  let _ = TypeAnnotation.copyByRef()
  // CHECK: function_ref @{{.*}} : $@convention(c) () -> TypeAnnotation.RefCountedType

  // BothAnnotations - API overrides type
  let _ = BothAnnotations.createByRef()
  // CHECK: function_ref @{{.*}} : $@convention(c) () -> @owned BothAnnotations.RefCountedType
  let _ = BothAnnotations.getByRef()
  // CHECK: function_ref @{{.*}} : $@convention(c) () -> BothAnnotations.RefCountedType
}

// CHECK: sil {{.*}}[clang NoAnnotations.getRefCountedByRef] @{{.*}} : $@convention(c) () -> NoAnnotations.RefCountedType
// CHECK: sil {{.*}}[clang NoAnnotations.createRefCountedByRef] @{{.*}} : $@convention(c) () -> NoAnnotations.RefCountedType
// CHECK: sil {{.*}}[clang NoAnnotations.copyRefCountedByRef] @{{.*}} : $@convention(c) () -> NoAnnotations.RefCountedType
// CHECK: sil {{.*}}[clang APIAnnotations.createByRef] @{{.*}} : $@convention(c) () -> @owned APIAnnotations.RefCountedType
// CHECK: sil {{.*}}[clang APIAnnotations.getByRef] @{{.*}} : $@convention(c) () -> APIAnnotations.RefCountedType
// CHECK: sil {{.*}}[clang TypeAnnotation.getByRef] @{{.*}} : $@convention(c) () -> TypeAnnotation.RefCountedType
// CHECK: sil {{.*}}[clang TypeAnnotation.createByRef] @{{.*}} : $@convention(c) () -> TypeAnnotation.RefCountedType
// CHECK: sil {{.*}}[clang TypeAnnotation.copyByRef] @{{.*}} : $@convention(c) () -> TypeAnnotation.RefCountedType
// CHECK: sil {{.*}}[clang BothAnnotations.createByRef] @{{.*}} : $@convention(c) () -> @owned BothAnnotations.RefCountedType
// CHECK: sil {{.*}}[clang BothAnnotations.getByRef] @{{.*}} : $@convention(c) () -> BothAnnotations.RefCountedType
