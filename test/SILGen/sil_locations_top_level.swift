// RUN: %swift -emit-silgen -emit-verbose-sil %s | FileCheck %s

// Test top-level/module locations.
class TopLevelObjectTy {
  destructor {
  }
}
var topLevelObject:TopLevelObjectTy

// CHECK-LABEL: sil internal @top_level_code
// tuple ()  {{.*}} top_level
// return    {{.*}} top_level
