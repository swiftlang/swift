// RUN: %target-swift-frontend -emit-silgen -emit-verbose-sil %s | FileCheck %s

// Test top-level/module locations.
class TopLevelObjectTy {
  init() { }

  deinit {
  }
}
var topLevelObject:TopLevelObjectTy

class TopLevelObjectTyWithoutDestructor {
  init() {
  }
}
var topLevelObject2:TopLevelObjectTyWithoutDestructor

// CHECK-LABEL: sil @main
// CHECK: integer_literal ${{.*}}, 0  {{.*}} top_level
// CHECK: return    {{.*}} top_level

// Check default constructor
// CHECK-LABEL: sil_locations_top_level.TopLevelObjectTy.init
// CHECK: bb
// CHECK: return {{.*}}// {{.*}} line:5:12

// Check allocating initializer
// CHECK-LABEL: sil_locations_top_level.TopLevelObjectTy.__allocating_init
// CHECK: sil hidden @_TFC23sil_locations_top_level16TopLevelObjectTyCfMS0_FT_S0_
// CHECK: alloc_ref {{.*}}line:5:3:auto_gen
// CHECK: function_ref

// Check explicit destructor
// CHECK_LABEL: sil hidden @_TFC23sil_locations_top_level16TopLevelObjectTyd
// CHECK:   return {{.*}}// {{.*}} line:8:3

// Check explicit constructor
// FIXME: The ConstructorDecl location is wrong here (looks like it's wrong in the AST).
// CHECK-LABEL: sil hidden @_TFC23sil_locations_top_level33TopLevelObjectTyWithoutDestructorcfMS0_FT_S0_
// CHECK: return {{.*}}// {{.*}} line:14:3:imp_return


// Check allocating constructor
// CHECK-LABEL: sil_locations_top_level.TopLevelObjectTyWithoutDestructor.__allocating_init
// CHECK: return {{.*}}// {{.*}} line:14:3:imp_return:auto_gen

// Check implicit destructor
// CHECK_LABEL: sil hidden @_TFC23sil_locations_top_level33TopLevelObjectTyWithoutDestructord
// CHECK:   return {{.*}}// {{.*}} line:12:7:imp_return:auto_gen
