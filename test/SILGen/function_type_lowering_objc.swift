// RUN: %empty-directory(%t)
// RUN: %build-clang-importer-objc-overlays
// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk-nosource -I %t) -module-name main %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: sil {{.*}}3foo{{.*}} : $@convention(thin) <T where T : NSCopying> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0 where τ_0_0 : AnyObject> (@guaranteed τ_0_0) -> ()
func foo<T: NSCopying>(f: (T) -> ()) {}
// CHECK-LABEL: sil {{.*}}3bar{{.*}} : $@convention(thin) <T where T : NSCopying> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0 where τ_0_0 : AnyObject> (@guaranteed Optional<τ_0_0>) -> ()
func bar<T: NSCopying>(f: (T?) -> ()) {}
