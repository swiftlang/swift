// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -I %S/Inputs/custom-modules -enable-experimental-concurrency %s -verify | %FileCheck %s
// REQUIRES: objc_interop

import Foundation
import ObjCConcurrency

// CHECK-LABEL: sil {{.*}}@${{.*}}14testSlowServer
func testSlowServer(slowServer: SlowServer) async throws {
  // CHECK: objc_method {{.*}} $@convention(objc_method) (NSString, @convention(block) (Int) -> (), SlowServer) -> ()
  let _: Int = await slowServer.doSomethingSlow("mail")
  // CHECK: objc_method {{.*}} $@convention(objc_method) (@convention(block) (Optional<NSString>, Optional<NSError>) -> (), SlowServer) -> ()
  let _: String? = try await slowServer.findAnswer()

  // CHECK: objc_method {{.*}} $@convention(objc_method) (NSString, @convention(block) () -> (), SlowServer) -> ()
  await slowServer.serverRestart("somewhere")
}
