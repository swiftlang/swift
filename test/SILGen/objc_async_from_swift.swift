// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -I %S/Inputs/custom-modules -enable-experimental-concurrency %s -verify | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-%target-cpu %s
// REQUIRES: objc_interop

import Foundation
import ObjCConcurrency

@objc protocol SlowServing {
    func requestInt() async -> Int
    func requestString() async -> String
    func requestIntAndString() async -> (Int, String)
    func tryRequestIntAndString() async throws -> (Int, String)
}

// CHECK-LABEL: sil {{.*}}@{{.*}}15testSlowServing
func testSlowServing(p: SlowServing) async throws {
    // CHECK: objc_method {{.*}} $@convention(objc_method) <τ_0_0 where τ_0_0 : SlowServing> (@convention(block) (Int) -> (), τ_0_0) -> ()
    let _: Int = await p.requestInt()
    // CHECK: objc_method {{.*}} $@convention(objc_method) <τ_0_0 where τ_0_0 : SlowServing> (@convention(block) (NSString) -> (), τ_0_0) -> ()
    let _: String = await p.requestString()
    // CHECK: objc_method {{.*}} $@convention(objc_method) <τ_0_0 where τ_0_0 : SlowServing> (@convention(block) (Int, NSString) -> (), τ_0_0) -> ()
    let _: (Int, String) = await p.requestIntAndString()
    // CHECK: objc_method {{.*}} $@convention(objc_method) <τ_0_0 where τ_0_0 : SlowServing> (@convention(block) (Int, Optional<NSString>, Optional<NSError>) -> (), τ_0_0) -> ()
    let _: (Int, String) = await try p.tryRequestIntAndString()
}

/*
class SlowSwiftServer: NSObject, SlowServing {
    func requestInt() async -> Int { return 0 }
    func requestString() async -> String { return "" }
    func requestIntAndString() async -> (Int, String) { return (0, "") }
    func tryRequestIntAndString() async throws -> (Int, String) { return (0, "") }
}
*/

protocol NativelySlowServing {
    func doSomethingSlow(_: String) async -> Int
    func findAnswer() async throws -> String
    func serverRestart(_: String) async
    func findMultipleAnswers() async throws -> (String, Int)
}

extension SlowServer: NativelySlowServing {}
