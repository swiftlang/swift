// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen %s -import-objc-header %S/Inputs/objc_error_convention_from_protocol.h | %FileCheck %s
// REQUIRES: objc_interop

import Foundation

class Caller1Impl: Caller1 {
  // declared as - (BOOL)call:(void (^_Nonnull)(void))callback error:(NSError**)error;
  // CHECK-LABEL: sil {{.*}} @$s{{.*}}11Caller1Impl{{.*}}call{{.*}}To :
  // CHECK-SAME: $@convention(objc_method) (@convention(block) () -> (), Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, Caller1Impl) -> ObjCBool
  func call(_ callback: @escaping () -> ()) throws { }
}

class CallerAImpl: CallerA {
  // declared as - (BOOL)use:(NSInteger)x thenCall:(void (^_Nonnull)(void))callback error:(NSError**)error;
  // CHECK-LABEL: sil {{.*}} @$s{{.*}}11CallerAImpl{{.*}}use{{.*}}thenCall{{.*}}To :
  // CHECK-SAME: $@convention(objc_method) (Int, @convention(block) () -> (), Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, CallerAImpl) -> ObjCBool
  func use(_ x: Int, thenCall callback: @escaping () -> ()) throws { }
}

class CallerBImpl: CallerB {
  // declared as - (BOOL)use:(NSInteger)x error:(NSError**)error thenCall:(void (^_Nonnull)(void))callback;
  // CHECK-LABEL: sil {{.*}} @$s{{.*}}11CallerBImpl{{.*}}use{{.*}}thenCall{{.*}}To :
  // CHECK-SAME: $@convention(objc_method) (Int, Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, @convention(block) () -> (), CallerBImpl) -> ObjCBool
  func use(_ x: Int, thenCall callback: @escaping () -> ()) throws { }
}

