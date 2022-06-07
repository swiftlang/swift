// RUN: %target-swift-emit-silgen(mock-sdk: -sdk %S/Inputs) %s | %FileCheck %s
// REQUIRES: objc_interop

import Foundation

@objc public class Horse : NSObject {
  @_alwaysEmitIntoClient @objc public dynamic var height: Int { 14 }
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s28always_emit_into_client_objc5HorseC6heightSivgTo : $@convention(objc_method) (Horse) -> Int {
// CHECK-LABEL: sil non_abi [serialized] [ossa] @$s28always_emit_into_client_objc5HorseC6heightSivg : $@convention(method) (@guaranteed Horse) -> Int {
