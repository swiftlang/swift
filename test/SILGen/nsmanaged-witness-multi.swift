// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -module-name main -Xllvm -sil-print-types -emit-silgen -sdk %S/Inputs -primary-file %s %S/Inputs/nsmanaged-witness-multi-other.swift -I %S/Inputs -I %t -enable-source-import | %FileCheck %s

// RUN: %target-swift-frontend -module-name main -Xllvm -sil-print-types -emit-silgen -sdk %S/Inputs -primary-file %s -primary-file %S/Inputs/nsmanaged-witness-multi-other.swift -I %S/Inputs -I %t -enable-source-import | %FileCheck %s

// RUN: %target-swift-frontend -module-name main -Xllvm -sil-print-types -emit-silgen -sdk %S/Inputs %s %S/Inputs/nsmanaged-witness-multi-other.swift -I %S/Inputs -I %t -enable-source-import | %FileCheck %s

// REQUIRES: objc_interop
import Foundation

public protocol FishProtocol {
  var name: String { get set }
}

extension Fish : FishProtocol {}

// Make sure the modify accessor for Fish.name is emitted here even though it
// its storage was declared in a different translation unit

// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$s4main4FishCAA0B8ProtocolA2aDP4nameSSvMTW :
// CHECK: function_ref @$s4main4FishC4nameSSvM
// CHECK: return

// CHECK-LABEL: sil shared [serialized] [ossa] @$s4main4FishC4nameSSvM : $@yield_once @convention(method) (@guaranteed Fish) -> @yields @inout String
// CHECK: objc_method %0 : $Fish, #Fish.name!getter.foreign
// CHECK: objc_method %0 : $Fish, #Fish.name!setter.foreign
// CHECK: unwind
