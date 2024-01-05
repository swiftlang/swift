// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-emit-silgen -module-name Conformance -I %t %t/Conformance.swift -disable-availability-checking | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: objc_interop

//--- module.modulemap
module ObjCProto {
  header "objc_proto.h"
  export *
}

//--- objc_proto.h
#import <Foundation/Foundation.h>

@protocol Doable <NSObject>

- (void)doitWithCompletion:(void (^ __nullable)(NSObject * __nullable result, NSError * __nullable error))completion;

@end

//--- Conformance.swift
import ObjCProto

public final class ConformsToProto: NSObject {}

extension ConformsToProto: Doable {
// CHECK-LABEL: sil shared [thunk] [ossa] @$s11Conformance15ConformsToProtoC4doitSo8NSObjectCyYaKFyyYacfU_To : {{.*}} {
// CHECK:       {{bb[0-9]+}}([[UNOWNED_MAYBE_COMPLETION:%[^,]+]] :
// CHECK:         [[MAYBE_COMPLETION:%[^,]+]] = copy_block [[UNOWNED_MAYBE_COMPLETION]]
// CHECK:         try_apply {{.*}} normal [[SUCCESS:bb[0-9]+]], error [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]([[RESULT:%[^,]+]] :
// CHECK:         [[GUARANTEED_MAYBE_COMPLETION_SUCCESS:%[^,]+]] = begin_borrow [[MAYBE_COMPLETION]]
// CHECK:         switch_enum [[GUARANTEED_MAYBE_COMPLETION_SUCCESS]]
// CHECK-SAME:        case #Optional.some!enumelt: [[SUCCESS_AND_COMPLETION:bb[0-9]+]]
// CHECK-SAME:        case #Optional.none!enumelt: [[SUCCESS_NO_COMPLETION:bb[0-9]+]]
// CHECK:       [[SUCCESS_NO_COMPLETION]]:
// CHECK:         br [[SUCCESS_JOIN:bb[0-9]+]]
// CHECK:       [[SUCCESS_AND_COMPLETION]]
// CHECK:         br [[SUCCESS_JOIN]]
// CHECK:       [[SUCCESS_JOIN]]:
// CHECK:         end_borrow [[GUARANTEED_MAYBE_COMPLETION_SUCCESS]]
// CHECK:         destroy_value [[MAYBE_COMPLETION]]
// CHECK:         destroy_value [[RESULT]]
// CHECK:         br [[EXIT:bb[0-9]+]]
// CHECK:       [[FAILURE]]([[ERROR:%[^,]+]] :
// CHECK:         [[GUARANTEED_MAYBE_COMPLETION_FAILURE:%[^,]+]] = begin_borrow [[MAYBE_COMPLETION]]
// CHECK:         switch_enum [[GUARANTEED_MAYBE_COMPLETION_FAILURE]]
// CHECK-SAME:        case #Optional.some!enumelt: [[FAILURE_AND_COMPLETION:bb[0-9]+]]
// CHECK-SAME:        case #Optional.none!enumelt: [[FAILURE_NO_COMPLETION:bb[0-9]+]]
// CHECK:       [[FAILURE_NO_COMPLETION]]:
// CHECK:         br [[FAILURE_JOIN:bb[0-9]+]]
// CHECK:       [[FAILURE_AND_COMPLETION]]
// CHECK:         br [[FAILURE_JOIN]]
// CHECK:       [[FAILURE_JOIN]]:
// CHECK:         end_borrow [[GUARANTEED_MAYBE_COMPLETION_FAILURE]]
// CHECK:         destroy_value [[MAYBE_COMPLETION]]
// CHECK:         destroy_value [[ERROR]]
// CHECK:         br [[EXIT]]
// CHECK:       [[EXIT]]:
// CHECK-LABEL: } // end sil function '$s11Conformance15ConformsToProtoC4doitSo8NSObjectCyYaKFyyYacfU_To'
  public func doit() async throws -> NSObject {
    fatalError()
  }
}

