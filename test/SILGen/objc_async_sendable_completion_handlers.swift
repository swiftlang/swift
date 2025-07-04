// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil %t/src/main.swift \
// RUN:   -import-objc-header %t/src/Test.h \
// RUN:   -swift-version 6 \
// RUN:   -module-name main -I %t -verify | %FileCheck %t/src/main.swift

// REQUIRES: objc_interop

// rdar://154695053

//--- Test.h

@import Foundation;

@interface Test <NSObject>
+(void) compute: (void (^)(void)) completion;
@end

//--- main.swift

func test(v: Test.Type) async {
  _ = await v.compute()
}

// CHECK-LABEL: sil hidden @$s4main4test1vySo4TestCm_tYaF : $@convention(thin) @async (@thick Test.Type) -> ()
// CHECK: [[TEST_TYPE:%.*]] = thick_to_objc_metatype %0 to $@objc_metatype Test.Type
// CHECK: [[METHOD:%.*]] = objc_method %5, #Test.compute!foreign : (Test.Type) -> () async -> (), $@convention(objc_method) (Optional<@convention(block) @Sendable () -> ()>, @objc_metatype Test.Type) -> ()
// CHECK: [[BLOCK:%.*]] = alloc_stack $@block_storage Any
// CHECK: [[COMPLETION_HANDLER:%.*]] = function_ref @$sIeyBh_ytTz_ : $@convention(c) @Sendable (@inout_aliasable @block_storage Any) -> ()
// CHECK: [[HEADER:%.*]] = init_block_storage_header [[BLOCK]], invoke [[COMPLETION_HANDLER]]
// CHECK: [[OPTIONAL_HEADER:%.*]] = enum $Optional<@convention(block) @Sendable () -> ()>, #Optional.some!enumelt, [[HEADER]]
// CHECK: {{.*}} = apply [[METHOD]]([[OPTIONAL_HEADER]], [[TEST_TYPE]])
// CHECK: } // end sil function '$s4main4test1vySo4TestCm_tYaF'

// CHECK-LABEL: sil shared [transparent] [thunk] @$sIeyBh_ytTz_ : $@convention(c) @Sendable (@inout_aliasable @block_storage Any) -> ()
