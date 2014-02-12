// Please keep this file in alphabetical order!

// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift %clang-importer-sdk -module-cache-path %t/clang-module-cache -emit-module -o %t %s
// RUN: %swift-ide-test %clang-importer-sdk -module-cache-path %t/clang-module-cache -print-as-objc %t/blocks.swiftmodule -source-filename %s > %t/blocks.h
// RUN: FileCheck %s < %t/blocks.h
// RUN: %check-in-clang %t/blocks.h

import ObjectiveC

// CHECK-LABEL: @interface Callbacks
// CHECK-NEXT: - (void (^)(void))voidBlocks:(void (^)(void))input;
// CHECK-NEXT: - (void)manyArguments:(void (^)(float, float, double, double))input;
// CHECK-NEXT: - (void)blockTakesBlock:(void (^)(void (^)(void)))input;
// CHECK-NEXT: - (void)blockReturnsBlock:(void (^ (^)(void))(void))input;
// CHECK-NEXT: - (void)blockTakesAndReturnsBlock:(uint8_t (^ (^)(uint16_t (^)(int16_t)))(int8_t))input;
// CHECK-NEXT: - (void)blockTakesTwoBlocksAndReturnsBlock:(uint8_t (^ (^)(uint16_t (^)(int16_t), uint32_t (^)(int32_t)))(int8_t))input;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class Callbacks {
  func voidBlocks(input: @objc_block () -> ()) -> @objc_block () -> () {
    return input
  }
  func manyArguments(input: @objc_block (Float, Float, Double, Double) -> ()) {}

  func blockTakesBlock(input: @objc_block (@objc_block () -> ()) -> ()) {}
  func blockReturnsBlock(input: @objc_block () -> @objc_block () -> ()) {}
  func blockTakesAndReturnsBlock(input:
    @objc_block (@objc_block (Int16) -> (UInt16)) ->
                (@objc_block (Int8) -> (UInt8))) {}
  func blockTakesTwoBlocksAndReturnsBlock(input:
    @objc_block (@objc_block (Int16) -> (UInt16),
                 @objc_block (Int32) -> (UInt32)) ->
                (@objc_block (Int8) -> (UInt8))) {}
}
