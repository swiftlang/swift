// Please keep this file in alphabetical order!

// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift %clang-importer-sdk -module-cache-path %t/clang-module-cache -emit-module -o %t %s
// RUN: %swift %clang-importer-sdk -module-cache-path %t/clang-module-cache -parse-as-library %t/blocks.swiftmodule -parse -emit-objc-header-path %t/blocks.h -import-objc-header %S/../Inputs/empty.h
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
// CHECK-NEXT: - (void (^)(NSObject *))returnsBlockWithInput;
// CHECK-NEXT: - (void (^)(NSObject *))returnsBlockWithParenthesizedInput;
// CHECK-NEXT: - (void (^)(NSObject *, NSObject *))returnsBlockWithTwoInputs;
// CHECK-NEXT: - (NSInteger (*)(NSInteger))functionPointers:(NSInteger (*)(NSInteger))input;
// CHECK-NEXT: - (void)functionPointerTakesAndReturnsFunctionPointer:(NSInteger (* (*)(NSInteger (*)(NSInteger)))(NSInteger))input;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class Callbacks {
  func voidBlocks(input: () -> ()) -> () -> () {
    return input
  }
  func manyArguments(input: (Float, Float, Double, Double) -> ()) {}

  func blockTakesBlock(input: (() -> ()) -> ()) {}
  func blockReturnsBlock(input: () -> () -> ()) {}
  func blockTakesAndReturnsBlock(input:
    ((Int16) -> (UInt16)) ->
                ((Int8) -> (UInt8))) {}
  func blockTakesTwoBlocksAndReturnsBlock(input:
    ((Int16) -> (UInt16),
                 (Int32) -> (UInt32)) ->
                ((Int8) -> (UInt8))) {}

  func returnsBlockWithInput() -> (NSObject -> ())? {
    return nil
  }
  func returnsBlockWithParenthesizedInput() -> ((NSObject) -> ())? {
    return nil
  }
  func returnsBlockWithTwoInputs() -> ((NSObject, NSObject) -> ())? {
    return nil
  }

  func functionPointers(input: CFunctionPointer<Int -> Int>)
      -> CFunctionPointer<Int -> Int> {
    return input
  }

  func functionPointerTakesAndReturnsFunctionPointer(
    input: CFunctionPointer<CFunctionPointer<Int -> Int>
                              -> CFunctionPointer<Int -> Int>>
  ) {
  }
}
