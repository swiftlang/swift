// Please keep this file in alphabetical order!

// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend %clang-importer-sdk -emit-module -o %t %s -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend %clang-importer-sdk -parse-as-library %t/blocks.swiftmodule -parse -emit-objc-header-path %t/blocks.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
// RUN: FileCheck %s < %t/blocks.h
// RUN: %check-in-clang %t/blocks.h

import ObjectiveC

// CHECK-LABEL: @interface Callbacks
// CHECK-NEXT: - (void (^ __nonnull)(void))voidBlocks:(void (^ __nonnull)(void))input;
// CHECK-NEXT: - (void)manyArguments:(void (^ __nonnull)(float, float, double, double))input;
// CHECK-NEXT: - (void)blockTakesBlock:(void (^ __nonnull)(void (^ __nonnull)(void)))input;
// CHECK-NEXT: - (void)blockReturnsBlock:(void (^ __nonnull (^ __nonnull)(void))(void))input;
// CHECK-NEXT: - (void)blockTakesAndReturnsBlock:(uint8_t (^ __nonnull (^ __nonnull)(uint16_t (^ __nonnull)(int16_t)))(int8_t))input;
// CHECK-NEXT: - (void)blockTakesTwoBlocksAndReturnsBlock:(uint8_t (^ __nonnull (^ __nonnull)(uint16_t (^ __nonnull)(int16_t), uint32_t (^ __nonnull)(int32_t)))(int8_t))input;
// CHECK-NEXT: - (void (^ __nullable)(NSObject * __nonnull))returnsBlockWithInput;
// CHECK-NEXT: - (void (^ __nullable)(NSObject * __nonnull))returnsBlockWithParenthesizedInput;
// CHECK-NEXT: - (void (^ __nullable)(NSObject * __nonnull, NSObject * __nonnull))returnsBlockWithTwoInputs;
// CHECK-NEXT: - (NSInteger (* __null_unspecified)(NSInteger))functionPointers:(NSInteger (* __null_unspecified)(NSInteger))input;
// CHECK-NEXT: - (void)functionPointerTakesAndReturnsFunctionPointer:(NSInteger (* __null_unspecified (* __null_unspecified)(NSInteger (* __null_unspecified)(NSInteger)))(NSInteger))input;
// CHECK-NEXT: @property (nonatomic, copy) NSInteger (^ __nullable savedBlock)(NSInteger);
// CHECK-NEXT: @property (nonatomic) NSInteger (* __null_unspecified savedFunctionPointer)(NSInteger);
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

  var savedBlock: (Int -> Int)?
  @objc var savedFunctionPointer: CFunctionPointer<Int -> Int> = nil
}
