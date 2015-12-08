// Please keep this file in alphabetical order!

// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %s -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %t/blocks.swiftmodule -parse -emit-objc-header-path %t/blocks.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
// RUN: FileCheck %s < %t/blocks.h
// RUN: %check-in-clang %t/blocks.h

// REQUIRES: objc_interop

import ObjectiveC

typealias MyTuple = (a: Int, b: AnyObject?)
typealias MyInt = Int

// CHECK-LABEL: @interface Callbacks
// CHECK-NEXT: - (void (^ _Nonnull)(void))voidBlocks:(void (^ _Nonnull)(void))input;
// CHECK-NEXT: - (void)manyArguments:(void (^ _Nonnull)(float, float, double, double))input;
// CHECK-NEXT: - (void)blockTakesBlock:(void (^ _Nonnull)(void (^ _Nonnull)(void)))input;
// CHECK-NEXT: - (void)blockReturnsBlock:(void (^ _Nonnull (^ _Nonnull)(void))(void))input;
// CHECK-NEXT: - (void)blockTakesAndReturnsBlock:(uint8_t (^ _Nonnull (^ _Nonnull)(uint16_t (^ _Nonnull)(int16_t)))(int8_t))input;
// CHECK-NEXT: - (void)blockTakesTwoBlocksAndReturnsBlock:(uint8_t (^ _Nonnull (^ _Nonnull)(uint16_t (^ _Nonnull)(int16_t), uint32_t (^ _Nonnull)(int32_t)))(int8_t))input;
// CHECK-NEXT: - (void (^ _Nullable)(NSObject * _Nonnull))returnsBlockWithInput;
// CHECK-NEXT: - (void (^ _Nullable)(NSObject * _Nonnull))returnsBlockWithParenthesizedInput;
// CHECK-NEXT: - (void (^ _Nullable)(NSObject * _Nonnull, NSObject * _Nonnull))returnsBlockWithTwoInputs;
// CHECK-NEXT: - (void)blockWithTypealias:(NSInteger (^ _Nonnull)(NSInteger, id _Nullable))input;
// CHECK-NEXT: - (void)blockWithSimpleTypealias:(NSInteger (^ _Nonnull)(NSInteger))input;
// CHECK-NEXT: - (NSInteger (* _Nonnull)(NSInteger))functionPointers:(NSInteger (* _Nonnull)(NSInteger))input;
// CHECK-NEXT: - (void)functionPointerTakesAndReturnsFunctionPointer:(NSInteger (* _Nonnull (^ _Nonnull (* _Nonnull)(NSInteger))(NSInteger))(NSInteger))input;
// CHECK-NEXT: @property (nonatomic, copy) NSInteger (^ _Nullable savedBlock)(NSInteger);
// CHECK-NEXT: @property (nonatomic) NSInteger (* _Nonnull savedFunctionPointer)(NSInteger);
// CHECK-NEXT: @property (nonatomic) NSInteger (* _Nullable savedFunctionPointer2)(NSInteger);
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

  func blockWithTypealias(input: MyTuple -> MyInt) {}
  func blockWithSimpleTypealias(input: MyInt -> MyInt) {}

  func functionPointers(input: @convention(c) Int -> Int)
      -> @convention(c) Int -> Int {
    return input
  }

  func functionPointerTakesAndReturnsFunctionPointer(
    input: @convention(c) Int -> Int
                              -> @convention(c) Int -> Int
  ) {
  }

  var savedBlock: (Int -> Int)?
  var savedFunctionPointer: @convention(c) Int -> Int = { $0 }
  var savedFunctionPointer2: (@convention(c) Int -> Int)? = { $0 }
}
