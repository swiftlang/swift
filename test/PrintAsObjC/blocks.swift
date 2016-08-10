// Please keep this file in alphabetical order!

// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %s -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %t/blocks.swiftmodule -parse -emit-objc-header-path %t/blocks.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s < %t/blocks.h
// RUN: %check-in-clang %t/blocks.h

// REQUIRES: objc_interop

import ObjectiveC

typealias MyTuple = (Int, AnyObject?)
typealias MyNamedTuple = (a: Int, b: AnyObject?)
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
// CHECK-NEXT: - (void)namedArguments:(void (^ _Nonnull)(float, float, double, double))input;
// CHECK-NEXT: - (void)blockTakesNamedBlock:(void (^ _Nonnull)(void (^ _Nonnull)(void)))input;
// CHECK-NEXT: - (void (^ _Nullable)(NSObject * _Nonnull))returnsBlockWithNamedInput;
// CHECK-NEXT: - (void)blockWithTypealiasWithNames:(NSInteger (^ _Nonnull)(NSInteger a, id _Nullable b))input;
// CHECK-NEXT: - (void)blockWithKeyword:(NSInteger (^ _Nonnull)(NSInteger))_Nullable_;
// CHECK-NEXT: - (NSInteger (* _Nonnull)(NSInteger))functionPointers:(NSInteger (* _Nonnull)(NSInteger))input;
// CHECK-NEXT: - (void)functionPointerTakesAndReturnsFunctionPointer:(NSInteger (* _Nonnull (^ _Nonnull (* _Nonnull)(NSInteger))(NSInteger))(NSInteger))input;
// CHECK-NEXT: - (NSInteger (* _Nonnull)(NSInteger))functionPointersWithName:(NSInteger (* _Nonnull)(NSInteger))input;
// CHECK-NEXT: @property (nonatomic, copy) NSInteger (^ _Nullable savedBlock)(NSInteger);
// CHECK-NEXT: @property (nonatomic, copy) NSInteger (^ _Nullable savedBlockWithName)(NSInteger);
// CHECK-NEXT: @property (nonatomic) NSInteger (* _Nonnull savedFunctionPointer)(NSInteger);
// CHECK-NEXT: @property (nonatomic) NSInteger (* _Nullable savedFunctionPointer2)(NSInteger);
// CHECK-NEXT: @property (nonatomic) NSInteger (* _Nonnull savedFunctionPointerWithName)(NSInteger);
// CHECK-NEXT: @property (nonatomic, copy, getter=this, setter=setThis:) NSInteger (^ _Nonnull this_)(NSInteger);
// CHECK-NEXT: @property (nonatomic, getter=class, setter=setClass:) NSInteger (* _Nonnull class_)(NSInteger);
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class Callbacks {
  func voidBlocks(_ input: @escaping () -> ()) -> () -> () {
    return input
  }
  func manyArguments(_ input: @escaping (Float, Float, Double, Double) -> ()) {}

  func blockTakesBlock(_ input: @escaping (() -> ()) -> ()) {}
  func blockReturnsBlock(_ input: @escaping () -> () -> ()) {}
  func blockTakesAndReturnsBlock(_ input:
    ((Int16) -> (UInt16)) ->
                ((Int8) -> (UInt8))) {}
  func blockTakesTwoBlocksAndReturnsBlock(_ input:
    ((Int16) -> (UInt16),
                 (Int32) -> (UInt32)) ->
                ((Int8) -> (UInt8))) {}

  func returnsBlockWithInput() -> ((NSObject) -> ())? {
    return nil
  }
  func returnsBlockWithParenthesizedInput() -> ((NSObject) -> ())? {
    return nil
  }
  func returnsBlockWithTwoInputs() -> ((NSObject, NSObject) -> ())? {
    return nil
  }

  func blockWithTypealias(_ input: @escaping (MyTuple) -> MyInt) {}
  func blockWithSimpleTypealias(_ input: @escaping (MyInt) -> MyInt) {}

  func namedArguments(_ input: @escaping (_ f1: Float, _ f2: Float, _ d1: Double, _ d2: Double) -> ()) {}
  func blockTakesNamedBlock(_ input: @escaping (_ block: () -> ()) -> ()) {}
  func returnsBlockWithNamedInput() -> ((_ object: NSObject) -> ())? {
    return nil
  }

  func blockWithTypealiasWithNames(_ input: (MyNamedTuple) -> MyInt) {}

  func blockWithKeyword(_ _Nullable: (_ `class`: Int) -> Int) {}

  func functionPointers(_ input: @escaping @convention(c) (Int) -> Int)
      -> @convention(c) (Int) -> Int {
    return input
  }

  func functionPointerTakesAndReturnsFunctionPointer(
    _ input: @escaping @convention(c) (Int) -> (Int)
                              -> @convention(c) (Int) -> Int
  ) {
  }

  func functionPointersWithName(_ input: @escaping @convention(c) (_ value: Int) -> Int)
      -> @convention(c) (_ result: Int) -> Int {
    return input
  }

  var savedBlock: ((Int) -> Int)?
  var savedBlockWithName: ((_ x: Int) -> Int)?
  var savedFunctionPointer: @convention(c) (Int) -> Int = { $0 }
  var savedFunctionPointer2: (@convention(c) (Int) -> Int)? = { $0 }
  var savedFunctionPointerWithName: @convention(c) (_ x: Int) -> Int = { $0 }

  // The following uses a clang keyword as the name.
  var this: (_ block: Int) -> Int = { $0 }
  var `class`: @convention(c) (_ function: Int) -> Int = { $0 }
}
