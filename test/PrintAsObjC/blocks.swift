// Please keep this file in alphabetical order!

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %s -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %t/blocks.swiftmodule -typecheck -emit-objc-header-path %t/blocks.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s < %t/blocks.h
// RUN: %check-in-clang %t/blocks.h

// REQUIRES: objc_interop

import ObjectiveC

typealias MyInt = Int
typealias MyBlockWithEscapingParam = (@escaping () -> ()) -> Int
typealias MyBlockWithNoescapeParam = (() -> ()) -> Int

// Please see related tests in PrintAsObjC/imported-block-typedefs.swift.

// CHECK-LABEL: @interface Callbacks
@objc class Callbacks {
  
  // CHECK-NEXT: - (void (^ _Nonnull)(void))voidBlocks:(void (^ _Nonnull)(void))input SWIFT_WARN_UNUSED_RESULT;
  @objc func voidBlocks(_ input: @escaping () -> ()) -> () -> () {
    return input
  }
  
  // CHECK-NEXT: - (void)manyArguments:(void (^ _Nonnull)(float, float, double, double))input;
  @objc func manyArguments(_ input: @escaping (Float, Float, Double, Double) -> ()) {}

  // CHECK-NEXT: - (void)blockTakesBlock:(void (^ _Nonnull)(SWIFT_NOESCAPE void (^ _Nonnull)(void)))input;
  @objc func blockTakesBlock(_ input: @escaping (() -> ()) -> ()) {}
  
  // CHECK-NEXT: - (void)blockReturnsBlock:(void (^ _Nonnull (^ _Nonnull)(void))(void))input;
  @objc func blockReturnsBlock(_ input: @escaping () -> () -> ()) {}
  
  // CHECK-NEXT: - (void)blockTakesAndReturnsBlock:(SWIFT_NOESCAPE uint8_t (^ _Nonnull (^ _Nonnull)(SWIFT_NOESCAPE uint16_t (^ _Nonnull)(int16_t)))(int8_t))input;
  @objc func blockTakesAndReturnsBlock(_ input:
    ((Int16) -> (UInt16)) ->
                ((Int8) -> (UInt8))) {}
  
  // CHECK-NEXT: - (void)blockTakesTwoBlocksAndReturnsBlock:(SWIFT_NOESCAPE uint8_t (^ _Nonnull (^ _Nonnull)(SWIFT_NOESCAPE uint16_t (^ _Nonnull)(int16_t), SWIFT_NOESCAPE uint32_t (^ _Nonnull)(int32_t)))(int8_t))input;
  @objc func blockTakesTwoBlocksAndReturnsBlock(_ input:
    ((Int16) -> (UInt16),
                 (Int32) -> (UInt32)) ->
                ((Int8) -> (UInt8))) {}

  // CHECK-NEXT: - (void (^ _Nullable)(NSObject * _Nonnull))returnsBlockWithInput SWIFT_WARN_UNUSED_RESULT;
  @objc func returnsBlockWithInput() -> ((NSObject) -> ())? {
    return nil
  }
  
  // CHECK-NEXT: - (void (^ _Nullable)(NSObject * _Nonnull))returnsBlockWithParenthesizedInput SWIFT_WARN_UNUSED_RESULT;
  @objc func returnsBlockWithParenthesizedInput() -> ((NSObject) -> ())? {
    return nil
  }
  
  // CHECK-NEXT: - (void (^ _Nullable)(NSObject * _Nonnull, NSObject * _Nonnull))returnsBlockWithTwoInputs SWIFT_WARN_UNUSED_RESULT;
  @objc func returnsBlockWithTwoInputs() -> ((NSObject, NSObject) -> ())? {
    return nil
  }
  
  // CHECK-NEXT: - (void)blockWithSimpleTypealias:(NSInteger (^ _Nonnull)(NSInteger))input;
  @objc func blockWithSimpleTypealias(_ input: @escaping (MyInt) -> MyInt) {}

  // CHECK-NEXT: - (void)namedArguments:(void (^ _Nonnull)(float, float, double, double))input;
  @objc func namedArguments(_ input: @escaping (_ f1: Float, _ f2: Float, _ d1: Double, _ d2: Double) -> ()) {}
  
  // CHECK-NEXT: - (void)blockTakesNamedBlock:(void (^ _Nonnull)(SWIFT_NOESCAPE void (^ _Nonnull)(void)))input;
  @objc func blockTakesNamedBlock(_ input: @escaping (_ block: () -> ()) -> ()) {}
  
  // CHECK-NEXT: - (void (^ _Nullable)(NSObject * _Nonnull))returnsBlockWithNamedInput SWIFT_WARN_UNUSED_RESULT;
  @objc func returnsBlockWithNamedInput() -> ((_ object: NSObject) -> ())? {
    return nil
  }

  // CHECK-NEXT: - (void)blockWithKeyword:(SWIFT_NOESCAPE NSInteger (^ _Nonnull)(NSInteger))_Nullable_;
  @objc func blockWithKeyword(_ _Nullable: (_ `class`: Int) -> Int) {}

  // CHECK-NEXT: - (NSInteger (* _Nonnull)(NSInteger))functionPointers:(NSInteger (* _Nonnull)(NSInteger))input SWIFT_WARN_UNUSED_RESULT;
  @objc func functionPointers(_ input: @escaping @convention(c) (Int) -> Int)
      -> @convention(c) (Int) -> Int {
    return input
  }

  // CHECK-NEXT: - (void)blockWithBlockTypealias:(SWIFT_NOESCAPE NSInteger (^ _Nonnull)(void (^ _Nonnull)(void)))block;
  @objc func blockWithBlockTypealias(_ block: MyBlockWithEscapingParam) {}

  // CHECK-NEXT: - (void)blockWithBlockTypealias2:(NSInteger (^ _Nonnull)(void (^ _Nonnull)(void)))block;
  @objc func blockWithBlockTypealias2(_ block: @escaping MyBlockWithEscapingParam) {}

  // CHECK-NEXT: - (void)blockWithBlockTypealias3:(SWIFT_NOESCAPE NSInteger (^ _Nonnull)(SWIFT_NOESCAPE void (^ _Nonnull)(void)))block;
  @objc func blockWithBlockTypealias3(_ block: MyBlockWithNoescapeParam) {}

  // CHECK-NEXT: - (void)blockWithBlockTypealias4:(NSInteger (^ _Nonnull)(SWIFT_NOESCAPE void (^ _Nonnull)(void)))block;
  @objc func blockWithBlockTypealias4(_ block: @escaping MyBlockWithNoescapeParam) {}

  // CHECK-NEXT: - (void)functionPointerTakesAndReturnsFunctionPointer:(NSInteger (* _Nonnull (^ _Nonnull (* _Nonnull)(NSInteger))(NSInteger))(NSInteger))input;
  @objc func functionPointerTakesAndReturnsFunctionPointer(
    _ input: @escaping @convention(c) (Int) -> (Int)
                              -> @convention(c) (Int) -> Int
  ) {
  }
  
  // CHECK-NEXT: - (void (* _Nonnull)(SWIFT_NOESCAPE NSInteger (* _Nonnull)(NSInteger, NSInteger)))returnsFunctionPointerThatTakesFunctionPointer SWIFT_WARN_UNUSED_RESULT;
  @objc func returnsFunctionPointerThatTakesFunctionPointer() ->
    @convention(c) (_ comparator: @convention(c) (_ x: Int, _ y: Int) -> Int) -> Void {
    fatalError()
  }

  // CHECK-NEXT: - (NSInteger (* _Nonnull)(NSInteger))functionPointersWithName:(NSInteger (* _Nonnull)(NSInteger))input SWIFT_WARN_UNUSED_RESULT;
  @objc func functionPointersWithName(_ input: @escaping @convention(c) (_ value: Int) -> Int)
      -> @convention(c) (_ result: Int) -> Int {
    return input
  }

  // CHECK-NEXT: @property (nonatomic, copy) NSInteger (^ _Nullable savedBlock)(NSInteger);
  @objc var savedBlock: ((Int) -> Int)?
  
  // CHECK-NEXT: @property (nonatomic, copy) NSInteger (^ _Nullable savedBlockWithName)(NSInteger);
  @objc var savedBlockWithName: ((_ x: Int) -> Int)?
  
  // CHECK-NEXT: @property (nonatomic) NSInteger (* _Nonnull savedFunctionPointer)(NSInteger);
  @objc var savedFunctionPointer: @convention(c) (Int) -> Int = { $0 }
  
  // CHECK-NEXT: @property (nonatomic) NSInteger (* _Nullable savedFunctionPointer2)(NSInteger);
  @objc var savedFunctionPointer2: (@convention(c) (Int) -> Int)? = { $0 }
  
  // CHECK-NEXT: @property (nonatomic) NSInteger (* _Nonnull savedFunctionPointerWithName)(NSInteger);
  @objc var savedFunctionPointerWithName: @convention(c) (_ x: Int) -> Int = { $0 }

  // The following uses a clang keyword as the name.
  
  // CHECK-NEXT: @property (nonatomic, copy, getter=this, setter=setThis:) NSInteger (^ _Nonnull this_)(NSInteger);
  @objc var this: (_ block: Int) -> Int = { $0 }
  
  // CHECK-NEXT: @property (nonatomic, getter=class, setter=setClass:) NSInteger (* _Nonnull class_)(NSInteger);
  @objc var `class`: @convention(c) (_ function: Int) -> Int = { $0 }
  
  // CHECK-NEXT: init
  @objc init() {}
}

// CHECK-NEXT: @end
