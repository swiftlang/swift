// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend %clang-importer-sdk -emit-module -I %S/Inputs/custom-modules -o %t %s
// RUN: %target-swift-frontend %clang-importer-sdk -parse-as-library %t/override.swiftmodule -parse -emit-objc-header-path %t/override.h
// RUN: FileCheck %s < %t/override.h
// RUN: %check-in-clang %t/override.h -I %S/Inputs/custom-modules -Wno-super-class-method-mismatch -Wno-overriding-method-mismatch
// RUN: not %check-in-clang %t/override.h -Wno-super-class-method-mismatch -I %S/Inputs/custom-modules 2>&1 | FileCheck -check-prefix=CLANG %s

// REQUIRES: objc_interop

import OverrideBase
// No errors from Clang until we get to the FixMe class.
// CLANG-NOT: error

// CHECK-LABEL: @interface A_Child : Base
class A_Child : Base {
  // CHECK-NEXT: @property (nonatomic, readonly, getter=prop) NSUInteger prop;
  override var prop: Int { return 0 }
  // CHECK-NEXT: - (id __nullable)objectAtIndexedSubscript:(NSUInteger)x;
  override subscript(x: Int) -> AnyObject? { return nil }

  // CHECK-NEXT: - (NSUInteger)foo;
  override func foo() -> Int { return 0 }
  // CHECK-NEXT: - (NSUInteger)foo:(NSUInteger)_;
  override func foo(_: Int) -> Int { return 0 }
  // CHECK-NEXT: - (NSUInteger)foo:(NSUInteger)x y:(NSUInteger)y;
  override func foo(x: Int, y: Int) -> Int { return x + y }

  // CHECK-NEXT: - (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
} // CHECK-NEXT: @end

// CHECK-LABEL: @interface A_Grandchild : A_Child
class A_Grandchild : A_Child {
  // CHECK-NEXT: @property (nonatomic, readonly, getter=prop) NSUInteger prop;
  override var prop: Int { return 0 }
  // CHECK-NEXT: - (id __nullable)objectAtIndexedSubscript:(NSUInteger)x;
  override subscript(x: Int) -> AnyObject? { return nil }

  // CHECK-NEXT: - (NSUInteger)foo;
  override func foo() -> Int { return 0 }
  // CHECK-NEXT: - (NSUInteger)foo:(NSUInteger)_;
  override func foo(_: Int) -> Int { return 0 }
  // CHECK-NEXT: - (NSUInteger)foo:(NSUInteger)x y:(NSUInteger)y;
  override func foo(x: Int, y: Int) -> Int { return x + y }

  // CHECK-NEXT: - (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
} // CHECK-NEXT: @end


// CHECK-LABEL: @interface B_EmptyChild : Base
class B_EmptyChild : Base {
} // CHECK: @end

// CHECK-LABEL: @interface B_GrandchildViaEmpty : B_EmptyChild
class B_GrandchildViaEmpty : B_EmptyChild {
  // CHECK-NEXT: @property (nonatomic, getter=prop, setter=setProp:) NSUInteger prop;
  override var prop: Int { 
    get { return 0 }
    set {}
  }

  // CHECK-NEXT: - (id __nullable)objectAtIndexedSubscript:(NSUInteger)x;
  // CHECK-NEXT: - (void)setObject:(id __nullable)newValue atIndexedSubscript:(NSUInteger)x;
  override subscript(x: Int) -> AnyObject? {
    get { return nil }
    set {}
  }

  // CHECK-NEXT: - (NSUInteger)foo;
  override func foo() -> Int { return 0 }
  // CHECK-NEXT: - (NSUInteger)foo:(NSUInteger)_;
  override func foo(_: Int) -> Int { return 0 }
  // CHECK-NEXT: - (NSUInteger)foo:(NSUInteger)x y:(NSUInteger)y;
  override func foo(x: Int, y: Int) -> Int { return x + y }

  // CHECK-NEXT: - (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
} // CHECK-NEXT: @end

// The output in this class doesn't yet preserve NSUInteger correctly.
// CHECK-LABEL: @interface FixMe : Base
class FixMe : Base {
  // CHECK-NEXT: - (void)callback:(NSInteger (^ __nullable)(void))fn;
  // CLANG: error: conflicting parameter types in declaration of 'callback:'
  override func callback(fn: (() -> Int)?) {}

  // CHECK-NEXT: - (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
} // CHECK-NEXT: @end

// No more errors from Clang.
// CLANG-NOT: error: