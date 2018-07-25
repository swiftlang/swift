// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %s
// RUN: %target-swift-frontend -parse-as-library %t/availability-real-sdk.swiftmodule -typecheck -emit-objc-header-path %t/availability-real-sdk.h -import-objc-header %S/../Inputs/empty.h
// RUN: %FileCheck %s < %t/availability-real-sdk.h
// RUN: %check-in-clang %t/availability-real-sdk.h

// REQUIRES: objc_interop


// CHECK-LABEL: @interface NSArray<ObjectType> (SWIFT_EXTENSION(main))
// CHECK-NEXT: - (id _Nonnull)methodDeprecatedInFavorOfReverseObjectEnumerator SWIFT_WARN_UNUSED_RESULT
// CHECK-SAME: SWIFT_DEPRECATED_MSG("", "reverseObjectEnumerator");
// CHECK-NEXT: - (NSArray * _Nonnull)methodDeprecatedInFavorOfAddingObjectWithObject:(id _Nonnull)object SWIFT_WARN_UNUSED_RESULT
// CHECK-SAME: SWIFT_DEPRECATED_MSG("", "arrayByAddingObject:");

// CHECK-NEXT: @end

// CHECK-LABEL: @interface SubClassOfSet : NSSet
// CHECK-NEXT: - (id _Nonnull)methodDeprecatedInFavorOfAnyObject SWIFT_WARN_UNUSED_RESULT
// CHECK-SAME: SWIFT_DEPRECATED_MSG("", "anyObject");
// CHECK-NEXT: - (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: - (nonnull instancetype)initWithObjects:(id _Nonnull const * _Nullable)objects count:(NSUInteger)cnt OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: - (nullable instancetype)initWithCoder:(NSCoder * _Nonnull)aDecoder OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: @end


import Foundation


public class SubClassOfSet: NSSet {
  @available(*, deprecated, renamed: "anyObject()")
  @objc func methodDeprecatedInFavorOfAnyObject() -> Any { return 0 }
}


extension NSArray {
  @available(*, deprecated, renamed: "reverseObjectEnumerator()")
  @objc func methodDeprecatedInFavorOfReverseObjectEnumerator() -> Any { return 0 }
  
  @available(*, deprecated, renamed: "adding(_:)")
  @objc func methodDeprecatedInFavorOfAddingObject(object: Any) -> NSArray {
    return self.adding(object) as NSArray
  }
}

