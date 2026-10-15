// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -enable-objc-interop -disable-objc-attr-requires-foundation-module -module-name main -import-objc-header %t/header.h -index-store-path %t/idx %t/main.swift

//--- header.h
__attribute__((objc_root_class))
__attribute__((external_source_symbol(language="Swift", defined_in="main", generated_declaration)))
@interface FinalClass
@end

@interface FinalClass (Category)
+ (void)categoryMethod;
- (void)instanceCategoryMethod;
@property (nonatomic, readonly) int categoryProperty;
@end

//--- main.swift
@objc public final class FinalClass {}

public func test(x: FinalClass) {
  FinalClass.categoryMethod()
  x.instanceCategoryMethod()
  _ = x.categoryProperty
}
