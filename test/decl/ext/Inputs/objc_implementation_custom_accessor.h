@import Foundation;

NS_ASSUME_NONNULL_BEGIN

// Properties that exercise custom getter/setter selector names. Used by
// test/decl/ext/objc_implementation_custom_accessor.swift and friends.
@interface CustomAccessorClass : NSObject

@property (nonatomic, getter=isEnabled) BOOL enabled;
@property (nonatomic, copy, getter=fooName, setter=setBar:) NSString *baz;

@end

NS_ASSUME_NONNULL_END
