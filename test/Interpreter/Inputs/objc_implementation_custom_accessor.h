#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

// Properties with custom getter/setter selector names must
// be implemented by an `@_objcImplementation` extension using the runtime
// selectors declared here.
@interface CustomAccessorClass : NSObject

- (nonnull instancetype)init;

// BOOL property with a custom getter (the canonical case from the radar).
@property (nonatomic, getter=isEnabled) BOOL enabled;

// Non-BOOL property with both a custom getter and a custom setter.
@property (nonatomic, copy, getter=fooName, setter=setBar:) NSString *baz;

@end

NS_ASSUME_NONNULL_END
