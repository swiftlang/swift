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

// Readonly property with a custom getter. Verifies that the setter side of
// the fix correctly does nothing on a property that has no setter.
@property (nonatomic, readonly, getter=hasFoo) BOOL foo;

// Plain property with no custom selectors, to confirm the fix does not
// change behaviour in the common case (selectors should still be the
// Swift defaults, derived from the property name).
@property (nonatomic) NSInteger count;

@end

// A category on the same class with its own custom-selector property. The
// implementation uses `@_objcImplementation(<CategoryName>)` and must still
// emit the header-declared selectors.
@interface CustomAccessorClass (CustomCategory)
@property (nonatomic, getter=isFlagSet) BOOL flagSet;
@end

// A second class used to cover two more code paths:
//  - candidate is a *stored* property (so the getter/setter accessors are
//    synthesized and reached lazily rather than from the parsed accessor
//    record);
//  - the implementation uses the legacy `@objc(<property-name>)` workaround
//    that early adopters of `@_objcImplementation` had to write before this
//    bug was fixed, confirming we still accept it.
@interface CustomAccessorClass2 : NSObject

- (nonnull instancetype)init;

@property (nonatomic, getter=isReady) BOOL ready;

@end

NS_ASSUME_NONNULL_END
