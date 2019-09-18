#import <FooKit.h>

@interface SECRETType : Base
@end

@interface Parent ()
- (nonnull instancetype)initWithSECRET:(int)secret __attribute__((objc_designated_initializer, swift_name("init(SECRET:)")));

- (void)methodSECRET;
- (nullable SECRETType *)methodWithSECRETType;

@property (readonly, strong, nullable) Parent *roPropSECRET;
@property (readwrite, strong, nullable) Parent *rwPropSECRET;

- (nullable Parent *)objectAtIndexedSubscript:(int)index;

@property (readwrite, strong, nullable) Parent *redefinedPropSECRET;
@end

@protocol MandatorySecrets
- (nonnull instancetype)initWithRequiredSECRET:(int)secret;
@end

@interface Parent () <MandatorySecrets>
- (nonnull instancetype)initWithRequiredSECRET:(int)secret __attribute__((objc_designated_initializer));
@end

@interface GenericParent<T: Base *> ()
@property (readonly, strong, nullable) T roPropSECRET;
- (nullable Parent *)objectAtIndexedSubscript:(int)index;
@end

@interface SubscriptParent ()
- (void)setObject:(nullable Parent *)object atIndexedSubscript:(int)index;
@end
