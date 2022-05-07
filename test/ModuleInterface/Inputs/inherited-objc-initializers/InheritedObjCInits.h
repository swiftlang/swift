#import <Foundation/Foundation.h>

@interface FrameworkObject : NSObject
- (nonnull instancetype)initWithInvocation:(nullable NSInvocation *)invocation NS_SWIFT_UNAVAILABLE("unavailable");
- (nonnull instancetype)initWithSelector:(nonnull SEL)selector;
- (nonnull instancetype)initWithInteger:(NSInteger)integer;
@end
