@import ObjectiveC;

@class ForwardClass;
void doSomething(ForwardClass *arg);

@interface Base
- (NSObject *)safeOverride:(ForwardClass *)arg;
- (NSObject *)unsafeOverrideParam:(NSObject *)arg;
- (ForwardClass *)unsafeOverrideReturn:(ForwardClass *)arg;
@end

@protocol UseForwardProto
- (void)consumeForwardClass:(ForwardClass *)arg;
@property ForwardClass *forward;
@end
