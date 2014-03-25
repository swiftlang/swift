@import ObjectiveC;

@class ForwardClass;
void doSomething(ForwardClass *arg);

@interface Base
- (NSObject *)safeOverride:(ForwardClass *)arg;
- (NSObject *)unsafeOverrideParam:(NSObject *)arg;
- (ForwardClass *)unsafeOverrideReturn:(ForwardClass *)arg;
@end

@protocol ForwardClassUser
- (void)consumeForwardClass:(ForwardClass *)arg;
@property ForwardClass *forward;
@end


@protocol ForwardProto;
void doSomethingProto(id <ForwardProto> arg);

@interface Base ()
- (NSObject *)safeOverrideProto:(id <ForwardProto>)arg;
- (NSObject *)unsafeOverrideProtoParam:(NSObject *)arg;
- (id <ForwardProto>)unsafeOverrideProtoReturn:(id <ForwardProto>)arg;
@end
