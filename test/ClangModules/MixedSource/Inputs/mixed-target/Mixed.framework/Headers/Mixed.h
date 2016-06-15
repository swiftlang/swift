@import Foundation;

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


@class PartialBaseClass;
@class PartialSubClass /* : NSObject */;
void doSomethingPartialBase(PartialBaseClass *arg);
void doSomethingPartialSub(PartialSubClass *arg);

@interface Base ()
- (NSObject *)safeOverridePartialSub:(PartialSubClass *)arg;
- (NSObject *)unsafeOverridePartialSubParam:(NSObject *)arg;
- (PartialSubClass *)unsafeOverridePartialSubReturn:(PartialSubClass *)arg;
@end

@interface WrapperInterface
typedef int NameInInterface;
@end

@protocol WrapperProto
typedef int NameInProtocol;
@end

@interface WrapperInterface (Category)
typedef int NameInCategory;
@end
