@import ObjectiveC;
@import ExternIntX;
#import "Protocols.h"

#import "used-by-both-headers.h"

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


// If you get an error on the next lines, you probably aren't using the correct Clang repository.
@partial_interface PartialBaseClass;
@partial_interface PartialSubClass : NSObject;
void doSomethingPartialBase(PartialBaseClass *arg);
void doSomethingPartialSub(PartialSubClass *arg);

@interface Base ()
- (NSObject *)safeOverridePartialSub:(PartialSubClass *)arg;
- (NSObject *)unsafeOverridePartialSubParam:(NSObject *)arg;
- (PartialSubClass *)unsafeOverridePartialSubReturn:(PartialSubClass *)arg;
@end

@interface PartialBaseClass ()
- (void)doPartialClassCategoryThing;
@end
@interface PartialSubClass ()
- (void)doPartialClassCategoryThing;
@end

