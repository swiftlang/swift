@import Foundation;

__attribute__((objc_root_class))
@interface Base
- (instancetype)returnMyself;
@end


@interface PropertyAndMethodCollisionBase
- (void)object:(id)obj doSomething:(SEL)selector;
@end

@interface PropertyAndMethodCollision : PropertyAndMethodCollisionBase
@property id object;
@end

@interface PropertyAndMethodReverseCollisionBase
@property id object;
@end

@interface PropertyAndMethodReverseCollision : PropertyAndMethodReverseCollisionBase
- (void)object:(id)obj doSomething:(SEL)selector;
@end

@protocol PropertyProto
@property id protoProp;
@property(readonly) id protoPropRO;
@end

@interface PropertyAndMethodCollision () <PropertyProto>
- (id)protoProp;
- (id)protoPropRO;
@end


@interface SubscriptAndProperty : NSObject
@property (readonly) int x;
- (id)objectAtIndexedSubscript:(int)i;
@end

@interface SubscriptAndProperty ()
@property int x;
- (void)setObject:(id)obj atIndexedSubscript:(int)i;
@end


@protocol SubscriptAndPropertyProto <NSObject>
@property(readonly) int x;
- (id)objectAtIndexedSubscript:(int)i;
@end

@interface SubscriptAndPropertyWithProto : NSObject
@end

@interface SubscriptAndPropertyWithProto (AdoptTheProtocol) <SubscriptAndPropertyProto>
@end

@interface SubscriptAndPropertyWithProto (DeclareTheSetters)
@property int x;
- (void)setObject:(id)obj atIndexedSubscript:(int)i;
@end

@protocol ProtoOrClass
@property int thisIsTheProto;
@end


#pragma mark Constant global properties

extern const int MAX;
extern NSString * const SomeImageName;
extern NSNumber * const SomeNumber;


__weak id globalWeakVar;

@protocol Incomplete
- (id)getObject;
- (id)getObjectFromVarArgs:(id)first, ...;
@end

@protocol IncompleteOptional
@optional
- (id)getObject;
- (id)getObjectFromVarArgs:(id)first, ...;
@end

@interface ExtraSelectors
- (void)foo:(int)a bar:(int)b :(int)c;
@end

@interface DeprecatedFactoryMethod
+ (instancetype)deprecatedFactoryMethod __attribute__((deprecated("use something newer")));
@end


@interface RepeatedMembers : NSObject
- (void)repeatedMethod;
- (void)anotherMethod;
- (void)repeatedMethod __attribute__((deprecated("use something newer")));
@end

// rdar://problem/19726164
@protocol FooDelegate <NSObject>
@property (nonatomic, assign, readonly, getter=isStarted) BOOL started;
@end

// rdar://problem/18847642
@interface NonNullDefaultInit
-(nonnull instancetype)init;
@end

@interface NonNullDefaultInitSub : NonNullDefaultInit
+ (null_unspecified instancetype)sub;
@end
