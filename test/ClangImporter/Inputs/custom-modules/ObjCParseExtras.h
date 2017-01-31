@import Foundation;

__attribute__((objc_root_class))
@interface Base
- (instancetype)returnMyself;
@end


@interface PropertyAndMethodCollisionBase
- (void)object:(id)obj doSomething:(SEL)selector;
+ (void)classRef:(id)obj doSomething:(SEL)selector;
@end

@interface PropertyAndMethodCollision : PropertyAndMethodCollisionBase
@property id object;
@property (class) id classRef;
@end

@interface PropertyAndMethodReverseCollisionBase
@property id object;
@property (class) id classRef;
@end

@interface PropertyAndMethodReverseCollision : PropertyAndMethodReverseCollisionBase
- (void)object:(id)obj doSomething:(SEL)selector;
+ (void)classRef:(id)obj doSomething:(SEL)selector;
@end

@protocol PropertyProto
@property id protoProp;
@property(readonly) id protoPropRO;
@property(class) id protoClassProp;
@property(class, readonly) id protoClassPropRO;
@end

@interface PropertyAndMethodCollision () <PropertyProto>
- (id)protoProp;
- (id)protoPropRO;
+ (id)protoClassProp;
+ (id)protoClassPropRO;
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

@interface StrangeSelectors
- (void)foo:(int)a bar:(int)b :(int)c;
+ (void)cStyle:(int)a, int b, int c;
+ (StrangeSelectors *):(int)x; // factory-method-like
+ (StrangeSelectors *):(int)x b:(int)y __attribute__((swift_name("init(a:b:)")));
- (void):(int)x;
- (void):(int)x :(int)y __attribute__((swift_name("empty(_:_:)")));
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

@interface SomeCell : NSObject
-(instancetype)initString:(NSString *)string;
@property (nonatomic,readonly,getter=isEnabled) BOOL enabled;
@end

@interface DesignatedInitRoot : NSObject
- (instancetype)init __attribute__((objc_designated_initializer));
@end

@interface DesignatedInitBase : DesignatedInitRoot
- (instancetype)initWithInt:(NSInteger)value __attribute__((objc_designated_initializer));
@end


@protocol ExplicitSetterProto
@property (readonly) id foo;
- (void)setFoo:(id)foo;
@end

@protocol OptionalSetterProto
@property (readonly) id bar;
@optional
- (void)setBar:(id)bar;
@end


typedef NSObject <NSCopying> *CopyableNSObject;
typedef SomeCell <NSCopying> *CopyableSomeCell;


@interface Printing : NSObject
- (void)print;
- (void)print:(id)thing;
- (void)print:(id)thing options:(id)options;

+ (void)print;
+ (void)print:(id)thing;
+ (void)print:(id)thing options:(id)options;
@end


@interface FailBase : NSObject
- (nullable instancetype)initWithValue:(NSInteger)val error:(NSError **)error;
+ (BOOL)processValueAndReturnError:(NSError **)error;
@end

@interface SelectorSplittingAccessors : NSObject
// Note the custom setter name here; this is important.
@property (setter=takeFooForBar:) BOOL fooForBar;
@end
