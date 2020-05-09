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

@interface PropertyAndMethodCollisionInOneClass
- (void)object;
+ (void)classRef;
@property (getter=getObject) id object;
@property (class,getter=getClassRef) id classRef;
@end

@interface PropertyAndMethodReverseCollisionInOneClass
@property (getter=getObject) id object;
@property (class,getter=getClassRef) id classRef;
- (void)object;
+ (void)classRef;
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

@interface DesignatedInitWithClassExtension : DesignatedInitRoot
- (instancetype)initWithInt:(NSInteger)value __attribute__((objc_designated_initializer));
- (instancetype)initWithConvenienceInt:(NSInteger)value;
@end
@interface DesignatedInitWithClassExtension ()
- (instancetype)initWithFloat:(float)value __attribute__((objc_designated_initializer));
@end

@interface DesignatedInitWithClassExtensionInAnotherModule : DesignatedInitRoot
- (instancetype)initWithInt:(NSInteger)value __attribute__((objc_designated_initializer));
- (instancetype)initWithConvenienceInt:(NSInteger)value;
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

@interface CallbackBase : NSObject
- (void)performWithHandler:(void(^ _Nonnull)(void))handler;
- (void)performWithOptHandler:(void(^ _Nullable)(void))handler;
- (void)performWithNonescapingHandler:(void(__attribute__((noescape)) ^ _Nonnull)(void))handler;
- (void)performWithOptNonescapingHandler:(void(__attribute__((noescape)) ^ _Nullable)(void))handler;
@end

@interface SelectorSplittingAccessors : NSObject
// Note the custom setter name here; this is important.
@property (setter=takeFooForBar:) BOOL fooForBar;
@end

@interface InstancetypeAccessor : NSObject
@property (class, readonly) InstancetypeAccessor *prop;
+ (instancetype)prop;
@end

typedef NSArray<NSString *> *NSStringArray;

@interface BridgedTypedefs : NSObject
@property (readonly,nonnull) NSArray<NSStringArray> *arrayOfArrayOfStrings;
@end

typedef NSString * _Nonnull (*FPTypedef)(NSString * _Nonnull);
extern FPTypedef _Nonnull getFP(void);


#if !__has_feature(objc_arc_fields)
# error "Your Clang is not new enough"
#endif
struct NonTrivialToCopy {
  __strong id field;
};

struct NonTrivialToCopyWrapper {
  struct NonTrivialToCopy inner;
};

struct TrivialToCopy {
  __unsafe_unretained id field;
};

@interface OverrideInExtensionBase : NSObject
- (void)method;
- (void)accessWarning;
@end

@interface OverrideInExtensionSub : OverrideInExtensionBase
@end

@interface SuperclassWithDesignatedInitInCategory
@end

@interface SubclassWithSwiftPrivateDesignatedInit : SuperclassWithDesignatedInitInCategory
-(instancetype) initWithI:(NSInteger)i __attribute__((objc_designated_initializer));
@end

@interface SuperclassWithDesignatedInitInCategory ()
-(instancetype) initWithI:(NSInteger)i __attribute__((objc_designated_initializer));
@end
