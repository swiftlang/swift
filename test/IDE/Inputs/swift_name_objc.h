@import ObjectiveC;

#define SWIFT_NAME(X) __attribute__((swift_name(#X)))

#ifndef SWIFT_ENUM_EXTRA
#  define SWIFT_ENUM_EXTRA
#endif

#ifndef SWIFT_ENUM
#  define SWIFT_ENUM(_type, _name)    \
  enum _name : _type _name;           \
  enum SWIFT_ENUM_EXTRA _name : _type
#endif

// Renaming classes
SWIFT_NAME(SomeClass)
@interface SNSomeClass : NSObject
- (instancetype)initWithFloat:(float)f;
- (instancetype)initWithDefault;
- (void)instanceMethodWithX:(float)x y:(float)y z:(float)z;
+ (instancetype)someClassWithDouble:(double)d;
+ (instancetype)someClassWithTry:(BOOL)shouldTry;
+ (NSObject *)buildWithObject:(NSObject *)object SWIFT_NAME(init(object:));

@property (readonly,nonatomic) float floatProperty;
@property (readwrite,nonatomic) double doubleProperty;
@end

SWIFT_NAME(SomeProtocol)
@protocol SNSomeProtocol
- (void)protoInstanceMethodWithX:(float)x y:(float)y;
@end

@interface SNSomeClass ()
- (void)extensionMethodWithX:(float)x y:(float)y;
@end

@interface SNSomeClass (Category1) <SNSomeProtocol>
- (void)categoryMethodWithX:(float)x y:(float)y;
- (void)categoryMethodWithX:(float)x y:(float)y z:(float)z;
@end

@interface SNCollision
@end

@protocol SNCollision
@end

@protocol NSAccessibility
@property (nonatomic) float accessibilityFloat;
@end
