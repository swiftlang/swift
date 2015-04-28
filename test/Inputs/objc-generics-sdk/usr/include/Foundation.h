#if __has_feature(modules)
@import ObjectiveC;
@import CoreFoundation;
@import CoreGraphics;
#else
#import <objc/NSObject.h>
#import <CoreFoundation.h>
#import <CoreGraphics.h>
#endif

#define NS_DESIGNATED_INITIALIZER __attribute__((objc_designated_initializer))

#pragma clang assume_nonnull begin

typedef struct objc_object { void *isa; } *id;

typedef struct _NSZone NSZone;
void *allocate(NSZone *zone);

typedef double NSTimeInterval;

@class NSString, NSArray, NSDictionary, NSSet, NSEnumerator;

#define NS_ARRAY(...) NSArray<__VA_ARGS__> *
@interface NSArray<ObjectType> : NSObject
- (ObjectType)objectAtIndexedSubscript:(NSUInteger)idx;
- description;
+ (instancetype)arrayWithObjects:(const ObjectType[])objects count:(NSUInteger)count;
- (void)makeObjectsPerformSelector:(SEL)aSelector;
- (void)makeObjectsPerformSelector:(SEL)aSelector withObject:(ObjectType)anObject;
@end

@interface NSCoder : NSObject
@end

@protocol NSCoding
- (instancetype)initWithCoder:(NSCoder *)aCoder;
@end

@protocol NSSecureCoding <NSCoding>
@end

@protocol NSCopying
- (id)copyWithZone:(NSZone *)zone;
@end

#define NS_DICTIONARY(...) NSDictionary<__VA_ARGS__> *
@interface NSDictionary<KeyType : id<NSCopying>, ObjectType> : NSObject /*<NSCopying, NSMutableCopying, NSSecureCoding, NSFastEnumeration>*/
@property (readonly) NSUInteger count;
- (nullable ObjectType)objectForKey:(KeyType)aKey;
- (NSEnumerator *)keyEnumerator;
@end
@interface NSDictionary<KeyType, ObjectType> (NSExtendedDictionary)
- (nullable ObjectType)objectForKeyedSubscript:(KeyType)key;
@end

@interface NSDictionary (Inits)
- (instancetype)init;
@end

@interface NSMutableDictionary<KeyType : id<NSCopying>, ObjectType> : NSDictionary<KeyType, ObjectType>
- (void)removeObjectForKey:(KeyType)aKey;
- (void)setObject:(ObjectType)anObject forKey:(KeyType)aKey;
@end

@interface NSMutableDictionary<KeyType, ObjectType> (NSExtendedMutableDictionary)
- (void)setObject:(ObjectType)obj forKeyedSubscript:(KeyType)key;
@end

#define NS_SET(...) NSSet<__VA_ARGS__> *
@interface NSSet<KeyType> : NSObject
- (instancetype)init;
- (NSUInteger)count;
- (KeyType)anyObject;
@end

@interface NSMutableSet<KeyType> : NSSet<KeyType>
- (void)addObject:(id)obj;
- (void)removeObject:(id)obj;
@end

@interface NSNumber : NSObject <NSCopying>
@end

@interface NSDecimalNumber : NSObject
+ (instancetype)initWithMantissa:(unsigned long long)mantissa exponent:(short)exponent isNegative:(BOOL)isNegative;
+ (NSDecimalNumber *)decimalNumberWithMantissa:(unsigned long long)mantissa exponent:(short)exponent isNegative:(BOOL)isNegative;
@end

@interface NSError : NSObject
@end

@interface NSString : NSObject <NSSecureCoding, NSCopying>
- (void)onlyOnNSString;
+ (instancetype)stringWithContentsOfFile:(NSString*)path error:(NSError**)error;
+ (instancetype)stringWithContentsOfFile:(NSString*)path encoding:(int)encoding error:(NSError**)error;
@end

@interface Bee : NSObject
-(void)buzz;
@end

@interface Hive : NSObject {
  Bee *queen;
}
- init;

@property (nonnull) NSArray<Bee *> *bees;
@property (nullable) NSDictionary<NSString *, Bee *> *beesByName;
@property NSSet<Bee *> *allBees;
@property NSDictionary<id <NSCopying>, Bee *> *anythingToBees;

+ (instancetype)hiveWithQueen:(Bee *)queen;

- (instancetype)visit;
@end

@interface NSMutableString : NSString
@end

@interface NSURL : NSObject
+ (instancetype)URLWithString:(NSString *)URLString;
@end

@interface NSAttributedString : NSString
- (NSAttributedString *)sliceAttributedString:(NSInteger)startIndex;
@end

typedef CGPoint NSPoint;
typedef CGSize NSSize;
typedef CGRect NSRect;

#define NS_ENUM(_type, _name) CF_ENUM(_type, _name)
#define NS_OPTIONS(_type, _name) CF_OPTIONS(_type, _name)

typedef NS_ENUM(NSUInteger, NSRuncingMode) {
  NSRuncingMince,
  NSRuncingQuince
};

#pragma clang assume_nonnull end

