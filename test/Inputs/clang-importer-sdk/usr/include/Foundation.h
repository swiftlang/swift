@import ObjectiveC;
@import CoreGraphics;

typedef struct objc_object { void *isa; } *id;

typedef signed char BOOL;

@class NSString;

/// Aaa.  NSArray.  Bbb.
@interface NSArray : NSObject
- (id)objectAtIndexedSubscript:(NSUInteger)idx;
- description;

@property NSString *nsstringProperty;
@property BOOL boolProperty;

@end

@protocol NSCoding
@end

@protocol NSSecureCoding <NSCoding>
@end

@interface NSString : NSObject <NSSecureCoding>
- (void)onlyOnNSString;
@end

NSString *NSStringToNSString(NSString *str);

@interface Hive : NSObject {
  B *queen;
}
- init;

@property __attribute__((iboutletcollection(B))) NSArray *bees;

@property(getter=isMakingHoney) BOOL makingHoney;
@property(setter=assignGuard:) id guard;

+ (instancetype)hiveWithQueen:(B *)queen;

- (instancetype)visit;
@end

BOOL BOOLtoBOOL(BOOL b);

typedef struct _NSZone NSZone;

void *allocate(NSZone *zone);

typedef CGPoint NSPoint;
typedef CGSize NSSize;
typedef CGRect NSRect;


@interface BadCollection
- (id)objectForKeyedSubscript:(id)key;
- (void)setObject:(id)object forKeyedSubscript:(NSString *)key;
@end

@interface BadCollectionParent
- (id)objectForKeyedSubscript:(NSString *)key;
@end

@interface BadCollectionChild : BadCollectionParent
- (void)setObject:(id)object forKeyedSubscript:(id)key;
@end

@interface ReadOnlyCollectionChild : BadCollectionParent
- (void)setObject:(id)object forKeyedSubscript:(id)key;
@end

//===---
// Enums.
//===---

#define CF_ENUM(_type, _name) enum _name : _type _name; enum _name : _type
#define CF_OPTIONS(_type, _name) enum _name : _type _name; enum _name : _type
#define NS_ENUM(_type, _name) CF_ENUM(_type, _name)
#define NS_OPTIONS(_type, _name) CF_OPTIONS(_type, _name)

/// Aaa.  NSRuncingMode.  Bbb.
typedef NS_ENUM(NSUInteger, NSRuncingMode) {
  NSRuncingMince,
  NSRuncingQuince
};

typedef NS_ENUM(int, NSUnderlyingType) {
  NSUnderlyingTypeZim,
  NSUnderlyingTypeZang,
  NSUnderlyingTypeFoo = 11,
  NSUnderlyingTypeBar = 22,
  NSUnderlyingTypeBas
};

typedef NS_ENUM(unsigned, NSUnsignedUnderlyingTypeNegativeValue) {
  NSNegativeOne = -1,
  NSNegativeTwo = -2,
};

typedef NS_ENUM(NSInteger, NSPrefixWordBreak) {
  NSPrefixWordBreakBanjo,
  NSPrefixWordBreakBandana
};

typedef NS_ENUM(NSInteger, NSPrefixWordBreak2) {
  NSPrefixWordBreakBarBas,
  NSPrefixWordBreakBareBass,
};

typedef NS_ENUM(NSInteger, NSPrefixWordBreak3) {
  NSPrefixWordBreak1Bob,
  NSPrefixWordBreak1Ben,
};

typedef NS_ENUM(NSInteger, NSSingleConstantEnum) {
  NSSingleConstantValue,
};

typedef NS_ENUM(unsigned char, NSAliasesEnum) {
  NSAliasesOriginal = 129,
  NSAliasesBySameValue = 129,
  NSAliasesByEquivalentValue = -127,
  NSAliasesByName = NSAliasesOriginal,
};

typedef NS_ENUM(NSUInteger, NSNumberFormatterBehavior) {
  NSNumberFormatterBehaviorDefault = 0,
  NSNumberFormatterBehavior10_0 = 1000,
  NSNumberFormatterBehavior10_4 = 1040,
};


/// Aaa.  NSRuncingOptions.  Bbb.
typedef NS_OPTIONS(NSUInteger, NSRuncingOptions) {
  NSRuncingEnableMince = 1,
  NSRuncingEnableQuince = 2,
};

typedef NS_OPTIONS(NSUInteger, NSSingleOptions) {
  NSSingleValue = 1,
};

@protocol NSWobbling
-(void)wobble;

- (instancetype)returnMyself; 

@optional
-(void)wibble;

- (id)objectAtIndexedSubscript:(NSUInteger)idx;
@end

#define NS_DESIGNATED_INITIALIZER __attribute__((objc_designated_initializer))

@interface NSURL : NSObject
@end
