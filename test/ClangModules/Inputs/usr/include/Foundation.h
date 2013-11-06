@import ObjectiveC;

typedef struct objc_object { void *isa; } *id;

typedef unsigned long NSUInteger;

typedef signed char BOOL;

@class NSString;

@interface NSArray : NSObject
- (id)objectAtIndexedSubscript:(NSUInteger)idx;
- description;

@property NSString *nsstringProperty;
@property BOOL boolProperty;

@end

@interface NSString : NSObject
- (void)onlyOnNSString;
@end

NSString *NSStringToNSString(NSString *str);

@interface Hive
@property __attribute__((iboutletcollection(B))) NSArray *bees;
@end

BOOL BOOLtoBOOL(BOOL b);

typedef struct _NSZone NSZone;

void *allocate(NSZone *zone);


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

#define NS_ENUM(_type, _name) enum _name : _type _name; enum _name : _type

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

typedef NS_ENUM(NSInteger, NSSingleConstantEnum) {
  NSSingleConstantValue,
};

@protocol NSWobbling
-(void)wobble;

@optional
-(void)wibble;

- (id)objectAtIndexedSubscript:(NSUInteger)idx;
@end

