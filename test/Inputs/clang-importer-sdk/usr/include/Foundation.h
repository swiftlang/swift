#if __has_feature(modules)
@import ObjectiveC;
@import CoreFoundation;
@import CoreGraphics;
#else
#import <objc/NSObject.h>
#import <CoreFoundation.h>
#import <CoreGraphics.h>
#endif
#import <stdbool.h>

#define NS_DESIGNATED_INITIALIZER __attribute__((objc_designated_initializer))

#define NS_NOESCAPE CF_NOESCAPE

typedef struct objc_object { void *isa; } *id;

typedef struct _NSZone NSZone;
void *allocate(NSZone *zone);

typedef double NSTimeInterval;

typedef struct _NSRange {
    NSUInteger location;
    NSUInteger length;
} NSRange;


extern NSUInteger NSRealMemoryAvailable(void) __attribute__((availability(macosx,introduced=10.0 ,deprecated=10.8,message="" ))) __attribute__((availability(ios,introduced=2.0 ,deprecated=6.0,message="" )));
extern NSUInteger SomeCrazyAppExtensionForbiddenAPI(void)
  __attribute__((availability(macosx_app_extension,unavailable,message="Not available in App Extensions")))
  __attribute__((availability(ios_app_extension,unavailable,message="Not available in App Extensions")))
  __attribute__((availability(xros_app_extension,unavailable,message="Not available in App Extensions")));

extern NSString *const globalStringAvailableOn51 __attribute__((availability(macosx,introduced=51)));
extern NSString *const globalStringAvailableOn52 __attribute__((availability(macosx,introduced=52)));

__attribute__((availability(macosx,introduced=51)))
@interface NSAvailableOn51 : NSObject
- (instancetype)init;
- (instancetype)initWithStringOn52:(NSString *)s __attribute__((availability(macosx,introduced=52)));

@property NSInteger propertyOn52 __attribute__((availability(macosx,introduced=52)));

- (void)methodAvailableOn52 __attribute__((availability(macosx,introduced=52)));
@end

extern NSAvailableOn51 *const globalClassInstanceAvailableOn51 __attribute__((availability(macosx,introduced=51)));

__attribute__((availability(macosx,introduced=51)))
@protocol NSProtocolAvailableOn51

@end

__attribute__((availability(macosx,introduced=10.9)))
@interface NSAvailableOn10_9 : NSObject

@property NSInteger propertyOn10_9;

// Properties with unavailable accessors declared before property.
- (void)setPropertyOn51WithSetterOn52Before:(NSInteger)prop __attribute__((availability(macosx,introduced=52)));
@property NSInteger propertyOn51WithSetterOn52Before __attribute__((availability(macosx,introduced=51)));

- (NSInteger)propertyOn51WithGetterOn52Before __attribute__((availability(macosx,introduced=52)));
@property NSInteger propertyOn51WithGetterOn52Before __attribute__((availability(macosx,introduced=51)));

// Properties with unavailable accessors declared after property.
@property NSInteger propertyOn51WithSetterOn52After __attribute__((availability(macosx,introduced=51)));
- (void)setPropertyOn51WithSetterOn52After:(NSInteger)prop __attribute__((availability(macosx,introduced=52)));

@property NSInteger propertyOn51WithGetterOn52After __attribute__((availability(macosx,introduced=51)));
- (NSInteger)propertyOn51WithGetterOn52After __attribute__((availability(macosx,introduced=52)));

// Property with redeclared with a setter in a category
@property(readonly) NSInteger readOnlyRedeclaredWithSetterInCategory __attribute__((availability(macosx,introduced=51)));

- (void)methodAvailableOn52 __attribute__((availability(macosx,introduced=52)));
@end

@interface NSAvailableOn10_9 (NSWithPropertyReclarationInACategory)

@property(readwrite) NSInteger readOnlyRedeclaredWithSetterInCategory __attribute__((availability(macosx,introduced=51)));
- (void)setReadOnlyRedeclaredWithSetterInCategory:(NSInteger)prop __attribute__((availability(macosx,introduced=52)));
@end

/// Aaa.  NSAvailableOnOSX51AndIOS8_0.  Bbb.
__attribute__((availability(macosx,introduced=51)))
__attribute__((availability(ios,introduced=8.0)))
@interface NSAvailableOnOSX51AndIOS8_0 : NSObject

@end

@class NSString, NSArray, NSDictionary, NSSet, NSEnumerator;

@class NSMutableArray<ObjectType>;

/// Aaa.  NSArray.  Bbb.
@interface NSArray<ObjectType> : NSObject
- (instancetype)initWithObjects:(const ObjectType _Nonnull [_Nullable])objects
                          count:(NSUInteger)cnt NS_DESIGNATED_INITIALIZER;
- (nonnull ObjectType)objectAtIndexedSubscript:(NSUInteger)idx;
- description;
- (void)makeObjectsPerformSelector:(nonnull SEL)aSelector;
- (void)makeObjectsPerformSelector:(nonnull SEL)aSelector withObject:(nullable ObjectType)anObject;
- (void)makeObjectsPerformSelector:(nonnull SEL)aSelector withObject:(nullable ObjectType)anObject withObject:(nullable ObjectType)anotherObject;
- (nonnull NSMutableArray<ObjectType> *)mutableCopy;
@end

@interface NSArray<ObjectType>(NSArrayCreation)
+ (instancetype)arrayWithObjects:(const ObjectType _Nonnull [_Nullable])objects
                           count:(NSUInteger)cnt;
@end

@interface NSArray (AddingObject)
- (NSInteger)indexOfObject:(nonnull id)object;
@end

@interface DummyClass : NSObject
- (nonnull id)objectAtIndexedSubscript:(NSUInteger)idx;
- description;

@property NSString *nsstringProperty;
@property BOOL boolProperty;
@property NSArray *arrayProperty;
@property NSDictionary *dictProperty;
@property NSSet *setProperty;

- (nonnull NSString*) fetchNonnullString;
- (nullable NSString*) fetchNullableString;
- (null_unspecified NSString*) fetchNullproneString;

- (void) takeNonnullString: (nonnull NSString*) string;
- (void) takeNullableString: (nullable NSString*) string;
- (void) takeNullproneString: (null_unspecified NSString*) string;

@property(readwrite) _Nonnull NSString *nonnullStringProperty;
@property(readwrite) _Nullable NSString *nullableStringProperty;
@property(readwrite) _Null_unspecified NSString *nullproneStringProperty;

@end

@interface DummyClass (Extras)
@property NSString *nsstringProperty2;
@end

@interface NSCoder : NSObject
@end

@protocol NSCoding
- (nullable instancetype)initWithCoder:(nonnull NSCoder *)aCoder;
@end

@protocol NSSecureCoding <NSCoding>
@end

@protocol NSCopying
- (id)copyWithZone:(nullable NSZone *)zone;
@end

@protocol NSMutableCopying
- (id)mutableCopyWithZone:(nullable NSZone *)zone;
@end

@interface NSDictionary<KeyType, ObjectType> : NSObject /*<NSCopying, NSMutableCopying, NSSecureCoding, NSFastEnumeration>*/
@property (readonly) NSUInteger count;
- (nullable ObjectType)objectForKey:(nonnull KeyType)aKey;
- (nonnull NSEnumerator *)keyEnumerator;
@end
@interface NSDictionary<KeyType, ObjectType> (NSExtendedDictionary)
- (nullable ObjectType)objectForKeyedSubscript:(nonnull KeyType)key;
@end

@interface NSDictionary (Inits)
- (nonnull instancetype)init;
@end

@interface NSMutableDictionary<KeyType : id<NSCopying>, ObjectType> : NSDictionary<KeyType, ObjectType>
- (void)removeObjectForKey:(nonnull KeyType)aKey;
- (void)setObject:(nonnull ObjectType)anObject forKey:(nonnull KeyType)aKey;
@end

@interface NSMutableDictionary<KeyType, ObjectType> (NSExtendedMutableDictionary)
- (void)setObject:(nullable ObjectType)obj forKeyedSubscript:(nonnull KeyType)key;
@end

@interface NSSet<KeyType> : NSObject
- (nonnull instancetype)init;
- (NSUInteger)count;
- (nonnull KeyType)anyObject;
- (nonnull instancetype)initWithArray:(nonnull NSArray<KeyType> *)array;
@end

@interface NSMutableSet<KeyType> : NSSet<KeyType>
- (void)addObject:(KeyType)obj;
- (void)removeObject:(KeyType)obj;
@end

@interface NSCountedSet<KeyType> : NSMutableSet<KeyType> 
- (instancetype)initWithCapacity:(NSUInteger)numItems NS_DESIGNATED_INITIALIZER;
- (instancetype)initWithArray:(NSArray<KeyType> *)array;
@end

@interface NSValue : NSObject <NSCopying>
@end

@interface NSValue (NSRange)
- (NSValue *)valueWithRange:(NSRange)range;
@property NSRange rangeValue;
@end

typedef __INT32_TYPE__ int32_t;

@interface NSNumber : NSValue
+ (nonnull NSNumber *)numberWithInt:(int)value;
+ (nonnull NSNumber *)numberWithInteger:(NSInteger)value;
+ (nonnull NSNumber *)numberWithUnsignedInteger:(NSUInteger)value;
+ (nonnull NSNumber *)numberWithDouble:(double)value;

- (nonnull NSNumber *)initWithInteger:(NSInteger)value;
- (nonnull NSNumber *)initWithUnsignedInteger:(NSUInteger)value;
- (nonnull NSNumber *)initWithDouble:(double)value;
- (nonnull NSNumber *)addDouble:(double)value;
- (nonnull NSNumber *)addBool:(BOOL)value;

- (nonnull NSNumber *)addUInt16:(unsigned short)value;
- (nonnull NSNumber *)addInt:(int)value;
- (nonnull NSNumber *)subtractInt32:(int32_t)value;

@property NSInteger integerValue;
@property NSUInteger unsignedIntegerValue;
@end

@interface NSDecimalNumber : NSObject
+ (instancetype)initWithMantissa:(unsigned long long)mantissa exponent:(short)exponent isNegative:(BOOL)isNegative;
+ (NSDecimalNumber *)decimalNumberWithMantissa:(unsigned long long)mantissa exponent:(short)exponent isNegative:(BOOL)isNegative;
@end

@interface NSError : NSObject
@property (readonly, nonnull) NSString *domain;
@property (readonly) NSInteger code;
- (nonnull instancetype)initWithDomain:(nonnull NSString *)domain code:(NSInteger)code userInfo:(nullable NSDictionary *)userInfo;
@end

@interface NSString : NSObject <NSSecureCoding, NSCopying>
- (void)onlyOnNSString;
+ (instancetype)stringWithContentsOfFile:(NSString*)path error:(NSError**)error;
+ (instancetype)stringWithContentsOfFile:(NSString*)path encoding:(int)encoding error:(NSError**)error;

+ (instancetype)stringWithPath:(NSString*)path;
+ (nullable instancetype)stringWithPath:(NSString*)path encoding:(int)encoding;
@end

__attribute__((warn_unused_result)) NSString *NSStringToNSString(NSString *str);

@interface Bee : NSObject
-(void)buzz;
@end

@interface Hive : NSObject {
  Bee *queen;
}
- init;
- (instancetype)initWithCoder:(NSCoder *)aDecoder;

@property (nonnull) NSArray<Bee *> *bees;
@property (nullable) NSDictionary<NSString *, Bee *> *beesByName;
@property (nonnull) NSSet<Bee *> *allBees;
@property (nonnull) NSDictionary<id <NSCopying>, Bee *> *anythingToBees;

@property(getter=isMakingHoney) BOOL makingHoney;
@property(readonly,getter=isEmpty) bool empty;
@property(setter=assignGuard:) id guard;

+ (instancetype)hiveWithQueen:(Bee *)queen;
+ (instancetype)hiveWithFlakyQueen:(Bee *)queen error:(NSError **)error;

- (instancetype)visit;
@end

@interface NSMutableString : NSString
@end

@interface NSURL : NSObject
- (instancetype)URLWithString:(NSString *)URLString;
+ (instancetype)URLWithString:(NSString *)URLString;
- (BOOL)getResourceValue:(out id _Nullable *)value
                  forKey:(NSString *)key
                   error:(out NSError *_Nullable *)error;
@end

// An all-initials name like NSURL or NSUUID, but one that isn't bridged.
@interface NSGUID : NSObject
@end

@interface NSAttributedString : NSString
- (NSAttributedString *)sliceAttributedString:(NSInteger)startIndex;
@end

BOOL BOOLtoBOOL(BOOL b);

typedef CGPoint NSPoint;
typedef CGSize NSSize;
typedef CGRect NSRect;
typedef NSPoint *NSPointArray;

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

typedef NS_ENUM(NSInteger, NSPrefixWordBreakCustom) {
  PrefixWordBreakProblemCase __attribute__((swift_name("problemCase"))),
  NSPrefixWordBreakDeprecatedGoodCase __attribute__((deprecated)),
};

typedef NS_ENUM(NSInteger, NSPrefixWordBreak2Custom) {
  PrefixWordBreak2ProblemCase __attribute__((swift_name("problemCase"))),
  PrefixWordBreak2DeprecatedBadCase __attribute__((deprecated)),
  NSPrefixWordBreak2DeprecatedGoodCase __attribute__((deprecated)),
  NSPrefixWordBreak2GoodCase,
};

typedef NS_ENUM(NSInteger, NSPrefixWordBreakReversedCustom) {
  NSPrefixWordBreakReversedDeprecatedGoodCase __attribute__((deprecated)),
  PrefixWordBreakReversedProblemCase __attribute__((swift_name("problemCase"))),
};

typedef NS_ENUM(NSInteger, NSPrefixWordBreakReorderedCustom) {
  PrefixWordBreakReorderedProblemCase __attribute__((swift_name("problemCase"))),
  NSPrefixWordBreakReorderedGoodCase,
  PrefixWordBreakReorderedDeprecatedBadCase __attribute__((deprecated)),
  NSPrefixWordBreakReorderedDeprecatedGoodCase __attribute__((deprecated)),
};

typedef NS_ENUM(NSInteger, NSPrefixWordBreakReordered2Custom) {
  PrefixWordBreakReordered2DeprecatedBadCase __attribute__((deprecated)),
  PrefixWordBreakReordered2ProblemCase __attribute__((swift_name("problemCase"))),
  NSPrefixWordBreakReordered2GoodCase,
  NSPrefixWordBreakReordered2DeprecatedGoodCase __attribute__((deprecated)),
};

typedef NS_ENUM(NSInteger, NSPrefixWordBreakForgotToDeprecate) {
  NSPrefixWordBreakNewName1,
  NSPrefixWordBreakNewName2,
  NSOldName1PrefixWordBreak = NSPrefixWordBreakNewName1, // should have been deprecated
};

typedef NS_ENUM(NSInteger, NSPrefixWordBreakInvalidWord) {
  NSPrefixWordBreakFoo,
  NSPrefixWordBreakBar,
  NSPrefixWordBreak42,  // expected prefix would have left an invalid identifier
};

// Check tiebreaking rules. In each case, the diagnostic should choose the 'Better' case over the 'Worse' case.
typedef NS_ENUM(NSInteger, NSPrefixWordBreakSuffixTieBreakers) {
  // Available preferred over unavailable.
  NSPrefixWordBreakBetterForTest1,
  NSPrefixWordBreakWorseForTest1 __attribute__((unavailable)),

  // Un-deprecated preferred over deprecated.
  NSPrefixWordBreakBetterForTest2,
  NSPrefixWordBreakWorseForTest2 __attribute__((deprecated)),

  // Shorter preferred over longer (it's a closer match).
  NSPrefixWordBreakBetterForTest3,
  NSPrefixWordBreakWorseXXXForTest3,

  // Alphabetically first preferred over second (arbitrary tiebreaker).
  NSPrefixWordBreakBetterForTest4,
  NSPrefixWordBreakWorseXForTest4,

  // Make sure we're not choosing based on ordering.
  NSPrefixWordBreakWorseXForTest5,
  NSPrefixWordBreakBetterForTest5,
};

typedef NS_OPTIONS(NSInteger, NSPrefixWordBreakOptions) {
  NSPrefixWordBreakNewOption1 = 0x1,
  NSPrefixWordBreakNewOption2 = 0x2,
  NSOldOption1PrefixWordBreak = NSPrefixWordBreakNewOption1, // should have been deprecated
};

typedef NS_ENUM(NSInteger, NSSwiftNameAllTheThings) {
  NSSwiftNameAllTheThingsA __attribute__((swift_name("Foo"))),
  NSSwiftNameAllTheThingsB __attribute__((swift_name("Bar"))),
};

typedef NS_ENUM(NSInteger, NSSwiftNameBad) {
  NSSwiftNameBadA __attribute__((swift_name("class"))),
};


typedef NS_ENUM(NSInteger, NSSingleConstantEnum) {
  NSSingleConstantValue,
};

typedef NS_ENUM(unsigned char, NSAliasesEnum) {
  NSAliasesOriginal = 129,
  NSAliasesBySameValue = 129,
  NSAliasesByEquivalentValue = -127,
  NSAliasesByName = NSAliasesOriginal,
  NSAliasesDifferentValue = 2
};

typedef NS_ENUM(unsigned char, NSUnavailableAliasesEnum) {
  NSUnavailableAliasesOriginalAU = 0,
  NSUnavailableAliasesAliasAU __attribute__((unavailable)) = 0,
  NSUnavailableAliasesOriginalUA __attribute__((unavailable)) = 1,
  NSUnavailableAliasesAliasUA = 1,
  NSUnavailableAliasesOriginalUU __attribute__((unavailable)) = 2,
  NSUnavailableAliasesAliasUU __attribute__((unavailable)) = 2,
};

NS_ENUM(NSInteger, NSMalformedEnumMissingTypedef) {
  NSMalformedEnumMissingTypedefValue
};

@interface NSNumberFormatter : NSObject
@end

typedef NS_ENUM(NSUInteger, NSNumberFormatterBehavior) {
  NSNumberFormatterBehaviorDefault = 0,
  NSNumberFormatterBehavior10_0 = 1000,
  NSNumberFormatterBehavior10_4 = 1040,
};

@interface NSNotification : NSObject
@end

@interface NSNotificationQueue : NSObject
@end

typedef NS_ENUM(NSUInteger, NSPostingStyle) {
  NSPostWhenIdle = 1,
  NSPostASAP = 2,
  NSPostNow = 3
};

@interface NSXMLNode : NSObject
@end

typedef NS_ENUM(NSUInteger, NSXMLNodeKind) {
  NSXMLInvalidKind = 0,
  NSXMLDocumentKind,
  NSXMLElementKind,
  NSXMLAttributeKind,
  NSXMLNamespaceKind,
  NSXMLProcessingInstructionKind,
  NSXMLCommentKind,
  NSXMLTextKind,
  NSXMLDTDKind __attribute__((swift_name("DTDKind"))),
  NSXMLEntityDeclarationKind,
  NSXMLAttributeDeclarationKind,
  NSXMLElementDeclarationKind,
  NSXMLNotationDeclarationKind
};

// From CoreFoundation
typedef CF_ENUM(NSInteger, CFURLPathStyle) {
  kCFURLPOSIXPathStyle = 0,
  kCFURLHFSPathStyle /*CF_ENUM_DEPRECATED(10_0, 10_9, 2_0, 7_0)*/,
  kCFURLWindowsPathStyle
};

typedef CF_ENUM(NSInteger, CFURLOrUTI) {
  kCFURLKind,
  kCFUTIKind
};

typedef CF_ENUM(NSInteger, Magnitude) {
  k0,
  k1,
  k2,
};

typedef CF_ENUM(NSInteger, MagnitudeWords) {
  kZero,
  kOne,
  kTwo,
};


// Deliberately simple to test the overlay module.
enum {
  NSUTF8StringEncoding = 8
};


/// Aaa.  NSRuncingOptions.  Bbb.
typedef NS_OPTIONS(NSUInteger, NSRuncingOptions) {
  NSRuncingNone = 0,
  NSRuncingEnableMince = 1,
  NSRuncingEnableQuince = 2,
};

typedef NS_OPTIONS(NSUInteger, NSSingleOptions) {
  NSSingleValue = 1,
};

// From CoreFoundation
typedef CF_OPTIONS(unsigned long, CFCalendarUnit) {
  kCFCalendarUnitEra = (1UL << 1),
  kCFCalendarUnitYear = (1UL << 2),
  kCFCalendarUnitMonth = (1UL << 3),
  kCFCalendarUnitDay = (1UL << 4),
  kCFCalendarUnitHour = (1UL << 5),
  kCFCalendarUnitMinute = (1UL << 6),
  kCFCalendarUnitSecond = (1UL << 7),
  kCFCalendarUnitWeek /*CF_ENUM_DEPRECATED(10_4, 51, 2_0, 8_0)*/ = (1UL << 8),
  kCFCalendarUnitWeekday = (1UL << 9),
  kCFCalendarUnitWeekdayOrdinal = (1UL << 10),
  kCFCalendarUnitQuarter /*CF_ENUM_AVAILABLE(10_6, 4_0)*/ = (1UL << 11),
  kCFCalendarUnitWeekOfMonth /*CF_ENUM_AVAILABLE(10_7, 5_0)*/ = (1UL << 12),
  kCFCalendarUnitWeekOfYear /*CF_ENUM_AVAILABLE(10_7, 5_0)*/ = (1UL << 13),
  kCFCalendarUnitYearForWeekOfYear /*CF_ENUM_AVAILABLE(10_7, 5_0)*/ = (1UL << 14),
};

// From Foundation

typedef NS_OPTIONS(NSUInteger, NSKeyValueObservingOptions) {
  NSKeyValueObservingOptionNew = 0x01,
  NSKeyValueObservingOptionOld = 0x02,
  NSKeyValueObservingOptionInitial /*NS_ENUM_AVAILABLE(10_5, 2_0)*/ = 0x04,
  NSKeyValueObservingOptionPrior /*NS_ENUM_AVAILABLE(10_5, 2_0)*/ = 0x08
};

@interface NSCalendar : NSObject
@end

#define NS_CALENDAR_ENUM_DEPRECATED(osx_in, osx_out, ios_in, ios_out, msg) \
  __attribute__((availability(macosx, introduced=osx_in, deprecated=osx_out, message=msg))) \
  __attribute__((availability(iphoneos, introduced=ios_in, deprecated=ios_out, message=msg)))
typedef NS_OPTIONS(NSUInteger, NSCalendarUnit) {
  NSCalendarUnitEra                = kCFCalendarUnitEra,
  NSCalendarUnitYear               = kCFCalendarUnitYear,
  NSCalendarUnitMonth              = kCFCalendarUnitMonth,
  // snip
  NSCalendarUnitCalendar           = (1 << 20),

  NSEraCalendarUnit NS_CALENDAR_ENUM_DEPRECATED(10_4, 10_9, 2_0, 7_0, "Use NSCalendarUnitEra instead") = NSCalendarUnitEra,
  NSYearCalendarUnit NS_CALENDAR_ENUM_DEPRECATED(10_4, 10_9, 2_0, 7_0, "Use NSCalendarUnitYear instead") = NSCalendarUnitYear,
  NSMonthCalendarUnit NS_CALENDAR_ENUM_DEPRECATED(10_4, 10_9, 2_0, 7_0, "Use NSCalendarUnitMonth instead") = NSCalendarUnitMonth,
  // snip
  NSCalendarCalendarUnit NS_CALENDAR_ENUM_DEPRECATED(10_7, 10_9, 4_0, 7_0, "Use NSCalendarUnitCalendar instead") = NSCalendarUnitCalendar,
};

typedef NS_OPTIONS(NSUInteger, NSCalendarUnitDeprecated) {
  NSEraCalendarUnitDeprecated NS_CALENDAR_ENUM_DEPRECATED(10_4, 10_9, 2_0, 7_0, "Use NSCalendarUnitEra instead") = NSCalendarUnitEra,
  NSYearCalendarUnitDeprecated NS_CALENDAR_ENUM_DEPRECATED(10_4, 10_9, 2_0, 7_0, "Use NSCalendarUnitYear instead") = NSCalendarUnitYear,
  NSMonthCalendarUnitDeprecated NS_CALENDAR_ENUM_DEPRECATED(10_4, 10_9, 2_0, 7_0, "Use NSCalendarUnitMonth instead") = NSCalendarUnitMonth,
  // snip
  NSCalendarCalendarUnitDeprecated NS_CALENDAR_ENUM_DEPRECATED(10_7, 10_9, 4_0, 7_0, "Use NSCalendarUnitCalendar instead") = NSCalendarUnitCalendar,
};

typedef NS_OPTIONS(NSUInteger, NSOptionsAlsoGetSwiftName) {
  ThisIsAnNSOptionsCaseWithSwiftName __attribute__((swift_name("Case"))) = 0x1
};

#define CF_SWIFT_UNAVAILABLE(_msg) __attribute__((availability(swift, unavailable, message=_msg)))
#define NS_SWIFT_UNAVAILABLE(_msg) CF_SWIFT_UNAVAILABLE(_msg)

typedef NS_ENUM(NSUInteger, NSRectEdge) {
  NSRectEdgeMinX = 0,
  NSRectEdgeMinY = 1,
  NSRectEdgeMaxX = 2,
  NSRectEdgeMaxY = 3,

  NSMinXEdge NS_SWIFT_UNAVAILABLE("Use NSRectEdge.MinX instead") = NSRectEdgeMinX,
  NSMinYEdge NS_SWIFT_UNAVAILABLE("Use NSRectEdge.MinY instead") = NSRectEdgeMinY,
  NSMaxXEdge __attribute__((availability(swift, unavailable, message="Use NSRectEdge.MaxX instead"))) = NSRectEdgeMaxX,
  NSMaxYEdge __attribute__((availability(swift, unavailable, message="Use NSRectEdge.MaxY instead"))) = NSRectEdgeMaxY,
};

// From CoreBluetooth
typedef NS_OPTIONS(NSInteger, CBCharacteristicProperties) {
  CBCharacteristicPropertyBroadcast = 0x01,
  CBCharacteristicPropertyRead = 0x02,
  CBCharacteristicPropertyWriteWithoutResponse = 0x04,
  CBCharacteristicPropertyWrite = 0x08,
  CBCharacteristicPropertyNotify = 0x10,
  CBCharacteristicPropertyIndicate = 0x20,
  CBCharacteristicPropertyAuthenticatedSignedWrites = 0x40,
  CBCharacteristicPropertyExtendedProperties = 0x80,
  CBCharacteristicPropertyNotifyEncryptionRequired /*NS_ENUM_AVAILABLE(10_9, 6_0)*/ = 0x100,
  CBCharacteristicPropertyIndicateEncryptionRequired /*NS_ENUM_AVAILABLE(10_9, 6_0)*/ = 0x200
};

// From CoreMedia
typedef CF_OPTIONS(unsigned int, CMTimeFlags) {
  kCMTimeFlags_Valid = 1UL<<0,
  kCMTimeFlags_HasBeenRounded = 1UL<<1,
  kCMTimeFlags_PositiveInfinity = 1UL<<2,
  kCMTimeFlags_NegativeInfinity = 1UL<<3,
  kCMTimeFlags_Indefinite = 1UL<<4,
  kCMTimeFlags_ImpliedValueFlagsMask = kCMTimeFlags_PositiveInfinity | kCMTimeFlags_NegativeInfinity | kCMTimeFlags_Indefinite
};
typedef CF_OPTIONS(unsigned int, CMTimeFlagsWithNumber) {
  kCMTimeFlagsWithNumber_Valid = 1UL<<0,
  kCMTimeFlagsWithNumber_888 = 1UL<<1,
};


// Contrived name with a plural "-es"...normally these are "beeps".
typedef NS_OPTIONS(NSInteger, AlertBuzzes) {
  AlertBuzzNone = 0,
  AlertBuzzFunk = 1 << 0,
  AlertBuzzHero = 1 << 1,
  AlertBuzzSosumi = 1 << 2
};

// From AppKit
typedef NS_OPTIONS(NSUInteger, NSBitmapFormat) {
  NSAlphaFirstBitmapFormat            = 1 << 0, // 0 means is alpha last (RGBA, CMYKA, etc.)
  NSAlphaNonpremultipliedBitmapFormat = 1 << 1, // 0 means is premultiplied
  NSFloatingPointSamplesBitmapFormat  = 1 << 2, // 0 is integer

  NS16BitLittleEndianBitmapFormat /*NS_ENUM_AVAILABLE_MAC(51)*/ = (1 << 8),
  NS32BitLittleEndianBitmapFormat /*NS_ENUM_AVAILABLE_MAC(51)*/ = (1 << 9),
  NS16BitBigEndianBitmapFormat /*NS_ENUM_AVAILABLE_MAC(51)*/ = (1 << 10),
  NS32BitBigEndianBitmapFormat /*NS_ENUM_AVAILABLE_MAC(51)*/ = (1 << 11)
};

typedef NS_OPTIONS(NSUInteger, NSBitmapFormatReversed) {
  NS16BitLittleEndianBitmapFormatR /*NS_ENUM_AVAILABLE_MAC(51)*/ = (1 << 8),
  NS32BitLittleEndianBitmapFormatR /*NS_ENUM_AVAILABLE_MAC(51)*/ = (1 << 9),
  NS16BitBigEndianBitmapFormatR /*NS_ENUM_AVAILABLE_MAC(51)*/ = (1 << 10),
  NS32BitBigEndianBitmapFormatR /*NS_ENUM_AVAILABLE_MAC(51)*/ = (1 << 11),

  NSAlphaFirstBitmapFormatR            = 1 << 0, // 0 means is alpha last (RGBA, CMYKA, etc.)
  NSAlphaNonpremultipliedBitmapFormatR = 1 << 1, // 0 means is premultiplied
  NSFloatingPointSamplesBitmapFormatR  = 1 << 2, // 0 is integer
};

typedef NS_OPTIONS(NSUInteger, NSBitmapFormat2) {
  NSU16a = 1,
  NSU32a = 2,
};

typedef NS_OPTIONS(NSUInteger, NSBitmapFormat3) {
  NSU16b = 1,
  NSU32b = 2,
  NSS16b = 4,
  NSS32b = 8,
};

typedef NS_OPTIONS(NSUInteger, NSUBitmapFormat4) {
  NSU16c = 1,
  NSU32c = 2,
};

typedef NS_OPTIONS(NSUInteger, NSABitmapFormat5) {
  NSAA16d = 1,
  NSAB32d = 2,
};

/// Aaa.  NSPotentiallyUnavailableOptions.  Bbb.
typedef NS_OPTIONS(NSUInteger, NSPotentiallyUnavailableOptions) {
  NSPotentiallyUnavailableOptionsFirst   = (1 << 0),
  NSPotentiallyUnavailableOptionsSecond  = (1 << 1),
  NSPotentiallyUnavailableOptionsThird   = (1 << 2),
}  __attribute__((availability(macosx, introduced=51)));

/// Aaa.  NSOptionsWithUnavailableElement.  Bbb.
typedef NS_OPTIONS(NSUInteger, NSOptionsWithUnavailableElement) {
  NSOptionsWithUnavailableElementFirst    = (1 << 0),
  NSOptionsWithUnavailableElementSecond   = (1 << 1),
  NSOptionsWithUnavailableElementThird __attribute__((availability(macosx, introduced=51))) = (1 << 2),
};

/// Aaa.  NSUnavailableEnum.  Bbb.
typedef NS_ENUM(NSUInteger, NSUnavailableEnum) {
  NSUnavailableEnumFirst,
  NSUnavailableEnumSecond,
  NSUnavailableEnumThird,
}  __attribute__((availability(macosx, introduced=51)));

/// Aaa.  NSEnumWithUnavailableElement.  Bbb.
typedef NS_ENUM(NSUInteger, NSEnumWithUnavailableElement) {
  NSEnumWithUnavailableElementFirst,
  NSEnumWithUnavailableElementSecond,
  NSEnumWithUnavailableElementThird __attribute__((availability(macosx, introduced=51))),
};

typedef NS_OPTIONS(NSUInteger, NSDeprecatedOptions) {
  NSDeprecatedOptionsNone = 0,
  NSDeprecatedOptionsFirst   = (1 << 0)
}  __attribute__((availability(macosx, introduced=51, deprecated=51, message="Use a different API")));

typedef NS_ENUM(NSUInteger, NSDeprecatedEnum) {
  NSDeprecatedEnumFirst
} __attribute__((availability(macosx, introduced=51, deprecated=51, message="Use a different API")));

typedef NS_OPTIONS(NSUInteger, NSExplicitlyUnavailableOptions) {
  NSExplicitlyUnavailableOptionsNone = 0,
  NSExplicitlyUnavailableOptionsFirst   = (1 << 0)
} __attribute__((unavailable));

typedef NS_OPTIONS(NSUInteger, NSExplicitlyUnavailableOnOSXOptions) {
  NSExplicitlyUnavailableOnOSXOptionsNone = 0,
  NSExplicitlyUnavailableOnOSXOptionsFirst   = (1 << 0)
}  __attribute__((availability(macosx, unavailable, message="Use a different API")));


@interface NSClassWithDeprecatedOptionsInMethodSignature : NSObject
+ (NSClassWithDeprecatedOptionsInMethodSignature *) sharedInstance;
@end

@interface NSClassWithDeprecatedOptionsInMethodSignature (ActuallyUseOptions)
  - (void)someMethodWithDeprecatedOptions:(NSDeprecatedOptions)options __attribute__((availability(macosx, introduced=51, deprecated=51, message="Use a different API")));
@end

@interface NSClassWithExplicitlyUnavailableOptionsInMethodSignature : NSObject
+ (NSClassWithExplicitlyUnavailableOptionsInMethodSignature *) sharedInstance;
@end

@interface NSClassWithExplicitlyUnavailableOptionsInMethodSignature (ActuallyUseOptions)
- (void)someMethodWithUnavailableOptions:(NSExplicitlyUnavailableOptions)options __attribute__((unavailable));
- (void)someMethodWithUnavailableOptionsOnOSX:(NSExplicitlyUnavailableOnOSXOptions)options __attribute__((availability(macosx, unavailable, message="Use a different API")));
@end

@interface NSClassWithPotentiallyUnavailableOptionsInMethodSignature : NSObject
+ (NSClassWithPotentiallyUnavailableOptionsInMethodSignature *) sharedInstance;
- (void)someMethodWithPotentiallyUnavailableOptions:(NSPotentiallyUnavailableOptions)options __attribute__((availability(macosx, introduced=52)));
@end

@protocol NSWobbling
-(void)wobble;

- (instancetype)returnMyself; 

@optional
-(void)wibble;

- (id)objectAtIndexedSubscript:(NSUInteger)idx;
@end

@protocol NSMaybeInitWobble
@optional
- (id)initWithWobble:(int)wobble;
@end

@interface NSURLRequest : NSObject
- (instancetype)requestWithString:(NSString *)URLString;
+ (instancetype)requestWithString:(NSString *)URLString;
+ (instancetype)URLRequestWithURL:(NSURL *)URL;
@end

NS_SWIFT_UNAVAILABLE("NSInvocation and related APIs not available")
@interface NSInvocation : NSObject
@end

typedef NS_ENUM(NSInteger, NSByteCountFormatterCountStyle) {
  NSByteCountFormatterCountStyleFile    = 0,
  NSByteCountFormatterCountStyleMemory  = 1,
  NSByteCountFormatterCountStyleDecimal = 2,
  NSByteCountFormatterCountStyleBinary  = 3
};

@interface NSByteCountFormatter : NSObject

@property NSByteCountFormatterCountStyle countStyle;

@end

NSArray *arrayToArray(NSArray *arr);
NSDictionary *dictToDict(NSDictionary *dict);
NSSet *setToSet(NSSet *dict);

@interface NSExtensionContext : NSObject
- (void)openURL:(NSURL *)URL completionHandler:(void (^)(BOOL success))completionHandler;
// Fake API, for testing initialisms.
- (void)openGUID:(NSGUID *)GUID completionHandler:(void (^)(BOOL success))completionHandler;
@end

@interface NSProcessInfo : NSObject
@property (class, readonly, strong, nonnull) NSProcessInfo *processInfo;
@end

@interface NSString(FoundationExts)
- (void)notBridgedMethod;
@end

@interface NSString(FoundationExts)
@property (nonatomic, copy) NSString *uppercaseString;
@end

typedef struct {
    unsigned long state;
    id __unsafe_unretained *itemsPtr;
    unsigned long *mutationsPtr;
    unsigned long extra[5];
} NSFastEnumerationState;

@protocol NSFastEnumeration

- (NSUInteger)countByEnumeratingWithState:(nonnull NSFastEnumerationState *)state objects:(id __unsafe_unretained [])buffer count:(NSUInteger)len;

@end

typedef const void * CFTypeRef;
typedef const struct __attribute__((objc_bridge(NSString))) __CFString * CFStringRef;
typedef const struct __attribute__((objc_bridge(NSDictionary))) __CFDictionary * CFDictionaryRef;
typedef struct CGColor *CGColorRef;

extern CFTypeRef CFRetain(CFTypeRef cf);
extern void CFRelease(CFTypeRef cf);
extern CFTypeRef CFAutorelease(CFTypeRef __attribute__((cf_consumed)) arg) __attribute__((availability(macosx,introduced=10.9)));
extern CGColorRef CGColorRetain(CGColorRef color) __attribute__((availability(macosx,introduced=10.3)));
extern void CGColorRelease(CGColorRef color) __attribute__((availability(macosx,introduced=10.3)));

@interface NSObject (NSDistributedObjects)
@property (readonly) Class classForPortCoder NS_SWIFT_UNAVAILABLE("Use NSXPCConnection instead");
@end

typedef NSString *_Nonnull NSNotificationName
    __attribute((swift_newtype(struct)));
typedef NSString *_Nonnull NSFileAttributeKey
    __attribute((swift_newtype(struct)));

NS_SWIFT_UNAVAILABLE("Use NSXPCConnection instead")
extern NSString * const NSConnectionReplyMode;
NS_SWIFT_UNAVAILABLE("Use NSXPCConnection instead")
extern NSString * const NSConnectionDidDieNotification;
NS_SWIFT_UNAVAILABLE("Use NSXPCConnection instead")
@interface NSConnection : NSObject {
}
@end
NS_SWIFT_UNAVAILABLE("Use NSXPCConnection instead")
@interface NSPortCoder : NSCoder
@end
NS_SWIFT_UNAVAILABLE("Use NSXPCConnection instead")
@protocol NSConnectionDelegate <NSObject>
@end
NS_SWIFT_UNAVAILABLE("Use NSXPCConnection instead")
@interface NSDistantObjectRequest : NSObject
@end
NS_SWIFT_UNAVAILABLE("Use NSXPCConnection instead")
@interface NSDistantObject
@end
NS_SWIFT_UNAVAILABLE("Use NSXPCConnection instead")
@interface NSPortNameServer : NSObject
@end
NS_SWIFT_UNAVAILABLE("Use NSXPCConnection instead")
@interface NSMachBootstrapServer : NSPortNameServer
@end
NS_SWIFT_UNAVAILABLE("Use NSXPCConnection instead")
@interface NSMessagePortNameServer : NSPortNameServer
@end
NS_SWIFT_UNAVAILABLE("Use NSXPCConnection instead")
@interface NSSocketPortNameServer : NSPortNameServer
@end
NS_SWIFT_UNAVAILABLE("Use NSCalendar and NSDateComponents and NSDateFormatter instead")
@interface NSCalendarDate : NSDate
@end
NS_SWIFT_UNAVAILABLE("NSInvocation and related APIs not available")
@interface NSInvocationOperation : NSObject
@end
NS_SWIFT_UNAVAILABLE("NSInvocation and related APIs not available")
@interface NSMethodSignature : NSObject
@end
/// Unavailable Global Functions
extern void NSSetZoneName(NSZone *_Nonnull zone, NSString *_Nonnull name)
    NS_SWIFT_UNAVAILABLE("Zone-based memory management is unavailable");
extern NSString *NSZoneName(NSZone *zone) NS_SWIFT_UNAVAILABLE("Zone-based memory management is unavailable");
extern NSZone *NSCreateZone(NSUInteger startSize, NSUInteger granularity, BOOL canFree) NS_SWIFT_UNAVAILABLE("Zone-based memory management is unavailable");

@interface NSXPCInterface : NSObject
+ (NSXPCInterface *)interfaceWithProtocol:(Protocol *)protocol;
@end

typedef struct NonNilableReferences {
  NSObject *_Nonnull __unsafe_unretained obj;
} NonNilableReferences;


@protocol NSProtocolWithOptionalRequirement
@optional
-(void)optionalRequirement;
-(DummyClass *)optionalRequirementMethodWithIUOResult;
@end

@interface NSClassWithMethodFromNSProtocolWithOptionalRequirement
-(void)optionalRequirement  __attribute__((availability(macosx, introduced=51)));
@end

__attribute__((availability(macosx, introduced = 51)))
@interface AnnotatedFrameworkClass : NSObject
@end

__attribute__((availability(macosx, introduced = 52)))
@interface AnnotatedLaterFrameworkClass : NSObject
@end

/// Aaa.  UnannotatedFrameworkProtocol.  Bbb.
@protocol UnannotatedFrameworkProtocol
- (void)doSomethingWithClass:(AnnotatedFrameworkClass *_Nullable)k;
- (void)doSomethingWithNonNullableClass:(AnnotatedFrameworkClass *_Nonnull)k;
- (void)doSomethingWithIUOClass:(AnnotatedFrameworkClass *_Null_unspecified)k;
- (AnnotatedFrameworkClass *_Nullable)returnSomething;

-(void)noUnavailableTypesInSignature;

- (void)doSomethingWithClass:(AnnotatedFrameworkClass *_Nonnull)k
               andLaterClass:(AnnotatedLaterFrameworkClass *_Nonnull)lk;

-(void)someMethodWithAvailability __attribute__((availability(macosx,introduced=53)));

@property(nonnull) AnnotatedFrameworkClass *someProperty;

@end

/// Aaa.  AnnotatedFrameworkProtocol.  Bbb.
__attribute__((availability(macosx, introduced = 10.9)))
@protocol AnnotatedFrameworkProtocol
- (AnnotatedFrameworkClass * _Nullable) returnSomething;
@end

/// Aaa.  FrameworkClassConformingToUnannotatedFrameworkProtocol.  Bbb.
@interface FrameworkClassConformingToUnannotatedFrameworkProtocol : NSObject<UnannotatedFrameworkProtocol>
@end

/// Aaa.  LaterFrameworkClassConformingToUnannotatedFrameworkProtocol.  Bbb.
__attribute__((availability(macosx, introduced = 52)))
@interface LaterFrameworkClassConformingToUnannotatedFrameworkProtocol : NSObject<UnannotatedFrameworkProtocol>
@end

/// Aaa.  LaterAnnotatedFrameworkProtocol.  Bbb.
__attribute__((availability(macosx, introduced = 52)))
@protocol LaterAnnotatedFrameworkProtocol
- (AnnotatedFrameworkClass * _Nullable) returnSomething;
- (void)doSomethingWithClass:(AnnotatedFrameworkClass *_Nonnull)k
               andLaterClass:(AnnotatedLaterFrameworkClass *_Nonnull)lk;
-(void)noUnavailableTypesInSignature;
-(void)someMethodWithAvailability __attribute__((availability(macosx,introduced=53)));
@property(nonnull) AnnotatedFrameworkClass *someProperty;
@end

/// Aaa.  FrameworkClassConformingToLaterAnnotatedFrameworkProtocol.  Bbb.
@interface FrameworkClassConformingToLaterAnnotatedFrameworkProtocol : NSObject<LaterAnnotatedFrameworkProtocol>
@end

@interface UnusedResults : NSObject
-(NSInteger)producesResult __attribute__((warn_unused_result));
@end

@interface NSObject (Silly)
- (void)doSelector:(nonnull SEL)selector;
@end

@interface Bee (Gerunds)
- (void)startSquashingBee:(nonnull Bee *)bee;
- (void)startSoothingBee:(nonnull Bee *)bee;
- (void)startShoppingBee:(nonnull Bee *)bee;
@end

@interface NSMutableArray<ObjectType> : NSArray
- (void)addObjects:(nonnull NSArray<ObjectType> *)objects;
@end

@interface NSString (Slicing)
- (nonnull NSString *)sliceFromIndex:(NSInteger)fromIndex toIndex:(NSInteger)toIndex;
@end

@interface NSString (Appending)
- (nonnull NSString *)stringByAppendingString:(nonnull NSString *)string;
- (nonnull NSString *)stringWithString:(nonnull NSString *)string;
- (nullable NSURL *)URLWithAddedString:(nonnull NSString *)string;
// Fake API for testing initialisms.
- (nullable NSGUID *)GUIDWithAddedString:(nonnull NSString *)string;
- (NSString *)stringForCalendarUnits:(NSCalendarUnit)units;
@end

@interface NSURL (Properties)
@property (readonly, nullable) NSURL *URLByDeletingLastPathComponent;
@property (readonly, nonnull) NSURL *URLWithHTTPS;
@end

@interface NSGUID (Properties)
@property (readonly, nullable) NSGUID *GUIDByCanonicalizing;
@property (readonly, nonnull) NSGUID *GUIDWithContext;
@end

typedef NS_OPTIONS(NSUInteger, NSEnumerationOptions) {
   NSEnumerationConcurrent = (1UL << 0),
   NSEnumerationReverse = (1UL << 1),
};

@interface NSArray (Enumeration)
- (void)enumerateObjectsUsingBlock:(void (^)(id obj,
                                             NSUInteger idx,
                                             BOOL *stop))block;

- (void)enumerateObjectsWithOptions:(NSEnumerationOptions)opts
                         usingBlock:(void (^)(id obj, NSUInteger idx,
                                              BOOL *stop))block;

- (void)enumerateObjectsRandomlyWithBlock:
    (void (^_Nullable)(id obj, NSUInteger idx, BOOL *stop))block;

- (void)enumerateObjectsHaphazardly:(void (^_Nullable)(id obj, NSUInteger idx,
                                                       BOOL *stop))block;

- (void)optionallyEnumerateObjects:(NSEnumerationOptions)opts
                              body:(void (^)(id obj, NSUInteger idx,
                                             BOOL *stop))block;

- (void)enumerateObjectsWhileOrderingPizza:(BOOL)pizza
                         withOptions:(NSEnumerationOptions)opts
                         usingBlock:(void (^)(id obj, NSUInteger idx,
                                              BOOL *stop))block;

- (void)doSomethingWithCopying:(nonnull id<NSCopying>)copying;
- (void)doSomethingElseWithCopying:(nonnull NSObject<NSCopying> *)copying;

- (void)enumerateObjectsWithNullableBlock:
    (void (^_Nullable)(id obj, NSUInteger idx, BOOL *stop))block;
@end

@interface NSMutableArray (Sorting)
- (void)sortUsingFunction:(NSInteger (*_Nonnull)(_Nonnull id,
                                                 _Nonnull id))fimctopn;
@end

@interface NSMutableArray (Removal)
- (void)removeObjects:(nonnull NSArray *)objects;
@end

@interface NSMutableArray (TypeSuffix)
- (void)doSomethingWithUnderlying:(NSUnderlyingType)underlying;
- (void)setDefaultEnumerationOptions:(NSEnumerationOptions)options;
@end

@interface NSString ()
- (nonnull NSString *)stringByNormalizingXMLPreservingComments:(BOOL)preserveFlag;
@end

@interface NSSet ()
- (nonnull NSSet *)setByAddingObject:(nonnull id)object;
@property (readonly) BOOL empty;
- (BOOL)nonEmpty;
@property (readonly) BOOL isStringSet;
@property (readonly) BOOL wantsAUnion;
@property (readonly) BOOL watchesItsLanguage;
@property (readonly) BOOL appliesForAJob;
@property (readonly) BOOL setShouldBeInfinite;
@end

int variadicFunc1(int A, ...);

int variadicFunc2(int A, ...);

@interface NSString (UTF8)
-(nullable instancetype)initWithUTF8String:(const char *)bytes;
@end

extern NSString *NSGlobalConstant;
extern void NSGlobalFunction(void);

extern void NS123(void);
extern void NSYELLING(void);
extern void NS_SCREAMING(void);
extern void NS_(void);
extern NSString *NSHTTPRequestKey;

@interface NSString (URLExtraction)
@property (nonnull,copy,readonly) NSArray<NSURL *> *URLsInText;
@property (nonnull,copy,readonly) NSArray<NSGUID *> *GUIDsInText;
@end

@interface NSObject (Selectors)
- (void)messageSomeObject:(nonnull id)object selector:(nonnull SEL)selector;
@end

@interface NSOperation : NSObject
@end

@interface NSProgress : NSObject
@end

@protocol NSProgressReporting <NSObject>
@property (readonly) NSProgress *progress;
@end

@interface NSIdLover: NSObject

- (id _Nonnull)makesId;
- (void)takesId:(id _Nonnull)x;
- (void)takesArrayOfId:(const id _Nonnull * _Nonnull)x;
- (void)takesNullableId:(id _Nullable)x;

@property (strong) id propertyOfId;

@end

@protocol NSIdLoving
- (void)takesIdViaProtocol:(id _Nonnull)x;
@end

#define NSTimeIntervalSince1970 978307200.0
#define NS_DO_SOMETHING 17

typedef NS_ENUM(NSUInteger, NSClothingStyle) {
  NSClothingStyleFormal = 0,
  NSClothingStyleSemiFormal,
  NSClothingStyleHipster,
  NSClothingStyleHippie
};
static const NSClothingStyle NSClothingStyleOfficeCasual __attribute__((availability(swift,unavailable,replacement="NSClothingStyleSemiFormal"))) = NSClothingStyleSemiFormal;

void acceptError(NSError * _Nonnull error);
NSError * _Nonnull produceError(void);
NSError * _Nullable produceOptionalError(void);

extern NSString * const FictionalServerErrorDomain;

typedef enum __attribute__((ns_error_domain(FictionalServerErrorDomain))) FictionalServerErrorCode : NSInteger {
  FictionalServerErrorMeltedDown = 1
} FictionalServerErrorCode;

@protocol Wearable
- (void)wear;
@end

@protocol Garment
@end

@protocol Cotton
@end

@interface Coat : NSObject<Wearable>

- (void)wear;
@property (class) Coat <Wearable> *fashionStatement;

@end

@protocol NSLaundry
- (void)wash:(Coat <Garment> * _Nonnull)garment;
- (void)bleach:(Coat <Garment, Cotton> * _Nonnull)garment;
- (Coat <Garment> * _Nonnull)dry;
@end

@interface NSLaundromat : NSObject
@end

extern NSString * const NSLaundryErrorDomain;

typedef enum __attribute__((ns_error_domain(NSLaundryErrorDomain))) __attribute__((swift_name("NSLaundromat.Error"))) NSLaundryErrorCode {
    NSLaundryErrorTooMuchSoap = 1,
    NSLaundryErrorCatInWasher = 2
};

typedef void (*event_handler)(_Nonnull id);
void install_global_event_handler(_Nullable event_handler handler);

@interface NSObject ()
- (void) addObserver: (id) observer
         forKeyPath: (NSString*) keyPath
         options: (NSInteger) options
         context: (void*) context;
- (void) removeObserver: (id) observer
         forKeyPath: (NSString*) keyPath
         context: (void*) options;
@end

_Nullable id returnNullableId(void);
void takeNullableId(_Nullable id);

@interface I
@end

@protocol OptionalRequirements
@optional
- (Coat *)optional;
@property NSString *name;
@end

@interface IUOProperty
@property (readonly) id<OptionalRequirements> iuo;
@end

@interface ColorDescriptor : NSObject <NSCopying>
@end

@interface ColorArray : NSObject
- (ColorDescriptor *)objectAtIndexedSubscript:(NSUInteger)attachmentIndex;
- (void)setObject:(nullable ColorDescriptor *)attachment atIndexedSubscript:(NSUInteger)attachmentIndex;
@end

@interface PaletteDescriptor : NSObject <NSCopying>
@property (readonly, nonnull) ColorArray *colors;
@end
