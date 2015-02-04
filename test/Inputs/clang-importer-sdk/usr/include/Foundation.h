#if __has_feature(modules)
@import ObjectiveC;
@import CoreFoundation;
@import CoreGraphics;
#else
#import <objc/NSObject.h>
#import <CoreFoundation.h>
#import <CoreGraphics.h>
#endif

typedef struct objc_object { void *isa; } *id;

typedef struct _NSZone NSZone;
void *allocate(NSZone *zone);

typedef double NSTimeInterval;

extern NSUInteger NSRealMemoryAvailable(void) __attribute__((availability(macosx,introduced=10.0 ,deprecated=10.8,message="" ))) __attribute__((availability(ios,introduced=2.0 ,deprecated=6.0,message="" )));
extern NSUInteger SomeCrazyAppExtensionForbiddenAPI(void)
  __attribute__((availability(macosx_app_extension,unavailable,message="Not available in App Extensions")))
  __attribute__((availability(ios_app_extension,unavailable,message="Not available in App Extensions")));

#define NS_SWIFT_UNAVAILABLE __attribute__((annotate("swift1_unavailable")))

NS_SWIFT_UNAVAILABLE void NSSwiftUnavailableFunction();

__attribute__((availability(macosx,introduced=10.10)))
@interface NSAvailableOn10_10 : NSObject
- (instancetype)init;
- (instancetype)initWithStringOn10_11:(NSString *)s __attribute__((availability(macosx,introduced=10.11)));

@property NSInteger propertyOn10_11 __attribute__((availability(macosx,introduced=10.11)));

- (void)methodAvailableOn10_11 __attribute__((availability(macosx,introduced=10.11)));
@end

__attribute__((availability(macosx,introduced=10.10)))
@protocol NSProtocolAvailableOn10_10

@end

@class NSString, NSArray, NSDictionary, NSSet, NSEnumerator;

/// Aaa.  NSArray.  Bbb.
@interface NSArray : NSObject
- (id)objectAtIndexedSubscript:(NSUInteger)idx;
- description;

@property NSString *nsstringProperty;
@property BOOL boolProperty;
@property NSArray *arrayProperty;
@property NSDictionary *dictProperty;
@property NSSet *setProperty;

+ (instancetype)arrayWithObjects:(const id[])objects count:(NSUInteger)count;
- (void)makeObjectsPerformSelector:(SEL)aSelector;
- (void)makeObjectsPerformSelector:(SEL)aSelector withObject:(id)anObject;
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

@interface NSDictionary : NSObject /*<NSCopying, NSMutableCopying, NSSecureCoding, NSFastEnumeration>*/
@property (readonly) NSUInteger count;
- (id)objectForKey:(id)aKey;
- (NSEnumerator *)keyEnumerator;
@end
@interface NSDictionary (NSExtendedDictionary)
- (id)objectForKeyedSubscript:(id)key /*NS_AVAILABLE(10_8, 6_0)*/;
@end

@interface NSDictionary (Inits)
- (instancetype)init;
@end

@interface NSMutableDictionary : NSDictionary
- (void)removeObjectForKey:(id)aKey;
- (void)setObject:(id)anObject forKey:(id <NSCopying>)aKey;
@end

@interface NSMutableDictionary (NSExtendedMutableDictionary)
- (void)setObject:(id)obj forKeyedSubscript:(id <NSCopying>)key /*NS_AVAILABLE(10_8, 6_0)*/;
@end

@interface NSSet : NSObject
- (instancetype)init;
- (NSUInteger)count;
- (id)anyObject;
@end

@interface NSMutableSet : NSSet
- (void)addObject:(id)obj;
- (void)removeObject:(id)obj;
@end

@interface NSNumber : NSObject
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

@interface NSMutableString : NSString
@end

BOOL BOOLtoBOOL(BOOL b);

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

typedef NS_ENUM(NSUInteger, NSPostingStyle) {
  NSPostWhenIdle = 1,
  NSPostASAP = 2,
  NSPostNow = 3
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
  kCFCalendarUnitWeek /*CF_ENUM_DEPRECATED(10_4, 10_10, 2_0, 8_0)*/ = (1UL << 8),
  kCFCalendarUnitWeekday = (1UL << 9),
  kCFCalendarUnitWeekdayOrdinal = (1UL << 10),
  kCFCalendarUnitQuarter /*CF_ENUM_AVAILABLE(10_6, 4_0)*/ = (1UL << 11),
  kCFCalendarUnitWeekOfMonth /*CF_ENUM_AVAILABLE(10_7, 5_0)*/ = (1UL << 12),
  kCFCalendarUnitWeekOfYear /*CF_ENUM_AVAILABLE(10_7, 5_0)*/ = (1UL << 13),
  kCFCalendarUnitYearForWeekOfYear /*CF_ENUM_AVAILABLE(10_7, 5_0)*/ = (1UL << 14),
};

typedef NS_OPTIONS(NSUInteger, NSKeyValueObservingOptions) {
  NSKeyValueObservingOptionNew = 0x01,
  NSKeyValueObservingOptionOld = 0x02,
  NSKeyValueObservingOptionInitial /*NS_ENUM_AVAILABLE(10_5, 2_0)*/ = 0x04,
  NSKeyValueObservingOptionPrior /*NS_ENUM_AVAILABLE(10_5, 2_0)*/ = 0x08
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
  AlertBuzzFunk,
  AlertBuzzHero,
  AlertBuzzSosumi
};

// From AppKit
typedef NS_OPTIONS(NSUInteger, NSBitmapFormat) {
  NSAlphaFirstBitmapFormat            = 1 << 0, // 0 means is alpha last (RGBA, CMYKA, etc.)
  NSAlphaNonpremultipliedBitmapFormat = 1 << 1, // 0 means is premultiplied
  NSFloatingPointSamplesBitmapFormat  = 1 << 2,	// 0 is integer

  NS16BitLittleEndianBitmapFormat /*NS_ENUM_AVAILABLE_MAC(10_10)*/ = (1 << 8),
  NS32BitLittleEndianBitmapFormat /*NS_ENUM_AVAILABLE_MAC(10_10)*/ = (1 << 9),
  NS16BitBigEndianBitmapFormat /*NS_ENUM_AVAILABLE_MAC(10_10)*/ = (1 << 10),
  NS32BitBigEndianBitmapFormat /*NS_ENUM_AVAILABLE_MAC(10_10)*/ = (1 << 11)
};

typedef NS_OPTIONS(NSUInteger, NSBitmapFormatReversed) {
  NS16BitLittleEndianBitmapFormatR /*NS_ENUM_AVAILABLE_MAC(10_10)*/ = (1 << 8),
  NS32BitLittleEndianBitmapFormatR /*NS_ENUM_AVAILABLE_MAC(10_10)*/ = (1 << 9),
  NS16BitBigEndianBitmapFormatR /*NS_ENUM_AVAILABLE_MAC(10_10)*/ = (1 << 10),
  NS32BitBigEndianBitmapFormatR /*NS_ENUM_AVAILABLE_MAC(10_10)*/ = (1 << 11),

  NSAlphaFirstBitmapFormatR            = 1 << 0, // 0 means is alpha last (RGBA, CMYKA, etc.)
  NSAlphaNonpremultipliedBitmapFormatR = 1 << 1, // 0 means is premultiplied
  NSFloatingPointSamplesBitmapFormatR  = 1 << 2,	// 0 is integer
};

typedef NS_OPTIONS(NSUInteger, NSBitmapFormat2) {
  NSU16a,
  NSU32a,
};

typedef NS_OPTIONS(NSUInteger, NSBitmapFormat3) {
  NSU16b,
  NSU32b,
  NSS16b,
  NSS32b,
};

typedef NS_OPTIONS(NSUInteger, NSUBitmapFormat4) {
  NSU16c,
  NSU32c,
};

typedef NS_OPTIONS(NSUInteger, NSABitmapFormat5) {
  NSAA16d,
  NSAB32d,
};

/// Aaa.  NSUnavailableOptions.  Bbb.
typedef NS_OPTIONS(NSUInteger, NSUnavailableOptions) {
  NSUnavailableOptionsFirst   = (1 << 0),
  NSUnavailableOptionsSecond  = (1 << 1),
  NSUnavailableOptionsThird   = (1 << 2),
}  __attribute__((availability(macosx, introduced=10.10)));

/// Aaa.  NSOptionsWithUnavailableElement.  Bbb.
typedef NS_OPTIONS(NSUInteger, NSOptionsWithUnavailableElement) {
  NSOptionsWithUnavailableElementFirst    = (1 << 0),
  NSOptionsWithUnavailableElementSecond   = (1 << 1),
  NSOptionsWithUnavailableElementThird __attribute__((availability(macosx, introduced=10.10))) = (1 << 2),
};

/// Aaa.  NSUnavailableEnum.  Bbb.
typedef NS_ENUM(NSUInteger, NSUnavailableEnum) {
  NSUnavailableEnumFirst,
  NSUnavailableEnumSecond,
  NSUnavailableEnumThird,
}  __attribute__((availability(macosx, introduced=10.10)));

/// Aaa.  NSEnumWithUnavailableElement.  Bbb.
typedef NS_ENUM(NSUInteger, NSEnumWithUnavailableElement) {
  NSEnumWithUnavailableElementFirst,
  NSEnumWithUnavailableElementSecond,
  NSEnumWithUnavailableElementThird __attribute__((availability(macosx, introduced=10.10))),
};



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

#define NS_DESIGNATED_INITIALIZER __attribute__((objc_designated_initializer))

@interface NSURL : NSObject
+ (instancetype)URLWithString:(NSString *)URLString;
@end

@interface NSURLRequest : NSObject
+ (instancetype)requestWithString:(NSString *)URLString;
+ (instancetype)URLRequestWithURL:(NSURL *)URL;
@end

typedef struct _NSRange {
    NSUInteger location;
    NSUInteger length;
} NSRange;

@interface NSAttributedString : NSString
- (NSAttributedString *)sliceAttributedString:(NSInteger)startIndex;
@end

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
- (void)openURL:(NSURL *)URL completion:(void (^)(BOOL success))handler;
@end

@interface NSProcessInfo : NSObject
+ (NSProcessInfo *)processInfo;
@end

@interface NSString(FoundationExts)
- (void)notBridgedMethod;
@end

typedef struct {
    unsigned long state;
    id __unsafe_unretained *itemsPtr;
    unsigned long *mutationsPtr;
    unsigned long extra[5];
} NSFastEnumerationState;

@protocol NSFastEnumeration

- (NSUInteger)countByEnumeratingWithState:(NSFastEnumerationState *)state objects:(id __unsafe_unretained [])buffer count:(NSUInteger)len;

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
@property (readonly) Class classForPortCoder;
@end

extern NSString * const NSConnectionReplyMode;
extern NSString * const NSConnectionDidDieNotification;
@interface NSConnection : NSObject {
}
@end
@interface NSPortCoder : NSCoder
@end
@protocol NSConnectionDelegate <NSObject>
@end
@interface NSDistantObjectRequest : NSObject
@end
@interface NSDistantObject
@end
@interface NSPortNameServer : NSObject
@end
@interface NSMachBootstrapServer : NSPortNameServer
@end
@interface NSMessagePortNameServer : NSPortNameServer
@end
@interface NSSocketPortNameServer : NSPortNameServer
@end
@interface NSCalendarDate : NSDate
@end
@interface NSInvocationOperation
@end
@interface NSMethodSignature : NSObject
@end
/// Unavailable Global Functions
extern void NSSetZoneName(NSZone *zone, NSString *name);
extern NSString *NSZoneName(NSZone *zone);
extern NSZone *NSCreateZone(NSUInteger startSize, NSUInteger granularity, BOOL canFree);

@interface NSXPCInterface : NSObject
+ (NSXPCInterface *)interfaceWithProtocol:(Protocol *)protocol;
@end

typedef struct NonNilableReferences {
  NSObject *__nonnull __unsafe_unretained obj;
} NonNilableReferences;
