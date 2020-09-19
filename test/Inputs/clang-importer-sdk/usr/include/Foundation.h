// expected-* lines in this file are used in test/ClangImporter/import-decision-remarks.swift.

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

#define NS_NOESCAPE CF_NOESCAPE

typedef struct objc_object { void *isa; } *id;
// expected-remark@-1:36 {{C field 'objc_object::isa' imported as property 'isa'}}
// expected-remark@-2:16 {{C struct 'objc_object' imported as struct 'objc_object'}}
// expected-remark@-3:44 {{C typedef 'id' imported as unavailable type alias 'id'}}

typedef struct _NSZone NSZone;
// expected-remark@-1 {{could not import C struct '_NSZone'}}
// expected-note@-2 {{found only forward declarations of C struct '_NSZone'; incomplete types cannot be imported into Swift}}
// expected-remark@-3 {{could not import C typedef 'NSZone'}}
// expected-note@-4 {{type could not be imported}}
void *allocate(NSZone *zone);
// expected-remark@-1 {{C function 'allocate' imported as global function 'allocate'}}

typedef double NSTimeInterval;
// expected-remark@-1 {{C typedef 'NSTimeInterval' imported as type alias 'TimeInterval'}}

typedef struct _NSRange {
    // expected-remark@-1 {{C struct '_NSRange' imported as struct '_NSRange'}}
    NSUInteger location;
    // expected-remark@-1 {{C field '_NSRange::location' imported as property 'location'}}
    NSUInteger length;
    // expected-remark@-1 {{C field '_NSRange::length' imported as property 'length'}}
} NSRange;
// expected-remark@-1 {{C typedef 'NSRange' imported as type alias 'NSRange'}}


extern NSUInteger NSRealMemoryAvailable(void) __attribute__((availability(macosx,introduced=10.0 ,deprecated=10.8,message="" ))) __attribute__((availability(ios,introduced=2.0 ,deprecated=6.0,message="" )));
// expected-remark@-1 {{C function 'NSRealMemoryAvailable' imported as unavailable global function 'NSRealMemoryAvailable()'}}
extern NSUInteger SomeCrazyAppExtensionForbiddenAPI(void)
  __attribute__((availability(macosx_app_extension,unavailable,message="Not available in App Extensions")))
  __attribute__((availability(ios_app_extension,unavailable,message="Not available in App Extensions")));
// expected-remark@-3 {{C function 'SomeCrazyAppExtensionForbiddenAPI' imported as global function 'SomeCrazyAppExtensionForbiddenAPI()'}}

extern NSString *const globalStringAvailableOn10_51 __attribute__((availability(macosx,introduced=10.51)));
// expected-remark@-1 {{C variable 'globalStringAvailableOn10_51' imported as let 'globalStringAvailableOn10_51'}}
extern NSString *const globalStringAvailableOn10_52 __attribute__((availability(macosx,introduced=10.52)));
// expected-remark@-1 {{C variable 'globalStringAvailableOn10_52' imported as let 'globalStringAvailableOn10_52'}}

__attribute__((availability(macosx,introduced=10.51)))
@interface NSAvailableOn10_51 : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSAvailableOn10_51' imported as class 'NSAvailableOn10_51'}}
- (instancetype)init;
// expected-remark@-1 {{Objective-C method '-[NSAvailableOn10_51 init]' imported as initializer 'init()'}}
- (instancetype)initWithStringOn10_52:(NSString *)s __attribute__((availability(macosx,introduced=10.52)));
// expected-remark@-1 {{Objective-C method '-[NSAvailableOn10_51 initWithStringOn10_52:]' imported as initializer 'init(stringOn10_52:)'}}

@property NSInteger propertyOn10_52 __attribute__((availability(macosx,introduced=10.52)));
// expected-remark@-1 {{Objective-C method '-[NSAvailableOn10_51 propertyOn10_52]' imported as getter for property 'propertyOn10_52'}}
// expected-remark@-2 {{Objective-C method '-[NSAvailableOn10_51 setPropertyOn10_52:]' imported as setter for property 'propertyOn10_52'}}
// expected-remark@-3 {{Objective-C property '-NSAvailableOn10_51.propertyOn10_52' imported as property 'propertyOn10_52'}}

- (void)methodAvailableOn10_52 __attribute__((availability(macosx,introduced=10.52)));
// expected-remark@-1 {{Objective-C method '-[NSAvailableOn10_51 methodAvailableOn10_52]' imported as instance method 'methodAvailableOn10_52()'}}
@end

extern NSAvailableOn10_51 *const globalClassInstanceAvailableOn10_51 __attribute__((availability(macosx,introduced=10.51)));
// expected-remark@-1 {{C variable 'globalClassInstanceAvailableOn10_51' imported as let 'globalClassInstanceAvailableOn10_51'}}

__attribute__((availability(macosx,introduced=10.51)))
@protocol NSProtocolAvailableOn10_51
// expected-remark@-1 {{Objective-C protocol 'NSProtocolAvailableOn10_51' imported as protocol 'NSProtocolAvailableOn10_51'}}

@end

__attribute__((availability(macosx,introduced=10.9)))
@interface NSAvailableOn10_9 : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSAvailableOn10_9' imported as class 'NSAvailableOn10_9'}}

@property NSInteger propertyOn10_9;
// expected-remark@-1 {{Objective-C method '-[NSAvailableOn10_9 propertyOn10_9]' imported as getter for property 'propertyOn10_9'}}
// expected-remark@-2 {{Objective-C method '-[NSAvailableOn10_9 setPropertyOn10_9:]' imported as setter for property 'propertyOn10_9'}}
// expected-remark@-3 {{Objective-C property '-NSAvailableOn10_9.propertyOn10_9' imported as property 'propertyOn10_9'}}

// Properties with unavailable accessors declared before property.
- (void)setPropertyOn10_51WithSetterOn10_52Before:(NSInteger)prop __attribute__((availability(macosx,introduced=10.52)));
// expected-remark@-1 {{Objective-C method '-[NSAvailableOn10_9 setPropertyOn10_51WithSetterOn10_52Before:]' imported as instance method 'setPropertyOn10_51WithSetterOn10_52Before'}}
// expected-remark@-2 {{Objective-C method '-[NSAvailableOn10_9 setPropertyOn10_51WithSetterOn10_52Before:]' imported as setter for property 'propertyOn10_51WithSetterOn10_52Before'}}
@property NSInteger propertyOn10_51WithSetterOn10_52Before __attribute__((availability(macosx,introduced=10.51)));
// expected-remark@-1 {{Objective-C method '-[NSAvailableOn10_9 propertyOn10_51WithSetterOn10_52Before]' imported as getter for property 'propertyOn10_51WithSetterOn10_52Before'}}
// expected-remark@-2 {{Objective-C property '-NSAvailableOn10_9.propertyOn10_51WithSetterOn10_52Before' imported as property 'propertyOn10_51WithSetterOn10_52Before'}}

- (NSInteger)propertyOn10_51WithGetterOn10_52Before __attribute__((availability(macosx,introduced=10.52)));
// expected-remark@-1 {{Objective-C method '-[NSAvailableOn10_9 propertyOn10_51WithGetterOn10_52Before]' imported as getter for property 'propertyOn10_51WithGetterOn10_52Before'}}
// expected-remark@-2 {{Objective-C method '-[NSAvailableOn10_9 propertyOn10_51WithGetterOn10_52Before]' imported as instance method 'propertyOn10_51WithGetterOn10_52Before()'}}
@property NSInteger propertyOn10_51WithGetterOn10_52Before __attribute__((availability(macosx,introduced=10.51)));
// expected-remark@-1 {{Objective-C method '-[NSAvailableOn10_9 setPropertyOn10_51WithGetterOn10_52Before:]' imported as setter for property 'propertyOn10_51WithGetterOn10_52Before'}}
// expected-remark@-2 {{Objective-C property '-NSAvailableOn10_9.propertyOn10_51WithGetterOn10_52Before' imported as property 'propertyOn10_51WithGetterOn10_52Before'}}

// Properties with unavailable accessors declared after property.
@property NSInteger propertyOn10_51WithSetterOn10_52After __attribute__((availability(macosx,introduced=10.51)));
// expected-remark@-1 {{Objective-C method '-[NSAvailableOn10_9 propertyOn10_51WithSetterOn10_52After]' imported as getter for property 'propertyOn10_51WithSetterOn10_52After'}}
// expected-remark@-2 {{Objective-C property '-NSAvailableOn10_9.propertyOn10_51WithSetterOn10_52After' imported as property 'propertyOn10_51WithSetterOn10_52After'}}
- (void)setPropertyOn10_51WithSetterOn10_52After:(NSInteger)prop __attribute__((availability(macosx,introduced=10.52)));
// expected-remark@-1 {{Objective-C method '-[NSAvailableOn10_9 setPropertyOn10_51WithSetterOn10_52After:]' imported as setter for property 'propertyOn10_51WithSetterOn10_52After'}}

@property NSInteger propertyOn10_51WithGetterOn10_52After __attribute__((availability(macosx,introduced=10.51)));
// expected-remark@-1 {{Objective-C method '-[NSAvailableOn10_9 setPropertyOn10_51WithGetterOn10_52After:]' imported as setter for property 'propertyOn10_51WithGetterOn10_52After'}}
// expected-remark@-2 {{Objective-C property '-NSAvailableOn10_9.propertyOn10_51WithGetterOn10_52After' imported as property 'propertyOn10_51WithGetterOn10_52After'}}
- (NSInteger)propertyOn10_51WithGetterOn10_52After __attribute__((availability(macosx,introduced=10.52)));
// expected-remark@-1 {{Objective-C method '-[NSAvailableOn10_9 propertyOn10_51WithGetterOn10_52After]' imported as getter for property 'propertyOn10_51WithGetterOn10_52After'}}

// Property with redeclared with a setter in a category
@property(readonly) NSInteger readOnlyRedeclaredWithSetterInCategory __attribute__((availability(macosx,introduced=10.51)));
// expected-remark@-1 {{Objective-C method '-[NSAvailableOn10_9 readOnlyRedeclaredWithSetterInCategory]' imported as getter for property 'readOnlyRedeclaredWithSetterInCategory'}}
// expected-remark@-2 {{Objective-C property '-NSAvailableOn10_9.readOnlyRedeclaredWithSetterInCategory' imported as property 'readOnlyRedeclaredWithSetterInCategory'}}

- (void)methodAvailableOn10_52 __attribute__((availability(macosx,introduced=10.52)));
// expected-remark@-1 {{Objective-C method '-[NSAvailableOn10_9 methodAvailableOn10_52]' imported as instance method 'methodAvailableOn10_52()'}}
@end

@interface NSAvailableOn10_9 (NSWithPropertyReclarationInACategory)
// expected-FIXME-remark@-1 {{<#diagnostic#>}}

@property(readwrite) NSInteger readOnlyRedeclaredWithSetterInCategory __attribute__((availability(macosx,introduced=10.51)));
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
- (void)setReadOnlyRedeclaredWithSetterInCategory:(NSInteger)prop __attribute__((availability(macosx,introduced=10.52)));
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
@end

/// Aaa.  NSAvailableOnOSX10_51AndIOS8_0.  Bbb.
__attribute__((availability(macosx,introduced=10.51)))
__attribute__((availability(ios,introduced=8.0)))
@interface NSAvailableOnOSX10_51AndIOS8_0 : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSAvailableOnOSX10_51AndIOS8_0' imported as class 'NSAvailableOnOSX10_51AndIOS8_0'}}

@end

@class NSString, NSArray, NSDictionary, NSSet, NSEnumerator;
// FIXME: These are doubled for some reason.
// expected-remark@-2:48 2 {{could not import Objective-C class interface 'NSEnumerator'}}
// expected-note@-3:48 2 {{found only forward declarations of Objective-C class interface 'NSEnumerator'; incomplete types cannot be imported into Swift}}

@class NSMutableArray<ObjectType>;

/// Aaa.  NSArray.  Bbb.
@interface NSArray<ObjectType> : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSArray' imported as class 'NSArray'}}
- (instancetype)initWithObjects:(const ObjectType _Nonnull [_Nullable])objects
                          count:(NSUInteger)cnt NS_DESIGNATED_INITIALIZER;
// expected-remark@-2 {{Objective-C method '-[NSArray initWithObjects:count:]' imported as initializer 'init(objects:count:)'}}
// --- Emitted for +arrayWithObjects:count: ---
// expected-note@-4 {{a better candidate, Objective-C method '-[NSArray initWithObjects:count:]', is already being imported with the name 'init(objects:count:)' and the same signature}}
- (nonnull ObjectType)objectAtIndexedSubscript:(NSUInteger)idx;
// expected-remark@-1 {{Objective-C method '-[NSArray objectAtIndexedSubscript:]' imported as unavailable instance method 'objectAtIndexedSubscript'}}
- description;
// expected-remark@-1 {{Objective-C method '-[NSArray description]' imported as instance method 'description()'}}
- (void)makeObjectsPerformSelector:(nonnull SEL)aSelector;
// expected-remark@-1 {{Objective-C method '-[NSArray makeObjectsPerformSelector:]' imported as instance method 'makeObjectsPerform'}}
- (void)makeObjectsPerformSelector:(nonnull SEL)aSelector withObject:(nullable ObjectType)anObject;
// expected-remark@-1 {{Objective-C method '-[NSArray makeObjectsPerformSelector:withObject:]' imported as instance method 'makeObjectsPerform(_:with:)'}}
- (void)makeObjectsPerformSelector:(nonnull SEL)aSelector withObject:(nullable ObjectType)anObject withObject:(nullable ObjectType)anotherObject;
// expected-remark@-1 {{Objective-C method '-[NSArray makeObjectsPerformSelector:withObject:withObject:]' imported as instance method 'makeObjectsPerform(_:with:with:)'}}
- (nonnull NSMutableArray<ObjectType> *)mutableCopy;
// expected-remark@-1 {{Objective-C method '-[NSArray mutableCopy]' imported as instance method 'mutableCopy()'}}
@end

@interface NSArray<ObjectType>(NSArrayCreation)
// expected-remark@-1 {{Objective-C category 'NSArray(NSArrayCreation)' imported as extension of 'NSArray'}}
+ (instancetype)arrayWithObjects:(const ObjectType _Nonnull [_Nullable])objects
                           count:(NSUInteger)cnt;
// expected-remark@-2 {{could not import Objective-C method '+[NSArray(NSArrayCreation) arrayWithObjects:count:]'}}
@end

@interface NSArray (AddingObject)
// expected-remark@-1 {{Objective-C category 'NSArray(AddingObject)' imported as extension of 'NSArray'}}
- (NSInteger)indexOfObject:(nonnull id)object;
// expected-remark@-1 {{Objective-C method '-[NSArray(AddingObject) indexOfObject:]' imported as instance method 'index(of:)'}}
@end

@interface DummyClass : NSObject
// expected-remark@-1 {{Objective-C class interface 'DummyClass' imported as class 'DummyClass'}}
- (nonnull id)objectAtIndexedSubscript:(NSUInteger)idx;
// expected-remark@-1 {{Objective-C method '-[DummyClass objectAtIndexedSubscript:]' imported as unavailable instance method 'objectAtIndexedSubscript'}}
// FIXME: Subscript???
- description;
// expected-remark@-1 {{Objective-C method '-[DummyClass description]' imported as instance method 'description()'}}

@property NSString *nsstringProperty;
// expected-remark@-1 {{Objective-C method '-[DummyClass nsstringProperty]' imported as getter for property 'nsstringProperty'}}
// expected-remark@-2 {{Objective-C method '-[DummyClass setNsstringProperty:]' imported as setter for property 'nsstringProperty'}}
// expected-remark@-3 {{Objective-C property '-DummyClass.nsstringProperty' imported as property 'nsstringProperty'}}
@property BOOL boolProperty;
// expected-remark@-1 {{Objective-C method '-[DummyClass boolProperty]' imported as getter for property 'boolProperty'}}
// expected-remark@-2 {{Objective-C method '-[DummyClass setBoolProperty:]' imported as setter for property 'boolProperty'}}
// expected-remark@-3 {{Objective-C property '-DummyClass.boolProperty' imported as property 'boolProperty'}}
@property NSArray *arrayProperty;
// expected-remark@-1 {{Objective-C method '-[DummyClass arrayProperty]' imported as getter for property 'arrayProperty'}}
// expected-remark@-2 {{Objective-C method '-[DummyClass setArrayProperty:]' imported as setter for property 'arrayProperty'}}
// expected-remark@-3 {{Objective-C property '-DummyClass.arrayProperty' imported as property 'arrayProperty'}}
@property NSDictionary *dictProperty;
// expected-remark@-1 {{Objective-C method '-[DummyClass dictProperty]' imported as getter for property 'dictProperty'}}
// expected-remark@-2 {{Objective-C method '-[DummyClass setDictProperty:]' imported as setter for property 'dictProperty'}}
// expected-remark@-3 {{Objective-C property '-DummyClass.dictProperty' imported as property 'dictProperty'}}
@property NSSet *setProperty;
// expected-remark@-1 {{Objective-C method '-[DummyClass setProperty]' imported as getter for property 'setProperty'}}
// expected-remark@-2 {{Objective-C method '-[DummyClass setSetProperty:]' imported as setter for property 'setProperty'}}
// expected-remark@-3 {{Objective-C property '-DummyClass.setProperty' imported as property 'setProperty'}}

- (nonnull NSString*) fetchNonnullString;
// expected-remark@-1 {{Objective-C method '-[DummyClass fetchNonnullString]' imported as instance method 'fetchNonnullString()'}}
- (nullable NSString*) fetchNullableString;
// expected-remark@-1 {{Objective-C method '-[DummyClass fetchNullableString]' imported as instance method 'fetchNullableString()'}}
- (null_unspecified NSString*) fetchNullproneString;
// expected-remark@-1 {{Objective-C method '-[DummyClass fetchNullproneString]' imported as instance method 'fetchNullproneString()'}}

- (void) takeNonnullString: (nonnull NSString*) string;
// expected-remark@-1 {{Objective-C method '-[DummyClass takeNonnullString:]' imported as instance method 'takeNonnullString'}}
- (void) takeNullableString: (nullable NSString*) string;
// expected-remark@-1 {{Objective-C method '-[DummyClass takeNullableString:]' imported as instance method 'takeNullableString'}}
- (void) takeNullproneString: (null_unspecified NSString*) string;
// expected-remark@-1 {{Objective-C method '-[DummyClass takeNullproneString:]' imported as instance method 'takeNullproneString'}}

@property(readwrite) _Nonnull NSString *nonnullStringProperty;
// expected-remark@-1 {{Objective-C method '-[DummyClass nonnullStringProperty]' imported as getter for property 'nonnullStringProperty'}}
// expected-remark@-2 {{Objective-C method '-[DummyClass setNonnullStringProperty:]' imported as setter for property 'nonnullStringProperty'}}
// expected-remark@-3 {{Objective-C property '-DummyClass.nonnullStringProperty' imported as property 'nonnullStringProperty'}}
@property(readwrite) _Nullable NSString *nullableStringProperty;
// expected-remark@-1 {{Objective-C method '-[DummyClass nullableStringProperty]' imported as getter for property 'nullableStringProperty'}}
// expected-remark@-2 {{Objective-C method '-[DummyClass setNullableStringProperty:]' imported as setter for property 'nullableStringProperty'}}
// expected-remark@-3 {{Objective-C property '-DummyClass.nullableStringProperty' imported as property 'nullableStringProperty'}}
@property(readwrite) _Null_unspecified NSString *nullproneStringProperty;
// expected-remark@-1 {{Objective-C method '-[DummyClass nullproneStringProperty]' imported as getter for property 'nullproneStringProperty'}}
// expected-remark@-2 {{Objective-C method '-[DummyClass setNullproneStringProperty:]' imported as setter for property 'nullproneStringProperty'}}
// expected-remark@-3 {{Objective-C property '-DummyClass.nullproneStringProperty' imported as property 'nullproneStringProperty'}}

@end

@interface DummyClass (Extras)
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
@property NSString *nsstringProperty2;
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
@end

@interface NSCoder : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSCoder' imported as class 'NSCoder'}}
@end

@protocol NSCoding
// expected-remark@-1 {{Objective-C protocol 'NSCoding' imported as protocol 'NSCoding'}}
- (nullable instancetype)initWithCoder:(nonnull NSCoder *)aCoder;
// expected-remark@-1 {{Objective-C method '-[NSCoding initWithCoder:]' imported as initializer 'init(coder:)'}}
@end

@protocol NSSecureCoding <NSCoding>
// expected-remark@-1 {{Objective-C protocol 'NSSecureCoding' imported as protocol 'NSSecureCoding'}}
@end

@protocol NSCopying
// expected-remark@-1 {{Objective-C protocol 'NSCopying' imported as protocol 'NSCopying'}}
- (id)copyWithZone:(nullable NSZone *)zone;
// expected-remark@-1 {{Objective-C method '-[NSCopying copyWithZone:]' imported as instance method 'copy(with:)'}}
@end

@interface NSDictionary<KeyType, ObjectType> : NSObject /*<NSCopying, NSMutableCopying, NSSecureCoding, NSFastEnumeration>*/
// expected-remark@-1 {{Objective-C class interface 'NSDictionary' imported as class 'NSDictionary'}}
@property (readonly) NSUInteger count;
// expected-remark@-1 {{Objective-C method '-[NSDictionary count]' imported as getter for property 'count'}}
// expected-remark@-2 {{Objective-C property '-NSDictionary.count' imported as property 'count'}}
- (nullable ObjectType)objectForKey:(nonnull KeyType)aKey;
// expected-remark@-1 {{Objective-C method '-[NSDictionary objectForKey:]' imported as instance method 'object(forKey:)'}}
- (nonnull NSEnumerator *)keyEnumerator;
// expected-remark@-1 {{could not import Objective-C method '-[NSDictionary keyEnumerator]'}}
// expected-note@-2 {{type could not be imported}}
@end
@interface NSDictionary<KeyType, ObjectType> (NSExtendedDictionary)
// expected-remark@-1 {{Objective-C category 'NSDictionary(NSExtendedDictionary)' imported as extension of 'NSDictionary'}}
- (nullable ObjectType)objectForKeyedSubscript:(nonnull KeyType)key;
// expected-remark@-1 {{Objective-C method '-[NSDictionary(NSExtendedDictionary) objectForKeyedSubscript:]' imported as unavailable instance method 'objectForKeyedSubscript'}}
// FIXME: Subscript?
@end

@interface NSDictionary (Inits)
// expected-remark@-1 {{Objective-C category 'NSDictionary(Inits)' imported as extension of 'NSDictionary'}}
- (nonnull instancetype)init;
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
@end

@interface NSMutableDictionary<KeyType : id<NSCopying>, ObjectType> : NSDictionary<KeyType, ObjectType>
// expected-remark@-1 {{Objective-C class interface 'NSMutableDictionary' imported as class 'NSMutableDictionary'}}
- (void)removeObjectForKey:(nonnull KeyType)aKey;
// expected-remark@-1 {{Objective-C method '-[NSMutableDictionary removeObjectForKey:]' imported as instance method 'removeObject(forKey:)'}}
- (void)setObject:(nonnull ObjectType)anObject forKey:(nonnull KeyType)aKey;
// expected-remark@-1 {{Objective-C method '-[NSMutableDictionary setObject:forKey:]' imported as instance method 'setObject(_:forKey:)'}}
@end

@interface NSMutableDictionary<KeyType, ObjectType> (NSExtendedMutableDictionary)
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
- (void)setObject:(nullable ObjectType)obj forKeyedSubscript:(nonnull KeyType)key;
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
@end

@interface NSSet<KeyType> : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSSet' imported as class 'NSSet'}}
- (nonnull instancetype)init;
// expected-remark@-1 {{Objective-C method '-[NSSet init]' imported as initializer 'init()'}}
- (NSUInteger)count;
// expected-remark@-1 {{Objective-C method '-[NSSet count]' imported as instance method 'count()'}}
- (nonnull KeyType)anyObject;
// expected-remark@-1 {{Objective-C method '-[NSSet anyObject]' imported as instance method 'anyObject()'}}
- (nonnull instancetype)initWithArray:(nonnull NSArray<KeyType> *)array;
// expected-remark@-1 {{Objective-C method '-[NSSet initWithArray:]' imported as initializer 'init(array:)'}}
@end

@interface NSMutableSet<KeyType> : NSSet<KeyType>
// expected-remark@-1 {{Objective-C class interface 'NSMutableSet' imported as class 'NSMutableSet'}}
- (void)addObject:(KeyType)obj;
// expected-remark@-1 {{Objective-C method '-[NSMutableSet addObject:]' imported as instance method 'add'}}
- (void)removeObject:(KeyType)obj;
// expected-remark@-1 {{Objective-C method '-[NSMutableSet removeObject:]' imported as instance method 'remove'}}
@end

@interface NSCountedSet<KeyType> : NSMutableSet<KeyType>
// expected-remark@-1 {{Objective-C class interface 'NSCountedSet' imported as class 'NSCountedSet'}}
- (instancetype)initWithCapacity:(NSUInteger)numItems NS_DESIGNATED_INITIALIZER;
// expected-remark@-1 {{Objective-C method '-[NSCountedSet initWithCapacity:]' imported as initializer 'init(capacity:)'}}
- (instancetype)initWithArray:(NSArray<KeyType> *)array;
// expected-remark@-1 {{Objective-C method '-[NSCountedSet initWithArray:]' imported as initializer 'init(array:)'}}
@end

@interface NSValue : NSObject <NSCopying>
// expected-remark@-1 {{Objective-C class interface 'NSValue' imported as class 'NSValue'}}
@end

@interface NSValue (NSRange)
// expected-remark@-1 {{Objective-C category 'NSValue(NSRange)' imported as extension of 'NSValue'}}
- (NSValue *)valueWithRange:(NSRange)range;
// expected-remark@-1 {{Objective-C method '-[NSValue(NSRange) valueWithRange:]' imported as instance method 'withRange'}}
@property NSRange rangeValue;
// expected-remark@-1 {{Objective-C method '-[NSValue(NSRange) rangeValue]' imported as getter for property 'rangeValue'}}
// expected-remark@-2 {{Objective-C method '-[NSValue(NSRange) setRangeValue:]' imported as setter for property 'rangeValue'}}
// expected-remark@-3 {{Objective-C property '-NSValue(NSRange).rangeValue' imported as property 'rangeValue'}}
@end

typedef __INT32_TYPE__ int32_t;
// expected-remark@-1 {{C typedef 'int32_t' imported as struct 'Int32'}}

@interface NSNumber : NSValue
// expected-remark@-1 {{Objective-C class interface 'NSNumber' imported as class 'NSNumber'}}
// FIXME: These are unavailable because of the initializers that follow; we should emit notes explaining this.
+ (nonnull NSNumber *)numberWithInt:(int)value;
// expected-remark@-1 {{Objective-C method '+[NSNumber numberWithInt:]' imported as unavailable initializer 'init(int:)'}}
+ (nonnull NSNumber *)numberWithInteger:(NSInteger)value;
// expected-remark@-1 {{Objective-C method '+[NSNumber numberWithInteger:]' imported as unavailable initializer 'init(integer:)'}}
+ (nonnull NSNumber *)numberWithUnsignedInteger:(NSUInteger)value;
// expected-remark@-1 {{Objective-C method '+[NSNumber numberWithUnsignedInteger:]' imported as unavailable initializer 'init(unsignedInteger:)'}}
+ (nonnull NSNumber *)numberWithDouble:(double)value;
// expected-remark@-1 {{Objective-C method '+[NSNumber numberWithDouble:]' imported as unavailable initializer 'init(double:)'}}

- (nonnull NSNumber *)initWithInteger:(NSInteger)value;
// expected-remark@-1 {{Objective-C method '-[NSNumber initWithInteger:]' imported as initializer 'init(value:)'}}
- (nonnull NSNumber *)initWithUnsignedInteger:(NSUInteger)value;
// expected-remark@-1 {{Objective-C method '-[NSNumber initWithUnsignedInteger:]' imported as initializer 'init(value:)'}}
- (nonnull NSNumber *)initWithDouble:(double)value;
// expected-remark@-1 {{Objective-C method '-[NSNumber initWithDouble:]' imported as initializer 'init(value:)'}}
- (nonnull NSNumber *)addDouble:(double)value;
// expected-remark@-1 {{Objective-C method '-[NSNumber addDouble:]' imported as instance method 'add'}}
- (nonnull NSNumber *)addBool:(BOOL)value;
// expected-remark@-1 {{Objective-C method '-[NSNumber addBool:]' imported as instance method 'add'}}

- (nonnull NSNumber *)addUInt16:(unsigned short)value;
// expected-remark@-1 {{Objective-C method '-[NSNumber addUInt16:]' imported as instance method 'add'}}
- (nonnull NSNumber *)addInt:(int)value;
// expected-remark@-1 {{Objective-C method '-[NSNumber addInt:]' imported as instance method 'add'}}
- (nonnull NSNumber *)subtractInt32:(int32_t)value;
// expected-remark@-1 {{Objective-C method '-[NSNumber subtractInt32:]' imported as instance method 'subtract'}}

@property NSInteger integerValue;
// expected-remark@-1 {{Objective-C method '-[NSNumber integerValue]' imported as getter for property 'intValue'}}
// expected-remark@-2 {{Objective-C method '-[NSNumber setIntegerValue:]' imported as setter for property 'intValue'}}
// expected-remark@-3 {{Objective-C property '-NSNumber.integerValue' imported as property 'intValue'}}
@property NSUInteger unsignedIntegerValue;
// expected-remark@-1 {{Objective-C method '-[NSNumber setUnsignedIntegerValue:]' imported as setter for property 'uintValue'}}
// expected-remark@-2 {{Objective-C method '-[NSNumber unsignedIntegerValue]' imported as getter for property 'uintValue'}}
// expected-remark@-3 {{Objective-C property '-NSNumber.unsignedIntegerValue' imported as property 'uintValue'}}
@end

@interface NSDecimalNumber : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSDecimalNumber' imported as class 'NSDecimalNumber'}}
+ (instancetype)initWithMantissa:(unsigned long long)mantissa exponent:(short)exponent isNegative:(BOOL)isNegative;
// expected-remark@-1 {{Objective-C method '+[NSDecimalNumber initWithMantissa:exponent:isNegative:]' imported as class method 'initWithMantissa(_:exponent:isNegative:)'}}
+ (NSDecimalNumber *)decimalNumberWithMantissa:(unsigned long long)mantissa exponent:(short)exponent isNegative:(BOOL)isNegative;
// expected-remark@-1 {{Objective-C method '+[NSDecimalNumber decimalNumberWithMantissa:exponent:isNegative:]' imported as initializer 'init(mantissa:exponent:isNegative:)'}}
@end

@interface NSError : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSError' imported as class 'NSError'}}
@property (readonly, nonnull) NSString *domain;
// expected-remark@-1 {{Objective-C method '-[NSError domain]' imported as getter for property 'domain'}}
// expected-remark@-2 {{Objective-C property '-NSError.domain' imported as property 'domain'}}
@property (readonly) NSInteger code;
// expected-remark@-1 {{Objective-C method '-[NSError code]' imported as getter for property 'code'}}
// expected-remark@-2 {{Objective-C property '-NSError.code' imported as property 'code'}}
- (nonnull instancetype)initWithDomain:(nonnull NSString *)domain code:(NSInteger)code userInfo:(nullable NSDictionary *)userInfo;
// expected-remark@-1 {{Objective-C method '-[NSError initWithDomain:code:userInfo:]' imported as initializer 'init(domain:code:userInfo:)'}}
@end

@interface NSString : NSObject <NSSecureCoding, NSCopying>
// expected-remark@-1 {{Objective-C class interface 'NSString' imported as class 'NSString'}}
- (void)onlyOnNSString;
// expected-remark@-1 {{Objective-C method '-[NSString onlyOnNSString]' imported as instance method 'onlyOnNSString()'}}
+ (instancetype)stringWithContentsOfFile:(NSString*)path error:(NSError**)error;
// expected-remark@-1 {{Objective-C method '+[NSString stringWithContentsOfFile:error:]' imported as initializer 'init(contentsOfFile:)'}}
+ (instancetype)stringWithContentsOfFile:(NSString*)path encoding:(int)encoding error:(NSError**)error;
// expected-remark@-1 {{Objective-C method '+[NSString stringWithContentsOfFile:encoding:error:]' imported as initializer 'init(contentsOfFile:encoding:)'}}

+ (instancetype)stringWithPath:(NSString*)path;
// expected-remark@-1 {{Objective-C method '+[NSString stringWithPath:]' imported as initializer 'init(path:)'}}
+ (nullable instancetype)stringWithPath:(NSString*)path encoding:(int)encoding;
// expected-remark@-1 {{Objective-C method '+[NSString stringWithPath:encoding:]' imported as initializer 'init(path:encoding:)'}}
@end

__attribute__((warn_unused_result)) NSString *NSStringToNSString(NSString *str);
// expected-remark@-1 {{C function 'NSStringToNSString' imported as global function 'NSStringToNSString'}}

@interface Bee : NSObject
// expected-remark@-1 {{Objective-C class interface 'Bee' imported as class 'Bee'}}
-(void)buzz;
// expected-remark@-1 {{Objective-C method '-[Bee buzz]' imported as instance method 'buzz()'}}
@end

@interface Hive : NSObject {
// expected-remark@-1 {{Objective-C class interface 'Hive' imported as class 'Hive'}}
  Bee *queen;
  // expected-remark@-1 {{could not import Objective-C instance variable 'Hive::queen'}}
  // expected-note@-2 {{use a property to access this instance variable}}
}
- init;
// expected-remark@-1 {{Objective-C method '-[Hive init]' imported as initializer 'init()'}}
- (instancetype)initWithCoder:(NSCoder *)aDecoder;
// expected-remark@-1 {{Objective-C method '-[Hive initWithCoder:]' imported as initializer 'init(coder:)'}}

@property (nonnull) NSArray<Bee *> *bees;
// expected-remark@-1 {{Objective-C method '-[Hive bees]' imported as getter for property 'bees'}}
// expected-remark@-2 {{Objective-C method '-[Hive setBees:]' imported as setter for property 'bees'}}
// expected-remark@-3 {{Objective-C property '-Hive.bees' imported as property 'bees'}}
@property (nullable) NSDictionary<NSString *, Bee *> *beesByName;
// expected-remark@-1 {{Objective-C method '-[Hive beesByName]' imported as getter for property 'beesByName'}}
// expected-remark@-2 {{Objective-C method '-[Hive setBeesByName:]' imported as setter for property 'beesByName'}}
// expected-remark@-3 {{Objective-C property '-Hive.beesByName' imported as property 'beesByName'}}
@property (nonnull) NSSet<Bee *> *allBees;
// expected-remark@-1 {{Objective-C method '-[Hive allBees]' imported as getter for property 'allBees'}}
// expected-remark@-2 {{Objective-C method '-[Hive setAllBees:]' imported as setter for property 'allBees'}}
// expected-remark@-3 {{Objective-C property '-Hive.allBees' imported as property 'allBees'}}
@property (nonnull) NSDictionary<id <NSCopying>, Bee *> *anythingToBees;
// expected-remark@-1 {{Objective-C method '-[Hive anythingToBees]' imported as getter for property 'anythingToBees'}}
// expected-remark@-2 {{Objective-C method '-[Hive setAnythingToBees:]' imported as setter for property 'anythingToBees'}}
// expected-remark@-3 {{Objective-C property '-Hive.anythingToBees' imported as property 'anythingToBees'}}

@property(getter=isMakingHoney) BOOL makingHoney;
// expected-remark@-1 {{Objective-C method '-[Hive isMakingHoney]' imported as getter for property 'isMakingHoney'}}
// expected-remark@-2 {{Objective-C method '-[Hive setMakingHoney:]' imported as setter for property 'isMakingHoney'}}
// expected-remark@-3 {{Objective-C property '-Hive.makingHoney' imported as property 'isMakingHoney'}}
@property(setter=assignGuard:) id guard;
// expected-remark@-1 {{Objective-C method '-[Hive assignGuard:]' imported as setter for property 'guard'}}
// expected-remark@-2 {{Objective-C method '-[Hive guard]' imported as getter for property 'guard'}}
// expected-remark@-3 {{Objective-C property '-Hive.guard' imported as property 'guard'}}

+ (instancetype)hiveWithQueen:(Bee *)queen;
// expected-remark@-1 {{Objective-C method '+[Hive hiveWithQueen:]' imported as initializer 'init(queen:)'}}
+ (instancetype)hiveWithFlakyQueen:(Bee *)queen error:(NSError **)error;
// expected-remark@-1 {{Objective-C method '+[Hive hiveWithFlakyQueen:error:]' imported as initializer 'init(flakyQueen:)'}}

- (instancetype)visit;
// expected-remark@-1 {{Objective-C method '-[Hive visit]' imported as instance method 'visit()'}}
@end

@interface NSMutableString : NSString
// expected-remark@-1 {{Objective-C class interface 'NSMutableString' imported as class 'NSMutableString'}}
@end

@interface NSURL : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSURL' imported as class 'NSURL'}}
- (instancetype)URLWithString:(NSString *)URLString;
// expected-remark@-1 {{Objective-C method '-[NSURL URLWithString:]' imported as instance method 'withString'}}
+ (instancetype)URLWithString:(NSString *)URLString;
// expected-remark@-1 {{Objective-C method '+[NSURL URLWithString:]' imported as initializer 'init(string:)'}}
- (BOOL)getResourceValue:(out id _Nullable *)value
                  forKey:(NSString *)key
                   error:(out NSError *_Nullable *)error;
// expected-remark@-3 {{Objective-C method '-[NSURL getResourceValue:forKey:error:]' imported as instance method 'getResourceValue(_:forKey:)'}}
@end

// An all-initials name like NSURL or NSUUID, but one that isn't bridged.
@interface NSGUID : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSGUID' imported as class 'NSGUID'}}
@end

@interface NSAttributedString : NSString
// expected-remark@-1 {{Objective-C class interface 'NSAttributedString' imported as class 'NSAttributedString'}}
- (NSAttributedString *)sliceAttributedString:(NSInteger)startIndex;
// expected-remark@-1 {{Objective-C method '-[NSAttributedString sliceAttributedString:]' imported as instance method 'sliceAttributedString'}}
@end

BOOL BOOLtoBOOL(BOOL b);
// expected-remark@-1 {{C function 'BOOLtoBOOL' imported as global function 'BOOLtoBOOL'}}

typedef CGPoint NSPoint;
// expected-remark@-1 {{C typedef 'NSPoint' imported as type alias 'NSPoint'}}
typedef CGSize NSSize;
// expected-remark@-1 {{C typedef 'NSSize' imported as type alias 'NSSize'}}
typedef CGRect NSRect;
// expected-remark@-1 {{C typedef 'NSRect' imported as type alias 'NSRect'}}
typedef NSPoint *NSPointArray;
// expected-remark@-1 {{C typedef 'NSPointArray' imported as type alias 'NSPointArray'}}

@interface BadCollection
// expected-remark@-1 {{Objective-C class interface 'BadCollection' imported as class 'BadCollection'}}
- (id)objectForKeyedSubscript:(id)key;
// expected-remark@-1 {{Objective-C method '-[BadCollection objectForKeyedSubscript:]' imported as unavailable instance method 'objectForKeyedSubscript'}}
// FIXME: Subscript?
- (void)setObject:(id)object forKeyedSubscript:(NSString *)key;
// expected-remark@-1 {{Objective-C method '-[BadCollection setObject:forKeyedSubscript:]' imported as instance method 'setObject(_:forKeyedSubscript:)'}}
// FIXME: Subscript?
@end

@interface BadCollectionParent
// expected-remark@-1 {{Objective-C class interface 'BadCollectionParent' imported as class 'BadCollectionParent'}}
- (id)objectForKeyedSubscript:(NSString *)key;
// expected-remark@-1 {{Objective-C method '-[BadCollectionParent objectForKeyedSubscript:]' imported as unavailable instance method 'objectForKeyedSubscript'}}
// FIXME: Subscript?
@end

@interface BadCollectionChild : BadCollectionParent
// expected-remark@-1 {{Objective-C class interface 'BadCollectionChild' imported as class 'BadCollectionChild'}}
- (void)setObject:(id)object forKeyedSubscript:(id)key;
// expected-remark@-1 {{Objective-C method '-[BadCollectionChild setObject:forKeyedSubscript:]' imported as instance method 'setObject(_:forKeyedSubscript:)'}}
@end

@interface ReadOnlyCollectionChild : BadCollectionParent
// expected-remark@-1 {{Objective-C class interface 'ReadOnlyCollectionChild' imported as class 'ReadOnlyCollectionChild'}}
- (void)setObject:(id)object forKeyedSubscript:(id)key;
// expected-remark@-1 {{Objective-C method '-[ReadOnlyCollectionChild setObject:forKeyedSubscript:]' imported as instance method 'setObject(_:forKeyedSubscript:)'}}
@end

//===---
// Enums.
//===---

#define NS_ENUM(_type, _name) CF_ENUM(_type, _name)
#define NS_OPTIONS(_type, _name) CF_OPTIONS(_type, _name)

/// Aaa.  NSRuncingMode.  Bbb.
typedef NS_ENUM(NSUInteger, NSRuncingMode) {
  // expected-remark@-1 {{C enum 'NSRuncingMode' imported as enum 'NSRuncingMode'}}
  // expected-remark@-2 {{C typedef 'NSRuncingMode' imported as enum 'NSRuncingMode'}}
  NSRuncingMince,
  // expected-remark@-1 {{C enum constant 'NSRuncingMince' imported as enum case 'mince'}}
  NSRuncingQuince
  // expected-remark@-1 {{C enum constant 'NSRuncingQuince' imported as enum case 'quince'}}
};

typedef NS_ENUM(int, NSUnderlyingType) {
  // expected-remark@-1 {{C enum 'NSUnderlyingType' imported as enum 'NSUnderlyingType'}}
  // expected-remark@-2 {{C typedef 'NSUnderlyingType' imported as enum 'NSUnderlyingType'}}
  NSUnderlyingTypeZim,
  // expected-remark@-1 {{C enum constant 'NSUnderlyingTypeZim' imported as enum case 'zim'}}
  NSUnderlyingTypeZang,
  // expected-remark@-1 {{C enum constant 'NSUnderlyingTypeZang' imported as enum case 'zang'}}
  NSUnderlyingTypeFoo = 11,
  // expected-remark@-1 {{C enum constant 'NSUnderlyingTypeFoo' imported as enum case 'foo'}}
  NSUnderlyingTypeBar = 22,
  // expected-remark@-1 {{C enum constant 'NSUnderlyingTypeBar' imported as enum case 'bar'}}
  NSUnderlyingTypeBas
  // expected-remark@-1 {{C enum constant 'NSUnderlyingTypeBas' imported as enum case 'bas'}}
};

typedef NS_ENUM(unsigned, NSUnsignedUnderlyingTypeNegativeValue) {
  // expected-remark@-1 {{C enum 'NSUnsignedUnderlyingTypeNegativeValue' imported as enum 'NSUnsignedUnderlyingTypeNegativeValue'}}
  // expected-remark@-2 {{C typedef 'NSUnsignedUnderlyingTypeNegativeValue' imported as enum 'NSUnsignedUnderlyingTypeNegativeValue'}}
  NSNegativeOne = -1,
  // expected-remark@-1 {{C enum constant 'NSNegativeOne' imported as enum case 'negativeOne'}}
  NSNegativeTwo = -2,
  // expected-remark@-1 {{C enum constant 'NSNegativeTwo' imported as enum case 'negativeTwo'}}
};

typedef NS_ENUM(NSInteger, NSPrefixWordBreak) {
  // expected-remark@-1 {{C enum 'NSPrefixWordBreak' imported as enum 'NSPrefixWordBreak'}}
  // expected-remark@-2 {{C typedef 'NSPrefixWordBreak' imported as enum 'NSPrefixWordBreak'}}
  NSPrefixWordBreakBanjo,
  // expected-remark@-1 {{C enum constant 'NSPrefixWordBreakBanjo' imported as enum case 'banjo'}}
  NSPrefixWordBreakBandana
  // expected-remark@-1 {{C enum constant 'NSPrefixWordBreakBandana' imported as enum case 'bandana'}}
};

typedef NS_ENUM(NSInteger, NSPrefixWordBreak2) {
  // expected-remark@-1 {{C enum 'NSPrefixWordBreak2' imported as enum 'NSPrefixWordBreak2'}}
  // expected-remark@-2 {{C typedef 'NSPrefixWordBreak2' imported as enum 'NSPrefixWordBreak2'}}
  NSPrefixWordBreakBarBas,
  // expected-remark@-1 {{C enum constant 'NSPrefixWordBreakBarBas' imported as enum case 'breakBarBas'}}
  NSPrefixWordBreakBareBass,
  // expected-remark@-1 {{C enum constant 'NSPrefixWordBreakBareBass' imported as enum case 'breakBareBass'}}
};

typedef NS_ENUM(NSInteger, NSPrefixWordBreak3) {
  // expected-remark@-1 {{C enum 'NSPrefixWordBreak3' imported as enum 'NSPrefixWordBreak3'}}
  // expected-remark@-2 {{C typedef 'NSPrefixWordBreak3' imported as enum 'NSPrefixWordBreak3'}}
  NSPrefixWordBreak1Bob,
  // expected-remark@-1 {{C enum constant 'NSPrefixWordBreak1Bob' imported as enum case 'break1Bob'}}
  NSPrefixWordBreak1Ben,
  // expected-remark@-1 {{C enum constant 'NSPrefixWordBreak1Ben' imported as enum case 'break1Ben'}}
};

typedef NS_ENUM(NSInteger, NSPrefixWordBreakCustom) {
  // expected-remark@-1 {{C enum 'NSPrefixWordBreakCustom' imported as enum 'NSPrefixWordBreakCustom'}}
  // expected-remark@-2 {{C typedef 'NSPrefixWordBreakCustom' imported as enum 'NSPrefixWordBreakCustom'}}
  PrefixWordBreakProblemCase __attribute__((swift_name("problemCase"))),
  // expected-remark@-1 {{C enum constant 'PrefixWordBreakProblemCase' imported as enum case 'problemCase'}}
  NSPrefixWordBreakDeprecatedGoodCase __attribute__((deprecated)),
  // expected-remark@-1 {{C enum constant 'NSPrefixWordBreakDeprecatedGoodCase' imported as enum case 'deprecatedGoodCase'}}
};

typedef NS_ENUM(NSInteger, NSPrefixWordBreak2Custom) {
  // expected-remark@-1 {{C enum 'NSPrefixWordBreak2Custom' imported as enum 'NSPrefixWordBreak2Custom'}}
  // expected-remark@-2 {{C typedef 'NSPrefixWordBreak2Custom' imported as enum 'NSPrefixWordBreak2Custom'}}
  PrefixWordBreak2ProblemCase __attribute__((swift_name("problemCase"))),
  // expected-remark@-1 {{C enum constant 'PrefixWordBreak2ProblemCase' imported as enum case 'problemCase'}}
  PrefixWordBreak2DeprecatedBadCase __attribute__((deprecated)),
  // expected-remark@-1 {{C enum constant 'PrefixWordBreak2DeprecatedBadCase' imported as enum case 'PrefixWordBreak2DeprecatedBadCase'}}
  NSPrefixWordBreak2DeprecatedGoodCase __attribute__((deprecated)),
  // expected-remark@-1 {{C enum constant 'NSPrefixWordBreak2DeprecatedGoodCase' imported as enum case 'deprecatedGoodCase'}}
  NSPrefixWordBreak2GoodCase,
  // expected-remark@-1 {{C enum constant 'NSPrefixWordBreak2GoodCase' imported as enum case 'goodCase'}}
};

typedef NS_ENUM(NSInteger, NSPrefixWordBreakReversedCustom) {
  // expected-remark@-1 {{C enum 'NSPrefixWordBreakReversedCustom' imported as enum 'NSPrefixWordBreakReversedCustom'}}
  // expected-remark@-2 {{C typedef 'NSPrefixWordBreakReversedCustom' imported as enum 'NSPrefixWordBreakReversedCustom'}}
  NSPrefixWordBreakReversedDeprecatedGoodCase __attribute__((deprecated)),
  // expected-remark@-1 {{C enum constant 'NSPrefixWordBreakReversedDeprecatedGoodCase' imported as enum case 'deprecatedGoodCase'}}
  PrefixWordBreakReversedProblemCase __attribute__((swift_name("problemCase"))),
  // expected-remark@-1 {{C enum constant 'PrefixWordBreakReversedProblemCase' imported as enum case 'problemCase'}}
};

typedef NS_ENUM(NSInteger, NSPrefixWordBreakReorderedCustom) {
  // expected-remark@-1 {{C enum 'NSPrefixWordBreakReorderedCustom' imported as enum 'NSPrefixWordBreakReorderedCustom'}}
  // expected-remark@-2 {{C typedef 'NSPrefixWordBreakReorderedCustom' imported as enum 'NSPrefixWordBreakReorderedCustom'}}
  PrefixWordBreakReorderedProblemCase __attribute__((swift_name("problemCase"))),
  // expected-remark@-1 {{C enum constant 'PrefixWordBreakReorderedProblemCase' imported as enum case 'problemCase'}}
  NSPrefixWordBreakReorderedGoodCase,
  // expected-remark@-1 {{C enum constant 'NSPrefixWordBreakReorderedGoodCase' imported as enum case 'goodCase'}}
  PrefixWordBreakReorderedDeprecatedBadCase __attribute__((deprecated)),
  // expected-remark@-1 {{C enum constant 'PrefixWordBreakReorderedDeprecatedBadCase' imported as enum case 'PrefixWordBreakReorderedDeprecatedBadCase'}}
  NSPrefixWordBreakReorderedDeprecatedGoodCase __attribute__((deprecated)),
  // expected-remark@-1 {{C enum constant 'NSPrefixWordBreakReorderedDeprecatedGoodCase' imported as enum case 'deprecatedGoodCase'}}
};

typedef NS_ENUM(NSInteger, NSPrefixWordBreakReordered2Custom) {
  // expected-remark@-1 {{C enum 'NSPrefixWordBreakReordered2Custom' imported as enum 'NSPrefixWordBreakReordered2Custom'}}
  // expected-remark@-2 {{C typedef 'NSPrefixWordBreakReordered2Custom' imported as enum 'NSPrefixWordBreakReordered2Custom'}}
  PrefixWordBreakReordered2DeprecatedBadCase __attribute__((deprecated)),
  // expected-remark@-1 {{C enum constant 'PrefixWordBreakReordered2DeprecatedBadCase' imported as enum case 'PrefixWordBreakReordered2DeprecatedBadCase'}}
  PrefixWordBreakReordered2ProblemCase __attribute__((swift_name("problemCase"))),
  // expected-remark@-1 {{C enum constant 'PrefixWordBreakReordered2ProblemCase' imported as enum case 'problemCase'}}
  NSPrefixWordBreakReordered2GoodCase,
  // expected-remark@-1 {{C enum constant 'NSPrefixWordBreakReordered2GoodCase' imported as enum case 'goodCase'}}
  NSPrefixWordBreakReordered2DeprecatedGoodCase __attribute__((deprecated)),
  // expected-remark@-1 {{C enum constant 'NSPrefixWordBreakReordered2DeprecatedGoodCase' imported as enum case 'deprecatedGoodCase'}}
};

typedef NS_ENUM(NSInteger, NSSwiftNameAllTheThings) {
  // expected-remark@-1 {{C enum 'NSSwiftNameAllTheThings' imported as enum 'NSSwiftNameAllTheThings'}}
  // expected-remark@-2 {{C typedef 'NSSwiftNameAllTheThings' imported as enum 'NSSwiftNameAllTheThings'}}
  NSSwiftNameAllTheThingsA __attribute__((swift_name("Foo"))),
  // expected-remark@-1 {{C enum constant 'NSSwiftNameAllTheThingsA' imported as enum case 'Foo'}}
  NSSwiftNameAllTheThingsB __attribute__((swift_name("Bar"))),
  // expected-remark@-1 {{C enum constant 'NSSwiftNameAllTheThingsB' imported as enum case 'Bar'}}
};

typedef NS_ENUM(NSInteger, NSSwiftNameBad) {
  // expected-remark@-1 {{C enum 'NSSwiftNameBad' imported as enum 'NSSwiftNameBad'}}
  // expected-remark@-2 {{C typedef 'NSSwiftNameBad' imported as enum 'NSSwiftNameBad'}}
  NSSwiftNameBadA __attribute__((swift_name("class"))),
  // expected-remark@-1 {{C enum constant 'NSSwiftNameBadA' imported as enum case 'class'}}
};


typedef NS_ENUM(NSInteger, NSSingleConstantEnum) {
  // expected-remark@-1 {{C enum 'NSSingleConstantEnum' imported as enum 'NSSingleConstantEnum'}}
  // expected-remark@-2 {{C typedef 'NSSingleConstantEnum' imported as enum 'NSSingleConstantEnum'}}
  NSSingleConstantValue,
  // expected-remark@-1 {{C enum constant 'NSSingleConstantValue' imported as enum case 'value'}}
};

typedef NS_ENUM(unsigned char, NSAliasesEnum) {
  // expected-remark@-1 {{C enum 'NSAliasesEnum' imported as enum 'NSAliasesEnum'}}
  // expected-remark@-2 {{C typedef 'NSAliasesEnum' imported as enum 'NSAliasesEnum'}}
  NSAliasesOriginal = 129,
  // expected-remark@-1 {{C enum constant 'NSAliasesOriginal' imported as enum case 'original'}}
  NSAliasesBySameValue = 129,
  // expected-remark@-1 {{C enum constant 'NSAliasesBySameValue' imported as static property 'bySameValue'}}
  NSAliasesByEquivalentValue = -127,
  // expected-remark@-1 {{C enum constant 'NSAliasesByEquivalentValue' imported as static property 'byEquivalentValue'}}
  NSAliasesByName = NSAliasesOriginal,
  // expected-remark@-1 {{C enum constant 'NSAliasesByName' imported as static property 'byName'}}
  NSAliasesDifferentValue = 2
  // expected-remark@-1 {{C enum constant 'NSAliasesDifferentValue' imported as enum case 'differentValue'}}
};

typedef NS_ENUM(unsigned char, NSUnavailableAliasesEnum) {
  // expected-remark@-1 {{C enum 'NSUnavailableAliasesEnum' imported as enum 'NSUnavailableAliasesEnum'}}
  // expected-remark@-2 {{C typedef 'NSUnavailableAliasesEnum' imported as enum 'NSUnavailableAliasesEnum'}}
  NSUnavailableAliasesOriginalAU = 0,
  // expected-remark@-1 {{C enum constant 'NSUnavailableAliasesOriginalAU' imported as enum case 'originalAU'}}
  NSUnavailableAliasesAliasAU __attribute__((unavailable)) = 0,
  // expected-remark@-1 {{C enum constant 'NSUnavailableAliasesAliasAU' imported as unavailable static property 'aliasAU'}}
  NSUnavailableAliasesOriginalUA __attribute__((unavailable)) = 1,
  // expected-remark@-1 {{C enum constant 'NSUnavailableAliasesOriginalUA' imported as unavailable static property 'originalUA'}}
  NSUnavailableAliasesAliasUA = 1,
  // expected-remark@-1 {{C enum constant 'NSUnavailableAliasesAliasUA' imported as enum case 'aliasUA'}}
  NSUnavailableAliasesOriginalUU __attribute__((unavailable)) = 2,
  // expected-remark@-1 {{C enum constant 'NSUnavailableAliasesOriginalUU' imported as unavailable enum case 'originalUU'}}
  NSUnavailableAliasesAliasUU __attribute__((unavailable)) = 2,
  // expected-remark@-1 {{C enum constant 'NSUnavailableAliasesAliasUU' imported as unavailable enum case 'aliasUU'}}
};

NS_ENUM(NSInteger, NSMalformedEnumMissingTypedef) {
  // expected-remark@-1 {{C enum 'NSMalformedEnumMissingTypedef' imported as enum 'NSMalformedEnumMissingTypedef'}}
  // expected-remark@-2 {{C variable 'NSMalformedEnumMissingTypedef' imported as var 'NSMalformedEnumMissingTypedef'}}
  NSMalformedEnumMissingTypedefValue
  // expected-remark@-1 {{C enum constant 'NSMalformedEnumMissingTypedefValue' imported as enum case 'value'}}
};

@interface NSNumberFormatter : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSNumberFormatter' imported as class 'NumberFormatter'}}
@end

typedef NS_ENUM(NSUInteger, NSNumberFormatterBehavior) {
  // expected-remark@-1 {{C enum 'NSNumberFormatterBehavior' imported as enum 'Behavior'}}
  // expected-remark@-2 {{C typedef 'NSNumberFormatterBehavior' imported as enum 'Behavior'}}
  NSNumberFormatterBehaviorDefault = 0,
  // expected-remark@-1 {{C enum constant 'NSNumberFormatterBehaviorDefault' imported as enum case 'default'}}
  NSNumberFormatterBehavior10_0 = 1000,
  // expected-remark@-1 {{C enum constant 'NSNumberFormatterBehavior10_0' imported as enum case 'behavior10_0'}}
  NSNumberFormatterBehavior10_4 = 1040,
  // expected-remark@-1 {{C enum constant 'NSNumberFormatterBehavior10_4' imported as enum case 'behavior10_4'}}
};

@interface NSNotification : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSNotification' imported as class 'NSNotification'}}
@end

@interface NSNotificationQueue : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSNotificationQueue' imported as class 'NotificationQueue'}}
@end

typedef NS_ENUM(NSUInteger, NSPostingStyle) {
  // expected-remark@-1 {{C enum 'NSPostingStyle' imported as enum 'PostingStyle'}}
  // expected-remark@-2 {{C typedef 'NSPostingStyle' imported as enum 'PostingStyle'}}
  NSPostWhenIdle = 1,
  // expected-remark@-1 {{C enum constant 'NSPostWhenIdle' imported as enum case 'whenIdle'}}
  NSPostASAP = 2,
  // expected-remark@-1 {{C enum constant 'NSPostASAP' imported as enum case 'asap'}}
  NSPostNow = 3
  // expected-remark@-1 {{C enum constant 'NSPostNow' imported as enum case 'now'}}
};

@interface NSXMLNode : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSXMLNode' imported as class 'XMLNode'}}
@end

typedef NS_ENUM(NSUInteger, NSXMLNodeKind) {
  // expected-remark@-1 {{C enum 'NSXMLNodeKind' imported as enum 'Kind'}}
  // expected-remark@-2 {{C typedef 'NSXMLNodeKind' imported as enum 'Kind'}}
  NSXMLInvalidKind = 0,
  // expected-remark@-1 {{C enum constant 'NSXMLInvalidKind' imported as enum case 'invalid'}}
  NSXMLDocumentKind,
  // expected-remark@-1 {{C enum constant 'NSXMLDocumentKind' imported as enum case 'document'}}
  NSXMLElementKind,
  // expected-remark@-1 {{C enum constant 'NSXMLElementKind' imported as enum case 'element'}}
  NSXMLAttributeKind,
  // expected-remark@-1 {{C enum constant 'NSXMLAttributeKind' imported as enum case 'attribute'}}
  NSXMLNamespaceKind,
  // expected-remark@-1 {{C enum constant 'NSXMLNamespaceKind' imported as enum case 'namespace'}}
  NSXMLProcessingInstructionKind,
  // expected-remark@-1 {{C enum constant 'NSXMLProcessingInstructionKind' imported as enum case 'processingInstruction'}}
  NSXMLCommentKind,
  // expected-remark@-1 {{C enum constant 'NSXMLCommentKind' imported as enum case 'comment'}}
  NSXMLTextKind,
  // expected-remark@-1 {{C enum constant 'NSXMLTextKind' imported as enum case 'text'}}
  NSXMLDTDKind __attribute__((swift_name("DTDKind"))),
  // expected-remark@-1 {{C enum constant 'NSXMLDTDKind' imported as enum case 'DTDKind'}}
  NSXMLEntityDeclarationKind,
  // expected-remark@-1 {{C enum constant 'NSXMLEntityDeclarationKind' imported as enum case 'entityDeclaration'}}
  NSXMLAttributeDeclarationKind,
  // expected-remark@-1 {{C enum constant 'NSXMLAttributeDeclarationKind' imported as enum case 'attributeDeclaration'}}
  NSXMLElementDeclarationKind,
  // expected-remark@-1 {{C enum constant 'NSXMLElementDeclarationKind' imported as enum case 'elementDeclaration'}}
  NSXMLNotationDeclarationKind
  // expected-remark@-1 {{C enum constant 'NSXMLNotationDeclarationKind' imported as enum case 'notationDeclaration'}}
};

// From CoreFoundation
typedef CF_ENUM(NSInteger, CFURLPathStyle) {
  // expected-remark@-1 {{C enum 'CFURLPathStyle' imported as enum 'CFURLPathStyle'}}
  // expected-remark@-2 {{C typedef 'CFURLPathStyle' imported as enum 'CFURLPathStyle'}}
  kCFURLPOSIXPathStyle = 0,
  // expected-remark@-1 {{C enum constant 'kCFURLPOSIXPathStyle' imported as enum case 'cfurlposixPathStyle'}}
  kCFURLHFSPathStyle /*CF_ENUM_DEPRECATED(10_0, 10_9, 2_0, 7_0)*/,
  // expected-remark@-1 {{C enum constant 'kCFURLHFSPathStyle' imported as enum case 'cfurlhfsPathStyle'}}
  kCFURLWindowsPathStyle
  // expected-remark@-1 {{C enum constant 'kCFURLWindowsPathStyle' imported as enum case 'cfurlWindowsPathStyle'}}
};

typedef CF_ENUM(NSInteger, CFURLOrUTI) {
  // expected-remark@-1 {{C enum 'CFURLOrUTI' imported as enum 'CFURLOrUTI'}}
  // expected-remark@-2 {{C typedef 'CFURLOrUTI' imported as enum 'CFURLOrUTI'}}
  kCFURLKind,
  // expected-remark@-1 {{C enum constant 'kCFURLKind' imported as enum case 'cfurlKind'}}
  kCFUTIKind
  // expected-remark@-1 {{C enum constant 'kCFUTIKind' imported as enum case 'cfutiKind'}}
};

typedef CF_ENUM(NSInteger, Magnitude) {
  // expected-remark@-1 {{C enum 'Magnitude' imported as enum 'Magnitude'}}
  // expected-remark@-2 {{C typedef 'Magnitude' imported as enum 'Magnitude'}}
  k0,
  // expected-remark@-1 {{C enum constant 'k0' imported as enum case 'k0'}}
  k1,
  // expected-remark@-1 {{C enum constant 'k1' imported as enum case 'k1'}}
  k2,
  // expected-remark@-1 {{C enum constant 'k2' imported as enum case 'k2'}}
};

typedef CF_ENUM(NSInteger, MagnitudeWords) {
  // expected-remark@-1 {{C enum 'MagnitudeWords' imported as enum 'MagnitudeWords'}}
  // expected-remark@-2 {{C typedef 'MagnitudeWords' imported as enum 'MagnitudeWords'}}
  kZero,
  // expected-remark@-1 {{C enum constant 'kZero' imported as enum case 'zero'}}
  kOne,
  // expected-remark@-1 {{C enum constant 'kOne' imported as enum case 'one'}}
  kTwo,
  // expected-remark@-1 {{C enum constant 'kTwo' imported as enum case 'two'}}
};


// Deliberately simple to test the overlay module.
enum {
  // FIXME: Indicate that the anonymous enum isn't imported?
  NSUTF8StringEncoding = 8
  // expected-remark@-1 {{C enum constant 'NSUTF8StringEncoding' imported as var 'NSUTF8StringEncoding'}}
};


/// Aaa.  NSRuncingOptions.  Bbb.
typedef NS_OPTIONS(NSUInteger, NSRuncingOptions) {
  // expected-remark@-1 {{C enum 'NSRuncingOptions' imported as struct 'NSRuncingOptions'}}
  // expected-remark@-2 {{C typedef 'NSRuncingOptions' imported as struct 'NSRuncingOptions'}}
  NSRuncingNone = 0,
  // expected-remark@-1 {{C enum constant 'NSRuncingNone' imported as unavailable static property 'none'}}
  NSRuncingEnableMince = 1,
  // expected-remark@-1 {{C enum constant 'NSRuncingEnableMince' imported as static property 'enableMince'}}
  NSRuncingEnableQuince = 2,
  // expected-remark@-1 {{C enum constant 'NSRuncingEnableQuince' imported as static property 'enableQuince'}}
};

typedef NS_OPTIONS(NSUInteger, NSSingleOptions) {
  // expected-remark@-1 {{C enum 'NSSingleOptions' imported as struct 'NSSingleOptions'}}
  // expected-remark@-2 {{C typedef 'NSSingleOptions' imported as struct 'NSSingleOptions'}}
  NSSingleValue = 1,
  // expected-remark@-1 {{C enum constant 'NSSingleValue' imported as static property 'value'}}
};

// From CoreFoundation
typedef CF_OPTIONS(unsigned long, CFCalendarUnit) {
  // expected-remark@-1 {{C enum 'CFCalendarUnit' imported as struct 'CFCalendarUnit'}}
  // expected-remark@-2 {{C typedef 'CFCalendarUnit' imported as struct 'CFCalendarUnit'}}
  kCFCalendarUnitEra = (1UL << 1),
  // expected-remark@-1 {{C enum constant 'kCFCalendarUnitEra' imported as static property 'era'}}
  kCFCalendarUnitYear = (1UL << 2),
  // expected-remark@-1 {{C enum constant 'kCFCalendarUnitYear' imported as static property 'year'}}
  kCFCalendarUnitMonth = (1UL << 3),
  // expected-remark@-1 {{C enum constant 'kCFCalendarUnitMonth' imported as static property 'month'}}
  kCFCalendarUnitDay = (1UL << 4),
  // expected-remark@-1 {{C enum constant 'kCFCalendarUnitDay' imported as static property 'day'}}
  kCFCalendarUnitHour = (1UL << 5),
  // expected-remark@-1 {{C enum constant 'kCFCalendarUnitHour' imported as static property 'hour'}}
  kCFCalendarUnitMinute = (1UL << 6),
  // expected-remark@-1 {{C enum constant 'kCFCalendarUnitMinute' imported as static property 'minute'}}
  kCFCalendarUnitSecond = (1UL << 7),
  // expected-remark@-1 {{C enum constant 'kCFCalendarUnitSecond' imported as static property 'second'}}
  kCFCalendarUnitWeek /*CF_ENUM_DEPRECATED(10_4, 10_51, 2_0, 8_0)*/ = (1UL << 8),
  // expected-remark@-1 {{C enum constant 'kCFCalendarUnitWeek' imported as static property 'week'}}
  kCFCalendarUnitWeekday = (1UL << 9),
  // expected-remark@-1 {{C enum constant 'kCFCalendarUnitWeekday' imported as static property 'weekday'}}
  kCFCalendarUnitWeekdayOrdinal = (1UL << 10),
  // expected-remark@-1 {{C enum constant 'kCFCalendarUnitWeekdayOrdinal' imported as static property 'weekdayOrdinal'}}
  kCFCalendarUnitQuarter /*CF_ENUM_AVAILABLE(10_6, 4_0)*/ = (1UL << 11),
  // expected-remark@-1 {{C enum constant 'kCFCalendarUnitQuarter' imported as static property 'quarter'}}
  kCFCalendarUnitWeekOfMonth /*CF_ENUM_AVAILABLE(10_7, 5_0)*/ = (1UL << 12),
  // expected-remark@-1 {{C enum constant 'kCFCalendarUnitWeekOfMonth' imported as static property 'weekOfMonth'}}
  kCFCalendarUnitWeekOfYear /*CF_ENUM_AVAILABLE(10_7, 5_0)*/ = (1UL << 13),
  // expected-remark@-1 {{C enum constant 'kCFCalendarUnitWeekOfYear' imported as static property 'weekOfYear'}}
  kCFCalendarUnitYearForWeekOfYear /*CF_ENUM_AVAILABLE(10_7, 5_0)*/ = (1UL << 14),
  // expected-remark@-1 {{C enum constant 'kCFCalendarUnitYearForWeekOfYear' imported as static property 'yearForWeekOfYear'}}
};

// From Foundation

typedef NS_OPTIONS(NSUInteger, NSKeyValueObservingOptions) {
  // expected-remark@-1 {{C enum 'NSKeyValueObservingOptions' imported as struct 'NSKeyValueObservingOptions'}}
  // expected-remark@-2 {{C typedef 'NSKeyValueObservingOptions' imported as struct 'NSKeyValueObservingOptions'}}
  NSKeyValueObservingOptionNew = 0x01,
  // expected-remark@-1 {{C enum constant 'NSKeyValueObservingOptionNew' imported as static property 'new'}}
  NSKeyValueObservingOptionOld = 0x02,
  // expected-remark@-1 {{C enum constant 'NSKeyValueObservingOptionOld' imported as static property 'old'}}
  NSKeyValueObservingOptionInitial /*NS_ENUM_AVAILABLE(10_5, 2_0)*/ = 0x04,
  // expected-remark@-1 {{C enum constant 'NSKeyValueObservingOptionInitial' imported as static property 'initial'}}
  NSKeyValueObservingOptionPrior /*NS_ENUM_AVAILABLE(10_5, 2_0)*/ = 0x08
  // expected-remark@-1 {{C enum constant 'NSKeyValueObservingOptionPrior' imported as static property 'prior'}}
};

@interface NSCalendar : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSCalendar' imported as class 'NSCalendar'}}
@end

#define NS_CALENDAR_ENUM_DEPRECATED(osx_in, osx_out, ios_in, ios_out, msg) \
  __attribute__((availability(macosx, introduced=osx_in, deprecated=osx_out, message=msg))) \
  __attribute__((availability(iphoneos, introduced=ios_in, deprecated=ios_out, message=msg)))
typedef NS_OPTIONS(NSUInteger, NSCalendarUnit) {
  // expected-remark@-1 {{C enum 'NSCalendarUnit' imported as struct 'Unit'}}
  // expected-remark@-2 {{C typedef 'NSCalendarUnit' imported as struct 'Unit'}}
  NSCalendarUnitEra                = kCFCalendarUnitEra,
  // expected-remark@-1 {{C enum constant 'NSCalendarUnitEra' imported as static property 'era'}}
  NSCalendarUnitYear               = kCFCalendarUnitYear,
  // expected-remark@-1 {{C enum constant 'NSCalendarUnitYear' imported as static property 'year'}}
  NSCalendarUnitMonth              = kCFCalendarUnitMonth,
  // expected-remark@-1 {{C enum constant 'NSCalendarUnitMonth' imported as static property 'month'}}

  // snip

  NSCalendarUnitCalendar           = (1 << 20),
  // expected-remark@-1 {{C enum constant 'NSCalendarUnitCalendar' imported as static property 'calendar'}}

  NSEraCalendarUnit NS_CALENDAR_ENUM_DEPRECATED(10_4, 10_9, 2_0, 7_0, "Use NSCalendarUnitEra instead") = NSCalendarUnitEra,
  // expected-remark@-1 {{C enum constant 'NSEraCalendarUnit' imported as unavailable static property 'NSEraCalendarUnit'}}
  NSYearCalendarUnit NS_CALENDAR_ENUM_DEPRECATED(10_4, 10_9, 2_0, 7_0, "Use NSCalendarUnitYear instead") = NSCalendarUnitYear,
  // expected-remark@-1 {{C enum constant 'NSYearCalendarUnit' imported as unavailable static property 'NSYearCalendarUnit'}}
  NSMonthCalendarUnit NS_CALENDAR_ENUM_DEPRECATED(10_4, 10_9, 2_0, 7_0, "Use NSCalendarUnitMonth instead") = NSCalendarUnitMonth,
  // expected-remark@-1 {{C enum constant 'NSMonthCalendarUnit' imported as unavailable static property 'NSMonthCalendarUnit'}}

  // snip

  NSCalendarCalendarUnit NS_CALENDAR_ENUM_DEPRECATED(10_7, 10_9, 4_0, 7_0, "Use NSCalendarUnitCalendar instead") = NSCalendarUnitCalendar,
  // expected-remark@-1 {{C enum constant 'NSCalendarCalendarUnit' imported as unavailable static property 'NSCalendarCalendarUnit'}}
};

typedef NS_OPTIONS(NSUInteger, NSCalendarUnitDeprecated) {
  // expected-remark@-1 {{C enum 'NSCalendarUnitDeprecated' imported as struct 'NSCalendarUnitDeprecated'}}
  // expected-remark@-2 {{C typedef 'NSCalendarUnitDeprecated' imported as struct 'NSCalendarUnitDeprecated'}}
  NSEraCalendarUnitDeprecated NS_CALENDAR_ENUM_DEPRECATED(10_4, 10_9, 2_0, 7_0, "Use NSCalendarUnitEra instead") = NSCalendarUnitEra,
  // expected-remark@-1 {{C enum constant 'NSEraCalendarUnitDeprecated' imported as unavailable static property 'eraCalendarUnitDeprecated'}}
  NSYearCalendarUnitDeprecated NS_CALENDAR_ENUM_DEPRECATED(10_4, 10_9, 2_0, 7_0, "Use NSCalendarUnitYear instead") = NSCalendarUnitYear,
  // expected-remark@-1 {{C enum constant 'NSYearCalendarUnitDeprecated' imported as unavailable static property 'yearCalendarUnitDeprecated'}}
  NSMonthCalendarUnitDeprecated NS_CALENDAR_ENUM_DEPRECATED(10_4, 10_9, 2_0, 7_0, "Use NSCalendarUnitMonth instead") = NSCalendarUnitMonth,
  // expected-remark@-1 {{C enum constant 'NSMonthCalendarUnitDeprecated' imported as unavailable static property 'monthCalendarUnitDeprecated'}}

  // snip

  NSCalendarCalendarUnitDeprecated NS_CALENDAR_ENUM_DEPRECATED(10_7, 10_9, 4_0, 7_0, "Use NSCalendarUnitCalendar instead") = NSCalendarUnitCalendar,
  // expected-remark@-1 {{C enum constant 'NSCalendarCalendarUnitDeprecated' imported as unavailable static property 'calendarCalendarUnitDeprecated'}}
};

typedef NS_OPTIONS(NSUInteger, NSOptionsAlsoGetSwiftName) {
  // expected-remark@-1 {{C enum 'NSOptionsAlsoGetSwiftName' imported as struct 'NSOptionsAlsoGetSwiftName'}}
  // expected-remark@-2 {{C typedef 'NSOptionsAlsoGetSwiftName' imported as struct 'NSOptionsAlsoGetSwiftName'}}
  ThisIsAnNSOptionsCaseWithSwiftName __attribute__((swift_name("Case"))) = 0x1
  // expected-remark@-1 {{C enum constant 'ThisIsAnNSOptionsCaseWithSwiftName' imported as static property 'Case'}}
};

#define CF_SWIFT_UNAVAILABLE(_msg) __attribute__((availability(swift, unavailable, message=_msg)))
#define NS_SWIFT_UNAVAILABLE(_msg) CF_SWIFT_UNAVAILABLE(_msg)

typedef NS_ENUM(NSUInteger, NSRectEdge) {
  // expected-remark@-1 {{C enum 'NSRectEdge' imported as enum 'NSRectEdge'}}
  // expected-remark@-2 {{C typedef 'NSRectEdge' imported as enum 'NSRectEdge'}}
  NSRectEdgeMinX = 0,
  // expected-remark@-1 {{C enum constant 'NSRectEdgeMinX' imported as enum case 'minX'}}
  NSRectEdgeMinY = 1,
  // expected-remark@-1 {{C enum constant 'NSRectEdgeMinY' imported as enum case 'minY'}}
  NSRectEdgeMaxX = 2,
  // expected-remark@-1 {{C enum constant 'NSRectEdgeMaxX' imported as enum case 'maxX'}}
  NSRectEdgeMaxY = 3,
  // expected-remark@-1 {{C enum constant 'NSRectEdgeMaxY' imported as enum case 'maxY'}}

  NSMinXEdge NS_SWIFT_UNAVAILABLE("Use NSRectEdge.MinX instead") = NSRectEdgeMinX,
  // expected-remark@-1 {{C enum constant 'NSMinXEdge' imported as unavailable static property 'NSMinXEdge'}}
  NSMinYEdge NS_SWIFT_UNAVAILABLE("Use NSRectEdge.MinY instead") = NSRectEdgeMinY,
  // expected-remark@-1 {{C enum constant 'NSMinYEdge' imported as unavailable static property 'NSMinYEdge'}}
  NSMaxXEdge __attribute__((availability(swift, unavailable, message="Use NSRectEdge.MaxX instead"))) = NSRectEdgeMaxX,
  // expected-remark@-1 {{C enum constant 'NSMaxXEdge' imported as unavailable static property 'NSMaxXEdge'}}
  NSMaxYEdge __attribute__((availability(swift, unavailable, message="Use NSRectEdge.MaxY instead"))) = NSRectEdgeMaxY,
  // expected-remark@-1 {{C enum constant 'NSMaxYEdge' imported as unavailable static property 'NSMaxYEdge'}}
};

// From CoreBluetooth
typedef NS_OPTIONS(NSInteger, CBCharacteristicProperties) {
  // expected-remark@-1 {{C enum 'CBCharacteristicProperties' imported as struct 'CBCharacteristicProperties'}}
  // expected-remark@-2 {{C typedef 'CBCharacteristicProperties' imported as struct 'CBCharacteristicProperties'}}
  CBCharacteristicPropertyBroadcast = 0x01,
  // expected-remark@-1 {{C enum constant 'CBCharacteristicPropertyBroadcast' imported as static property 'broadcast'}}
  CBCharacteristicPropertyRead = 0x02,
  // expected-remark@-1 {{C enum constant 'CBCharacteristicPropertyRead' imported as static property 'read'}}
  CBCharacteristicPropertyWriteWithoutResponse = 0x04,
  // expected-remark@-1 {{C enum constant 'CBCharacteristicPropertyWriteWithoutResponse' imported as static property 'writeWithoutResponse'}}
  CBCharacteristicPropertyWrite = 0x08,
  // expected-remark@-1 {{C enum constant 'CBCharacteristicPropertyWrite' imported as static property 'write'}}
  CBCharacteristicPropertyNotify = 0x10,
  // expected-remark@-1 {{C enum constant 'CBCharacteristicPropertyNotify' imported as static property 'notify'}}
  CBCharacteristicPropertyIndicate = 0x20,
  // expected-remark@-1 {{C enum constant 'CBCharacteristicPropertyIndicate' imported as static property 'indicate'}}
  CBCharacteristicPropertyAuthenticatedSignedWrites = 0x40,
  // expected-remark@-1 {{C enum constant 'CBCharacteristicPropertyAuthenticatedSignedWrites' imported as static property 'authenticatedSignedWrites'}}
  CBCharacteristicPropertyExtendedProperties = 0x80,
  // expected-remark@-1 {{C enum constant 'CBCharacteristicPropertyExtendedProperties' imported as static property 'extendedProperties'}}
  CBCharacteristicPropertyNotifyEncryptionRequired /*NS_ENUM_AVAILABLE(10_9, 6_0)*/ = 0x100,
  // expected-remark@-1 {{C enum constant 'CBCharacteristicPropertyNotifyEncryptionRequired' imported as static property 'notifyEncryptionRequired'}}
  CBCharacteristicPropertyIndicateEncryptionRequired /*NS_ENUM_AVAILABLE(10_9, 6_0)*/ = 0x200
  // expected-remark@-1 {{C enum constant 'CBCharacteristicPropertyIndicateEncryptionRequired' imported as static property 'indicateEncryptionRequired'}}
};

// From CoreMedia
typedef CF_OPTIONS(unsigned int, CMTimeFlags) {
  // expected-remark@-1 {{C enum 'CMTimeFlags' imported as struct 'CMTimeFlags'}}
  // expected-remark@-2 {{C typedef 'CMTimeFlags' imported as struct 'CMTimeFlags'}}
  kCMTimeFlags_Valid = 1UL<<0,
  // expected-remark@-1 {{C enum constant 'kCMTimeFlags_Valid' imported as static property 'valid'}}
  kCMTimeFlags_HasBeenRounded = 1UL<<1,
  // expected-remark@-1 {{C enum constant 'kCMTimeFlags_HasBeenRounded' imported as static property 'hasBeenRounded'}}
  kCMTimeFlags_PositiveInfinity = 1UL<<2,
  // expected-remark@-1 {{C enum constant 'kCMTimeFlags_PositiveInfinity' imported as static property 'positiveInfinity'}}
  kCMTimeFlags_NegativeInfinity = 1UL<<3,
  // expected-remark@-1 {{C enum constant 'kCMTimeFlags_NegativeInfinity' imported as static property 'negativeInfinity'}}
  kCMTimeFlags_Indefinite = 1UL<<4,
  // expected-remark@-1 {{C enum constant 'kCMTimeFlags_Indefinite' imported as static property 'indefinite'}}
  kCMTimeFlags_ImpliedValueFlagsMask = kCMTimeFlags_PositiveInfinity | kCMTimeFlags_NegativeInfinity | kCMTimeFlags_Indefinite
  // expected-remark@-1 {{C enum constant 'kCMTimeFlags_ImpliedValueFlagsMask' imported as static property 'impliedValueFlagsMask'}}
};
typedef CF_OPTIONS(unsigned int, CMTimeFlagsWithNumber) {
  // expected-remark@-1 {{C enum 'CMTimeFlagsWithNumber' imported as struct 'CMTimeFlagsWithNumber'}}
  // expected-remark@-2 {{C typedef 'CMTimeFlagsWithNumber' imported as struct 'CMTimeFlagsWithNumber'}}
  kCMTimeFlagsWithNumber_Valid = 1UL<<0,
  // expected-remark@-1 {{C enum constant 'kCMTimeFlagsWithNumber_Valid' imported as static property '_Valid'}}
  kCMTimeFlagsWithNumber_888 = 1UL<<1,
  // expected-remark@-1 {{C enum constant 'kCMTimeFlagsWithNumber_888' imported as static property '_888'}}
};


// Contrived name with a plural "-es"...normally these are "beeps".
typedef NS_OPTIONS(NSInteger, AlertBuzzes) {
  // expected-remark@-1 {{C enum 'AlertBuzzes' imported as struct 'AlertBuzzes'}}
  // expected-remark@-2 {{C typedef 'AlertBuzzes' imported as struct 'AlertBuzzes'}}
  AlertBuzzNone = 0,
  // expected-remark@-1 {{C enum constant 'AlertBuzzNone' imported as unavailable static property 'none'}}
  AlertBuzzFunk = 1 << 0,
  // expected-remark@-1 {{C enum constant 'AlertBuzzFunk' imported as static property 'funk'}}
  AlertBuzzHero = 1 << 1,
  // expected-remark@-1 {{C enum constant 'AlertBuzzHero' imported as static property 'hero'}}
  AlertBuzzSosumi = 1 << 2
  // expected-remark@-1 {{C enum constant 'AlertBuzzSosumi' imported as static property 'sosumi'}}
};

// From AppKit
typedef NS_OPTIONS(NSUInteger, NSBitmapFormat) {
  // expected-remark@-1 {{C enum 'NSBitmapFormat' imported as struct 'NSBitmapFormat'}}
  // expected-remark@-2 {{C typedef 'NSBitmapFormat' imported as struct 'NSBitmapFormat'}}
  NSAlphaFirstBitmapFormat            = 1 << 0, // 0 means is alpha last (RGBA, CMYKA, etc.)
  // expected-remark@-1 {{C enum constant 'NSAlphaFirstBitmapFormat' imported as static property 'NSAlphaFirstBitmapFormat'}}
  NSAlphaNonpremultipliedBitmapFormat = 1 << 1, // 0 means is premultiplied
  // expected-remark@-1 {{C enum constant 'NSAlphaNonpremultipliedBitmapFormat' imported as static property 'NSAlphaNonpremultipliedBitmapFormat'}}
  NSFloatingPointSamplesBitmapFormat  = 1 << 2, // 0 is integer
  // expected-remark@-1 {{C enum constant 'NSFloatingPointSamplesBitmapFormat' imported as static property 'NSFloatingPointSamplesBitmapFormat'}}

  NS16BitLittleEndianBitmapFormat /*NS_ENUM_AVAILABLE_MAC(10_51)*/ = (1 << 8),
  // expected-remark@-1 {{C enum constant 'NS16BitLittleEndianBitmapFormat' imported as static property 'NS16BitLittleEndianBitmapFormat'}}
  NS32BitLittleEndianBitmapFormat /*NS_ENUM_AVAILABLE_MAC(10_51)*/ = (1 << 9),
  // expected-remark@-1 {{C enum constant 'NS32BitLittleEndianBitmapFormat' imported as static property 'NS32BitLittleEndianBitmapFormat'}}
  NS16BitBigEndianBitmapFormat /*NS_ENUM_AVAILABLE_MAC(10_51)*/ = (1 << 10),
  // expected-remark@-1 {{C enum constant 'NS16BitBigEndianBitmapFormat' imported as static property 'NS16BitBigEndianBitmapFormat'}}
  NS32BitBigEndianBitmapFormat /*NS_ENUM_AVAILABLE_MAC(10_51)*/ = (1 << 11)
  // expected-remark@-1 {{C enum constant 'NS32BitBigEndianBitmapFormat' imported as static property 'NS32BitBigEndianBitmapFormat'}}
};

typedef NS_OPTIONS(NSUInteger, NSBitmapFormatReversed) {
  // expected-remark@-1 {{C enum 'NSBitmapFormatReversed' imported as struct 'NSBitmapFormatReversed'}}
  // expected-remark@-2 {{C typedef 'NSBitmapFormatReversed' imported as struct 'NSBitmapFormatReversed'}}
  NS16BitLittleEndianBitmapFormatR /*NS_ENUM_AVAILABLE_MAC(10_51)*/ = (1 << 8),
  // expected-remark@-1 {{C enum constant 'NS16BitLittleEndianBitmapFormatR' imported as static property 'NS16BitLittleEndianBitmapFormatR'}}
  NS32BitLittleEndianBitmapFormatR /*NS_ENUM_AVAILABLE_MAC(10_51)*/ = (1 << 9),
  // expected-remark@-1 {{C enum constant 'NS32BitLittleEndianBitmapFormatR' imported as static property 'NS32BitLittleEndianBitmapFormatR'}}
  NS16BitBigEndianBitmapFormatR /*NS_ENUM_AVAILABLE_MAC(10_51)*/ = (1 << 10),
  // expected-remark@-1 {{C enum constant 'NS16BitBigEndianBitmapFormatR' imported as static property 'NS16BitBigEndianBitmapFormatR'}}
  NS32BitBigEndianBitmapFormatR /*NS_ENUM_AVAILABLE_MAC(10_51)*/ = (1 << 11),
  // expected-remark@-1 {{C enum constant 'NS32BitBigEndianBitmapFormatR' imported as static property 'NS32BitBigEndianBitmapFormatR'}}

  NSAlphaFirstBitmapFormatR            = 1 << 0, // 0 means is alpha last (RGBA, CMYKA, etc.)
  // expected-remark@-1 {{C enum constant 'NSAlphaFirstBitmapFormatR' imported as static property 'NSAlphaFirstBitmapFormatR'}}
  NSAlphaNonpremultipliedBitmapFormatR = 1 << 1, // 0 means is premultiplied
  // expected-remark@-1 {{C enum constant 'NSAlphaNonpremultipliedBitmapFormatR' imported as static property 'NSAlphaNonpremultipliedBitmapFormatR'}}
  NSFloatingPointSamplesBitmapFormatR  = 1 << 2, // 0 is integer
  // expected-remark@-1 {{C enum constant 'NSFloatingPointSamplesBitmapFormatR' imported as static property 'NSFloatingPointSamplesBitmapFormatR'}}
};

typedef NS_OPTIONS(NSUInteger, NSBitmapFormat2) {
  // expected-remark@-1 {{C enum 'NSBitmapFormat2' imported as struct 'NSBitmapFormat2'}}
  // expected-remark@-2 {{C typedef 'NSBitmapFormat2' imported as struct 'NSBitmapFormat2'}}
  NSU16a = 1,
  // expected-remark@-1 {{C enum constant 'NSU16a' imported as static property 'NSU16a'}}
  NSU32a = 2,
  // expected-remark@-1 {{C enum constant 'NSU32a' imported as static property 'NSU32a'}}
};

typedef NS_OPTIONS(NSUInteger, NSBitmapFormat3) {
  // expected-remark@-1 {{C enum 'NSBitmapFormat3' imported as struct 'NSBitmapFormat3'}}
  // expected-remark@-2 {{C typedef 'NSBitmapFormat3' imported as struct 'NSBitmapFormat3'}}
  NSU16b = 1,
  // expected-remark@-1 {{C enum constant 'NSU16b' imported as static property 'NSU16b'}}
  NSU32b = 2,
  // expected-remark@-1 {{C enum constant 'NSU32b' imported as static property 'NSU32b'}}
  NSS16b = 4,
  // expected-remark@-1 {{C enum constant 'NSS16b' imported as static property 'NSS16b'}}
  NSS32b = 8,
  // expected-remark@-1 {{C enum constant 'NSS32b' imported as static property 'NSS32b'}}
};

typedef NS_OPTIONS(NSUInteger, NSUBitmapFormat4) {
  // expected-remark@-1 {{C enum 'NSUBitmapFormat4' imported as struct 'NSUBitmapFormat4'}}
  // expected-remark@-2 {{C typedef 'NSUBitmapFormat4' imported as struct 'NSUBitmapFormat4'}}
  NSU16c = 1,
  // expected-remark@-1 {{C enum constant 'NSU16c' imported as static property 'NSU16c'}}
  NSU32c = 2,
  // expected-remark@-1 {{C enum constant 'NSU32c' imported as static property 'NSU32c'}}
};

typedef NS_OPTIONS(NSUInteger, NSABitmapFormat5) {
  // expected-remark@-1 {{C enum 'NSABitmapFormat5' imported as struct 'NSABitmapFormat5'}}
  // expected-remark@-2 {{C typedef 'NSABitmapFormat5' imported as struct 'NSABitmapFormat5'}}
  NSAA16d = 1,
  // expected-remark@-1 {{C enum constant 'NSAA16d' imported as static property 'NSAA16d'}}
  NSAB32d = 2,
  // expected-remark@-1 {{C enum constant 'NSAB32d' imported as static property 'NSAB32d'}}
};

/// Aaa.  NSPotentiallyUnavailableOptions.  Bbb.
typedef NS_OPTIONS(NSUInteger, NSPotentiallyUnavailableOptions) {
  // expected-remark@-1 {{C enum 'NSPotentiallyUnavailableOptions' imported as struct 'NSPotentiallyUnavailableOptions'}}
  // expected-remark@-2 {{C typedef 'NSPotentiallyUnavailableOptions' imported as struct 'NSPotentiallyUnavailableOptions'}}
  NSPotentiallyUnavailableOptionsFirst   = (1 << 0),
  // expected-remark@-1 {{C enum constant 'NSPotentiallyUnavailableOptionsFirst' imported as static property 'first'}}
  NSPotentiallyUnavailableOptionsSecond  = (1 << 1),
  // expected-remark@-1 {{C enum constant 'NSPotentiallyUnavailableOptionsSecond' imported as static property 'second'}}
  NSPotentiallyUnavailableOptionsThird   = (1 << 2),
  // expected-remark@-1 {{C enum constant 'NSPotentiallyUnavailableOptionsThird' imported as static property 'third'}}
}  __attribute__((availability(macosx, introduced=10.51)));

/// Aaa.  NSOptionsWithUnavailableElement.  Bbb.
typedef NS_OPTIONS(NSUInteger, NSOptionsWithUnavailableElement) {
  // expected-remark@-1 {{C enum 'NSOptionsWithUnavailableElement' imported as struct 'NSOptionsWithUnavailableElement'}}
  // expected-remark@-2 {{C typedef 'NSOptionsWithUnavailableElement' imported as struct 'NSOptionsWithUnavailableElement'}}
  NSOptionsWithUnavailableElementFirst    = (1 << 0),
  // expected-remark@-1 {{C enum constant 'NSOptionsWithUnavailableElementFirst' imported as static property 'first'}}
  NSOptionsWithUnavailableElementSecond   = (1 << 1),
  // expected-remark@-1 {{C enum constant 'NSOptionsWithUnavailableElementSecond' imported as static property 'second'}}
  NSOptionsWithUnavailableElementThird __attribute__((availability(macosx, introduced=10.51))) = (1 << 2),
  // expected-remark@-1 {{C enum constant 'NSOptionsWithUnavailableElementThird' imported as static property 'third'}}
};

/// Aaa.  NSUnavailableEnum.  Bbb.
typedef NS_ENUM(NSUInteger, NSUnavailableEnum) {
  // expected-remark@-1 {{C enum 'NSUnavailableEnum' imported as enum 'NSUnavailableEnum'}}
  // expected-remark@-2 {{C typedef 'NSUnavailableEnum' imported as enum 'NSUnavailableEnum'}}
  NSUnavailableEnumFirst,
  // expected-remark@-1 {{C enum constant 'NSUnavailableEnumFirst' imported as enum case 'first'}}
  NSUnavailableEnumSecond,
  // expected-remark@-1 {{C enum constant 'NSUnavailableEnumSecond' imported as enum case 'second'}}
  NSUnavailableEnumThird,
  // expected-remark@-1 {{C enum constant 'NSUnavailableEnumThird' imported as enum case 'third'}}
}  __attribute__((availability(macosx, introduced=10.51)));

/// Aaa.  NSEnumWithUnavailableElement.  Bbb.
typedef NS_ENUM(NSUInteger, NSEnumWithUnavailableElement) {
  // expected-remark@-1 {{C enum 'NSEnumWithUnavailableElement' imported as enum 'NSEnumWithUnavailableElement'}}
  // expected-remark@-2 {{C typedef 'NSEnumWithUnavailableElement' imported as enum 'NSEnumWithUnavailableElement'}}
  NSEnumWithUnavailableElementFirst,
  // expected-remark@-1 {{C enum constant 'NSEnumWithUnavailableElementFirst' imported as enum case 'first'}}
  NSEnumWithUnavailableElementSecond,
  // expected-remark@-1 {{C enum constant 'NSEnumWithUnavailableElementSecond' imported as enum case 'second'}}
  NSEnumWithUnavailableElementThird __attribute__((availability(macosx, introduced=10.51))),
  // expected-remark@-1 {{C enum constant 'NSEnumWithUnavailableElementThird' imported as enum case 'third'}}
};

typedef NS_OPTIONS(NSUInteger, NSDeprecatedOptions) {
  // expected-remark@-1 {{C enum 'NSDeprecatedOptions' imported as struct 'NSDeprecatedOptions'}}
  // expected-remark@-2 {{C typedef 'NSDeprecatedOptions' imported as struct 'NSDeprecatedOptions'}}
  NSDeprecatedOptionsNone = 0,
  // expected-remark@-1 {{C enum constant 'NSDeprecatedOptionsNone' imported as unavailable static property 'none'}}
  NSDeprecatedOptionsFirst   = (1 << 0)
  // expected-remark@-1 {{C enum constant 'NSDeprecatedOptionsFirst' imported as static property 'first'}}
}  __attribute__((availability(macosx, introduced=10.51, deprecated=10.51, message="Use a different API")));

typedef NS_ENUM(NSUInteger, NSDeprecatedEnum) {
  // expected-remark@-1 {{C enum 'NSDeprecatedEnum' imported as enum 'NSDeprecatedEnum'}}
  // expected-remark@-2 {{C typedef 'NSDeprecatedEnum' imported as enum 'NSDeprecatedEnum'}}
  NSDeprecatedEnumFirst
  // expected-remark@-1 {{C enum constant 'NSDeprecatedEnumFirst' imported as enum case 'first'}}
} __attribute__((availability(macosx, introduced=10.51, deprecated=10.51, message="Use a different API")));

typedef NS_OPTIONS(NSUInteger, NSExplicitlyUnavailableOptions) {
  // expected-remark@-1 {{C enum 'NSExplicitlyUnavailableOptions' imported as unavailable struct 'NSExplicitlyUnavailableOptions'}}
  // expected-remark@-2 {{C typedef 'NSExplicitlyUnavailableOptions' imported as unavailable struct 'NSExplicitlyUnavailableOptions'}}
  NSExplicitlyUnavailableOptionsNone = 0,
  // expected-remark@-1 {{C enum constant 'NSExplicitlyUnavailableOptionsNone' imported as unavailable static property 'none'}}
  NSExplicitlyUnavailableOptionsFirst   = (1 << 0)
  // expected-remark@-1 {{C enum constant 'NSExplicitlyUnavailableOptionsFirst' imported as static property 'first'}}
} __attribute__((unavailable));

typedef NS_OPTIONS(NSUInteger, NSExplicitlyUnavailableOnOSXOptions) {
  // expected-remark@-1 {{C enum 'NSExplicitlyUnavailableOnOSXOptions' imported as unavailable struct 'NSExplicitlyUnavailableOnOSXOptions'}}
  // expected-remark@-2 {{C typedef 'NSExplicitlyUnavailableOnOSXOptions' imported as unavailable struct 'NSExplicitlyUnavailableOnOSXOptions'}}
  NSExplicitlyUnavailableOnOSXOptionsNone = 0,
  // expected-remark@-1 {{C enum constant 'NSExplicitlyUnavailableOnOSXOptionsNone' imported as unavailable static property 'none'}}
  NSExplicitlyUnavailableOnOSXOptionsFirst   = (1 << 0)
  // expected-remark@-1 {{C enum constant 'NSExplicitlyUnavailableOnOSXOptionsFirst' imported as static property 'first'}}
}  __attribute__((availability(macosx, unavailable, message="Use a different API")));


@interface NSClassWithDeprecatedOptionsInMethodSignature : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSClassWithDeprecatedOptionsInMethodSignature' imported as class 'NSClassWithDeprecatedOptionsInMethodSignature'}}
+ (NSClassWithDeprecatedOptionsInMethodSignature *) sharedInstance;
// expected-remark@-1 {{Objective-C method '+[NSClassWithDeprecatedOptionsInMethodSignature sharedInstance]' imported as class method 'sharedInstance()'}}
@end

@interface NSClassWithDeprecatedOptionsInMethodSignature (ActuallyUseOptions)
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
  - (void)someMethodWithDeprecatedOptions:(NSDeprecatedOptions)options __attribute__((availability(macosx, introduced=10.51, deprecated=10.51, message="Use a different API")));
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
@end

@interface NSClassWithExplicitlyUnavailableOptionsInMethodSignature : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSClassWithExplicitlyUnavailableOptionsInMethodSignature' imported as class 'NSClassWithExplicitlyUnavailableOptionsInMethodSignature'}}
+ (NSClassWithExplicitlyUnavailableOptionsInMethodSignature *) sharedInstance;
// expected-remark@-1 {{Objective-C method '+[NSClassWithExplicitlyUnavailableOptionsInMethodSignature sharedInstance]' imported as class method 'sharedInstance()'}}
@end

@interface NSClassWithExplicitlyUnavailableOptionsInMethodSignature (ActuallyUseOptions)
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
- (void)someMethodWithUnavailableOptions:(NSExplicitlyUnavailableOptions)options __attribute__((unavailable));
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
- (void)someMethodWithUnavailableOptionsOnOSX:(NSExplicitlyUnavailableOnOSXOptions)options __attribute__((availability(macosx, unavailable, message="Use a different API")));
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
@end

@interface NSClassWithPotentiallyUnavailableOptionsInMethodSignature : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSClassWithPotentiallyUnavailableOptionsInMethodSignature' imported as class 'NSClassWithPotentiallyUnavailableOptionsInMethodSignature'}}
+ (NSClassWithPotentiallyUnavailableOptionsInMethodSignature *) sharedInstance;
// expected-remark@-1 {{Objective-C method '+[NSClassWithPotentiallyUnavailableOptionsInMethodSignature sharedInstance]' imported as class method 'sharedInstance()'}}
- (void)someMethodWithPotentiallyUnavailableOptions:(NSPotentiallyUnavailableOptions)options __attribute__((availability(macosx, introduced=10.52)));
// expected-remark@-1 {{Objective-C method '-[NSClassWithPotentiallyUnavailableOptionsInMethodSignature someMethodWithPotentiallyUnavailableOptions:]' imported as instance method 'someMethod(potentiallyUnavailableOptions:)'}}
@end

@protocol NSWobbling
// expected-remark@-1 {{Objective-C protocol 'NSWobbling' imported as protocol 'NSWobbling'}}
-(void)wobble;
// expected-remark@-1 {{Objective-C method '-[NSWobbling wobble]' imported as instance method 'wobble()'}}

- (instancetype)returnMyself;
// expected-remark@-1 {{Objective-C method '-[NSWobbling returnMyself]' imported as instance method 'returnMyself()'}}

@optional
-(void)wibble;
// expected-remark@-1 {{Objective-C method '-[NSWobbling wibble]' imported as instance method 'wibble()'}}

- (id)objectAtIndexedSubscript:(NSUInteger)idx;
// expected-remark@-1 {{Objective-C method '-[NSWobbling objectAtIndexedSubscript:]' imported as unavailable instance method 'objectAtIndexedSubscript'}}
@end

@protocol NSMaybeInitWobble
// expected-remark@-1 {{Objective-C protocol 'NSMaybeInitWobble' imported as protocol 'NSMaybeInitWobble'}}
@optional
- (id)initWithWobble:(int)wobble;
// expected-remark@-1 {{Objective-C method '-[NSMaybeInitWobble initWithWobble:]' imported as initializer 'init(wobble:)'}}
@end

@interface NSURLRequest : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSURLRequest' imported as class 'NSURLRequest'}}
- (instancetype)requestWithString:(NSString *)URLString;
// expected-remark@-1 {{Objective-C method '-[NSURLRequest requestWithString:]' imported as instance method 'withString'}}
+ (instancetype)requestWithString:(NSString *)URLString;
// expected-remark@-1 {{Objective-C method '+[NSURLRequest requestWithString:]' imported as initializer 'init(string:)'}}
+ (instancetype)URLRequestWithURL:(NSURL *)URL;
// expected-remark@-1 {{Objective-C method '+[NSURLRequest URLRequestWithURL:]' imported as initializer 'init(url:)'}}
@end

NS_SWIFT_UNAVAILABLE("NSInvocation and related APIs not available")
@interface NSInvocation : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSInvocation' imported as unavailable class 'NSInvocation'}}
@end

typedef NS_ENUM(NSInteger, NSByteCountFormatterCountStyle) {
  // expected-remark@-1 {{C enum 'NSByteCountFormatterCountStyle' imported as enum 'CountStyle'}}
  // expected-remark@-2 {{C typedef 'NSByteCountFormatterCountStyle' imported as enum 'CountStyle'}}
  NSByteCountFormatterCountStyleFile    = 0,
  // expected-remark@-1 {{C enum constant 'NSByteCountFormatterCountStyleFile' imported as enum case 'file'}}
  NSByteCountFormatterCountStyleMemory  = 1,
  // expected-remark@-1 {{C enum constant 'NSByteCountFormatterCountStyleMemory' imported as enum case 'memory'}}
  NSByteCountFormatterCountStyleDecimal = 2,
  // expected-remark@-1 {{C enum constant 'NSByteCountFormatterCountStyleDecimal' imported as enum case 'decimal'}}
  NSByteCountFormatterCountStyleBinary  = 3
  // expected-remark@-1 {{C enum constant 'NSByteCountFormatterCountStyleBinary' imported as enum case 'binary'}}
};

@interface NSByteCountFormatter : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSByteCountFormatter' imported as class 'ByteCountFormatter'}}

@property NSByteCountFormatterCountStyle countStyle;
// expected-remark@-1 {{Objective-C method '-[NSByteCountFormatter countStyle]' imported as getter for property 'countStyle'}}
// expected-remark@-2 {{Objective-C method '-[NSByteCountFormatter setCountStyle:]' imported as setter for property 'countStyle'}}
// expected-remark@-3 {{Objective-C property '-NSByteCountFormatter.countStyle' imported as property 'countStyle'}}

@end

NSArray *arrayToArray(NSArray *arr);
// expected-remark@-1 {{C function 'arrayToArray' imported as global function 'arrayToArray'}}
NSDictionary *dictToDict(NSDictionary *dict);
// expected-remark@-1 {{C function 'dictToDict' imported as global function 'dictToDict'}}
NSSet *setToSet(NSSet *dict);
// expected-remark@-1 {{C function 'setToSet' imported as global function 'setToSet'}}

@interface NSExtensionContext : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSExtensionContext' imported as class 'NSExtensionContext'}}
- (void)openURL:(NSURL *)URL completionHandler:(void (^)(BOOL success))completionHandler;
// expected-remark@-1 {{Objective-C method '-[NSExtensionContext openURL:completionHandler:]' imported as instance method 'open(_:completionHandler:)'}}

// Fake API, for testing initialisms.
- (void)openGUID:(NSGUID *)GUID completionHandler:(void (^)(BOOL success))completionHandler;
// expected-remark@-1 {{Objective-C method '-[NSExtensionContext openGUID:completionHandler:]' imported as instance method 'open(_:completionHandler:)'}}
@end

@interface NSProcessInfo : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSProcessInfo' imported as class 'ProcessInfo'}}
@property (class, readonly, strong, nonnull) NSProcessInfo *processInfo;
// expected-remark@-1 {{Objective-C method '+[NSProcessInfo processInfo]' imported as getter for class property 'processInfo'}}
// expected-remark@-2 {{Objective-C property '+NSProcessInfo.processInfo' imported as class property 'processInfo'}}
@end

@interface NSString(FoundationExts)
// expected-remark@-1 {{Objective-C category 'NSString(FoundationExts)' imported as extension of 'NSString'}}
- (void)notBridgedMethod;
// expected-remark@-1 {{Objective-C method '-[NSString(FoundationExts) notBridgedMethod]' imported as instance method 'notBridgedMethod()'}}
@end

@interface NSString(FoundationExts)
// expected-remark@-1 {{Objective-C category 'NSString(FoundationExts)' imported as extension of 'NSString'}}
@property (nonatomic, copy) NSString *uppercaseString;
// expected-remark@-1 {{Objective-C method '-[NSString(FoundationExts) setUppercaseString:]' imported as setter for property 'uppercased'}}
// expected-remark@-2 {{Objective-C method '-[NSString(FoundationExts) uppercaseString]' imported as getter for property 'uppercased'}}
// expected-remark@-3 {{Objective-C property '-NSString(FoundationExts).uppercaseString' imported as property 'uppercased'}}
@end

typedef struct {
    // expected-remark@-1 {{C struct 'NSFastEnumerationState' imported as struct 'NSFastEnumerationState'}}
    unsigned long state;
    // expected-remark@-1 {{C field '(anonymous struct)::state' imported as property 'state'}}
    id __unsafe_unretained *itemsPtr;
    // expected-remark@-1 {{C field '(anonymous struct)::itemsPtr' imported as property 'itemsPtr'}}
    unsigned long *mutationsPtr;
    // expected-remark@-1 {{C field '(anonymous struct)::mutationsPtr' imported as property 'mutationsPtr'}}
    unsigned long extra[5];
    // expected-remark@-1 {{C field '(anonymous struct)::extra' imported as property 'extra'}}
} NSFastEnumerationState;
// expected-remark@-1 {{C typedef 'NSFastEnumerationState' imported as struct 'NSFastEnumerationState'}}

@protocol NSFastEnumeration
// expected-remark@-1 {{Objective-C protocol 'NSFastEnumeration' imported as protocol 'NSFastEnumeration'}}

- (NSUInteger)countByEnumeratingWithState:(nonnull NSFastEnumerationState *)state objects:(id __unsafe_unretained [])buffer count:(NSUInteger)len;
// expected-remark@-1 {{Objective-C method '-[NSFastEnumeration countByEnumeratingWithState:objects:count:]' imported as instance method 'countByEnumerating(with:objects:count:)'}}

@end

typedef const void * CFTypeRef;
// expected-remark@-1 {{C typedef 'CFTypeRef' imported as type alias 'CFTypeRef'}}
typedef const struct __attribute__((objc_bridge(NSString))) __CFString * CFStringRef;
// expected-remark@-1 {{C typedef 'CFStringRef' imported as class 'CFString'}}
typedef const struct __attribute__((objc_bridge(NSDictionary))) __CFDictionary * CFDictionaryRef;
// expected-remark@-1 {{C typedef 'CFDictionaryRef' imported as class 'CFDictionary'}}
typedef struct CGColor *CGColorRef;
// expected-remark@-1 {{C typedef 'CGColorRef' imported as class 'CGColor'}}

extern CFTypeRef CFRetain(CFTypeRef cf);
// expected-remark@-1 {{C function 'CFRetain' imported as unavailable global function 'CFRetain'}}
extern void CFRelease(CFTypeRef cf);
// expected-remark@-1 {{C function 'CFRelease' imported as unavailable global function 'CFRelease'}}
extern CFTypeRef CFAutorelease(CFTypeRef __attribute__((cf_consumed)) arg) __attribute__((availability(macosx,introduced=10.9)));
// expected-remark@-1 {{C function 'CFAutorelease' imported as unavailable global function 'CFAutorelease'}}
extern CGColorRef CGColorRetain(CGColorRef color) __attribute__((availability(macosx,introduced=10.3)));
// expected-remark@-1 {{C function 'CGColorRetain' imported as unavailable global function 'CGColorRetain'}}
extern void CGColorRelease(CGColorRef color) __attribute__((availability(macosx,introduced=10.3)));
// expected-remark@-1 {{C function 'CGColorRelease' imported as unavailable global function 'CGColorRelease'}}

@interface NSObject (NSDistributedObjects)
// expected-remark@-1 {{Objective-C category 'NSObject(NSDistributedObjects)' imported as extension of 'NSObject'}}
@property (readonly) Class classForPortCoder NS_SWIFT_UNAVAILABLE("Use NSXPCConnection instead");
// FIXME: why not unavailable? expected-remark@-1 {{Objective-C method '-[NSObject(NSDistributedObjects) classForPortCoder]' also imported as class method 'classForPortCoder()'}}
// expected-remark@-2 {{Objective-C method '-[NSObject(NSDistributedObjects) classForPortCoder]' imported as unavailable getter for property 'classForPortCoder'}}
// expected-remark@-3 {{Objective-C property '-NSObject(NSDistributedObjects).classForPortCoder' imported as unavailable property 'classForPortCoder'}}
@end

typedef NSString *_Nonnull NSNotificationName
    __attribute((swift_newtype(struct)));
// expected-remark@-2 {{C typedef 'NSNotificationName' imported as struct 'Name'}}

NS_SWIFT_UNAVAILABLE("Use NSXPCConnection instead")
extern NSString * const NSConnectionReplyMode;
// expected-remark@-1 {{C variable 'NSConnectionReplyMode' imported as unavailable let 'NSConnectionReplyMode'}}
NS_SWIFT_UNAVAILABLE("Use NSXPCConnection instead")
extern NSString * const NSConnectionDidDieNotification;
// expected-remark@-1 {{C variable 'NSConnectionDidDieNotification' imported as unavailable static property 'NSConnectionDidDie'}}
NS_SWIFT_UNAVAILABLE("Use NSXPCConnection instead")
@interface NSConnection : NSObject {
  // expected-remark@-1 {{Objective-C class interface 'NSConnection' imported as unavailable class 'NSConnection'}}
}
@end
NS_SWIFT_UNAVAILABLE("Use NSXPCConnection instead")
@interface NSPortCoder : NSCoder
// expected-remark@-1 {{Objective-C class interface 'NSPortCoder' imported as unavailable class 'NSPortCoder'}}
@end
NS_SWIFT_UNAVAILABLE("Use NSXPCConnection instead")
@protocol NSConnectionDelegate <NSObject>
// expected-remark@-1 {{Objective-C protocol 'NSConnectionDelegate' imported as unavailable protocol 'NSConnectionDelegate'}}
@end
NS_SWIFT_UNAVAILABLE("Use NSXPCConnection instead")
@interface NSDistantObjectRequest : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSDistantObjectRequest' imported as unavailable class 'NSDistantObjectRequest'}}
@end
NS_SWIFT_UNAVAILABLE("Use NSXPCConnection instead")
@interface NSDistantObject
// expected-remark@-1 {{Objective-C class interface 'NSDistantObject' imported as unavailable class 'NSDistantObject'}}
@end
NS_SWIFT_UNAVAILABLE("Use NSXPCConnection instead")
@interface NSPortNameServer : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSPortNameServer' imported as unavailable class 'NSPortNameServer'}}
@end
NS_SWIFT_UNAVAILABLE("Use NSXPCConnection instead")
@interface NSMachBootstrapServer : NSPortNameServer
// expected-remark@-1 {{Objective-C class interface 'NSMachBootstrapServer' imported as unavailable class 'NSMachBootstrapServer'}}
@end
NS_SWIFT_UNAVAILABLE("Use NSXPCConnection instead")
@interface NSMessagePortNameServer : NSPortNameServer
// expected-remark@-1 {{Objective-C class interface 'NSMessagePortNameServer' imported as unavailable class 'NSMessagePortNameServer'}}
@end
NS_SWIFT_UNAVAILABLE("Use NSXPCConnection instead")
@interface NSSocketPortNameServer : NSPortNameServer
// expected-remark@-1 {{Objective-C class interface 'NSSocketPortNameServer' imported as unavailable class 'NSSocketPortNameServer'}}
@end
NS_SWIFT_UNAVAILABLE("Use NSCalendar and NSDateComponents and NSDateFormatter instead")
@interface NSCalendarDate : NSDate
// expected-remark@-1 {{Objective-C class interface 'NSCalendarDate' imported as unavailable class 'NSCalendarDate'}}
@end
NS_SWIFT_UNAVAILABLE("NSInvocation and related APIs not available")
@interface NSInvocationOperation : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSInvocationOperation' imported as unavailable class 'NSInvocationOperation'}}
@end
NS_SWIFT_UNAVAILABLE("NSInvocation and related APIs not available")
@interface NSMethodSignature : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSMethodSignature' imported as unavailable class 'NSMethodSignature'}}
@end
/// Unavailable Global Functions
extern void NSSetZoneName(NSZone *_Nonnull zone, NSString *_Nonnull name)
    NS_SWIFT_UNAVAILABLE("Zone-based memory management is unavailable");
// expected-remark@-2 {{C function 'NSSetZoneName' imported as unavailable global function 'NSSetZoneName'}}
extern NSString *NSZoneName(NSZone *zone) NS_SWIFT_UNAVAILABLE("Zone-based memory management is unavailable");
// expected-remark@-1 {{C function 'NSZoneName' imported as unavailable global function 'NSZoneName'}}
extern NSZone *NSCreateZone(NSUInteger startSize, NSUInteger granularity, BOOL canFree) NS_SWIFT_UNAVAILABLE("Zone-based memory management is unavailable");
// expected-remark@-1 {{C function 'NSCreateZone' imported as unavailable global function 'NSCreateZone'}}

@interface NSXPCInterface : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSXPCInterface' imported as class 'NSXPCInterface'}}
+ (NSXPCInterface *)interfaceWithProtocol:(Protocol *)protocol;
// expected-remark@-1 {{Objective-C method '+[NSXPCInterface interfaceWithProtocol:]' imported as initializer 'init(with:)'}}
@end

typedef struct NonNilableReferences {
  // expected-remark@-1 {{C struct 'NonNilableReferences' imported as struct 'NonNilableReferences'}}
  NSObject *_Nonnull __unsafe_unretained obj;
  // expected-remark@-1 {{C field 'NonNilableReferences::obj' imported as property 'obj'}}
} NonNilableReferences;
// expected-remark@-1 {{C typedef 'NonNilableReferences' imported as struct 'NonNilableReferences'}}


@protocol NSProtocolWithOptionalRequirement
// expected-remark@-1 {{Objective-C protocol 'NSProtocolWithOptionalRequirement' imported as protocol 'NSProtocolWithOptionalRequirement'}}
@optional
-(void)optionalRequirement;
// expected-remark@-1 {{Objective-C method '-[NSProtocolWithOptionalRequirement optionalRequirement]' imported as instance method 'optionalRequirement()'}}
-(DummyClass *)optionalRequirementMethodWithIUOResult;
// expected-remark@-1 {{Objective-C method '-[NSProtocolWithOptionalRequirement optionalRequirementMethodWithIUOResult]' imported as instance method 'optionalRequirementMethodWithIUOResult()'}}
@end

@interface NSClassWithMethodFromNSProtocolWithOptionalRequirement
// expected-remark@-1 {{Objective-C class interface 'NSClassWithMethodFromNSProtocolWithOptionalRequirement' imported as class 'NSClassWithMethodFromNSProtocolWithOptionalRequirement'}}
-(void)optionalRequirement  __attribute__((availability(macosx, introduced=10.51)));
// expected-remark@-1 {{Objective-C method '-[NSClassWithMethodFromNSProtocolWithOptionalRequirement optionalRequirement]' also imported as class method 'optionalRequirement()'}}
// expected-remark@-2 {{Objective-C method '-[NSClassWithMethodFromNSProtocolWithOptionalRequirement optionalRequirement]' imported as instance method 'optionalRequirement()'}}
@end

__attribute__((availability(macosx, introduced = 10.51)))
@interface AnnotatedFrameworkClass : NSObject
// expected-remark@-1 {{Objective-C class interface 'AnnotatedFrameworkClass' imported as class 'AnnotatedFrameworkClass'}}
@end

__attribute__((availability(macosx, introduced = 10.52)))
@interface AnnotatedLaterFrameworkClass : NSObject
// expected-remark@-1 {{Objective-C class interface 'AnnotatedLaterFrameworkClass' imported as class 'AnnotatedLaterFrameworkClass'}}
@end

/// Aaa.  UnannotatedFrameworkProtocol.  Bbb.
@protocol UnannotatedFrameworkProtocol
// expected-remark@-1 {{Objective-C protocol 'UnannotatedFrameworkProtocol' imported as protocol 'UnannotatedFrameworkProtocol'}}
- (void)doSomethingWithClass:(AnnotatedFrameworkClass *_Nullable)k;
// expected-remark@-1 {{Objective-C method '-[UnannotatedFrameworkProtocol doSomethingWithClass:]' imported as instance method 'doSomething(with:)'}}
- (void)doSomethingWithNonNullableClass:(AnnotatedFrameworkClass *_Nonnull)k;
// expected-remark@-1 {{Objective-C method '-[UnannotatedFrameworkProtocol doSomethingWithNonNullableClass:]' imported as instance method 'doSomething(withNonNullableClass:)'}}
- (void)doSomethingWithIUOClass:(AnnotatedFrameworkClass *_Null_unspecified)k;
// expected-remark@-1 {{Objective-C method '-[UnannotatedFrameworkProtocol doSomethingWithIUOClass:]' imported as instance method 'doSomething(withIUOClass:)'}}
- (AnnotatedFrameworkClass *_Nullable)returnSomething;
// expected-remark@-1 {{Objective-C method '-[UnannotatedFrameworkProtocol returnSomething]' imported as instance method 'returnSomething()'}}

-(void)noUnavailableTypesInSignature;
// expected-remark@-1 {{Objective-C method '-[UnannotatedFrameworkProtocol noUnavailableTypesInSignature]' imported as instance method 'noUnavailableTypesInSignature()'}}

- (void)doSomethingWithClass:(AnnotatedFrameworkClass *_Nonnull)k
               andLaterClass:(AnnotatedLaterFrameworkClass *_Nonnull)lk;
// expected-remark@-2 {{Objective-C method '-[UnannotatedFrameworkProtocol doSomethingWithClass:andLaterClass:]' imported as instance method 'doSomething(with:andLaterClass:)'}}

-(void)someMethodWithAvailability __attribute__((availability(macosx,introduced=10.53)));
// expected-remark@-1 {{Objective-C method '-[UnannotatedFrameworkProtocol someMethodWithAvailability]' imported as instance method 'someMethodWithAvailability()'}}

@property(nonnull) AnnotatedFrameworkClass *someProperty;
// FIXME: repeats expected-remark@-1 3 {{Objective-C method '-[UnannotatedFrameworkProtocol setSomeProperty:]' imported as setter for property 'someProperty'}}
// FIXME: repeats expected-remark@-2 3 {{Objective-C method '-[UnannotatedFrameworkProtocol someProperty]' imported as getter for property 'someProperty'}}
// expected-remark@-3 {{Objective-C property '-UnannotatedFrameworkProtocol.someProperty' imported as property 'someProperty'}}

@end

/// Aaa.  AnnotatedFrameworkProtocol.  Bbb.
__attribute__((availability(macosx, introduced = 10.9)))
@protocol AnnotatedFrameworkProtocol
// expected-remark@-1 {{Objective-C protocol 'AnnotatedFrameworkProtocol' imported as protocol 'AnnotatedFrameworkProtocol'}}
- (AnnotatedFrameworkClass * _Nullable) returnSomething;
// expected-remark@-1 {{Objective-C method '-[AnnotatedFrameworkProtocol returnSomething]' imported as instance method 'returnSomething()'}}
@end

/// Aaa.  FrameworkClassConformingToUnannotatedFrameworkProtocol.  Bbb.
@interface FrameworkClassConformingToUnannotatedFrameworkProtocol : NSObject<UnannotatedFrameworkProtocol>
// expected-remark@-1 {{Objective-C class interface 'FrameworkClassConformingToUnannotatedFrameworkProtocol' imported as class 'FrameworkClassConformingToUnannotatedFrameworkProtocol'}}
@end

/// Aaa.  LaterFrameworkClassConformingToUnannotatedFrameworkProtocol.  Bbb.
__attribute__((availability(macosx, introduced = 10.52)))
@interface LaterFrameworkClassConformingToUnannotatedFrameworkProtocol : NSObject<UnannotatedFrameworkProtocol>
// expected-remark@-1 {{Objective-C class interface 'LaterFrameworkClassConformingToUnannotatedFrameworkProtocol' imported as class 'LaterFrameworkClassConformingToUnannotatedFrameworkProtocol'}}
@end

/// Aaa.  LaterAnnotatedFrameworkProtocol.  Bbb.
__attribute__((availability(macosx, introduced = 10.52)))
@protocol LaterAnnotatedFrameworkProtocol
// expected-remark@-1 {{Objective-C protocol 'LaterAnnotatedFrameworkProtocol' imported as protocol 'LaterAnnotatedFrameworkProtocol'}}
- (AnnotatedFrameworkClass * _Nullable) returnSomething;
// expected-remark@-1 {{Objective-C method '-[LaterAnnotatedFrameworkProtocol returnSomething]' imported as instance method 'returnSomething()'}}
- (void)doSomethingWithClass:(AnnotatedFrameworkClass *_Nonnull)k
               andLaterClass:(AnnotatedLaterFrameworkClass *_Nonnull)lk;
// expected-remark@-2 {{Objective-C method '-[LaterAnnotatedFrameworkProtocol doSomethingWithClass:andLaterClass:]' imported as instance method 'doSomething(with:andLaterClass:)'}}
-(void)noUnavailableTypesInSignature;
// expected-remark@-1 {{Objective-C method '-[LaterAnnotatedFrameworkProtocol noUnavailableTypesInSignature]' imported as instance method 'noUnavailableTypesInSignature()'}}
-(void)someMethodWithAvailability __attribute__((availability(macosx,introduced=10.53)));
// expected-remark@-1 {{Objective-C method '-[LaterAnnotatedFrameworkProtocol someMethodWithAvailability]' imported as instance method 'someMethodWithAvailability()'}}
@property(nonnull) AnnotatedFrameworkClass *someProperty;
// FIXME: repeats expected-remark@-1 2 {{Objective-C method '-[LaterAnnotatedFrameworkProtocol setSomeProperty:]' imported as setter for property 'someProperty'}}
// FIXME: repeats expected-remark@-2 2 {{Objective-C method '-[LaterAnnotatedFrameworkProtocol someProperty]' imported as getter for property 'someProperty'}}
// expected-remark@-3 {{Objective-C property '-LaterAnnotatedFrameworkProtocol.someProperty' imported as property 'someProperty'}}
@end

/// Aaa.  FrameworkClassConformingToLaterAnnotatedFrameworkProtocol.  Bbb.
@interface FrameworkClassConformingToLaterAnnotatedFrameworkProtocol : NSObject<LaterAnnotatedFrameworkProtocol>
// expected-remark@-1 {{Objective-C class interface 'FrameworkClassConformingToLaterAnnotatedFrameworkProtocol' imported as class 'FrameworkClassConformingToLaterAnnotatedFrameworkProtocol'}}
@end

@interface UnusedResults : NSObject
// expected-remark@-1 {{Objective-C class interface 'UnusedResults' imported as class 'UnusedResults'}}
-(NSInteger)producesResult __attribute__((warn_unused_result));
// expected-remark@-1 {{Objective-C method '-[UnusedResults producesResult]' imported as instance method 'producesResult()'}}
@end

@interface NSObject (Silly)
// expected-remark@-1 {{Objective-C category 'NSObject(Silly)' imported as extension of 'NSObject'}}
-(void)doSelector:(SEL)selector;
// expected-remark@-1 {{Objective-C method '-[NSObject(Silly) doSelector:]' also imported as class method 'do'}}
// expected-remark@-2 {{Objective-C method '-[NSObject(Silly) doSelector:]' imported as instance method 'do'}}
@end

@interface Bee (Gerunds)
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
- (void)startSquashingBee:(nonnull Bee *)bee;
//  expected-FIXME-remark@-1 {{<#diagnostic#>}}
- (void)startSoothingBee:(nonnull Bee *)bee;
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
- (void)startShoppingBee:(nonnull Bee *)bee;
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
@end

@interface NSMutableArray<ObjectType> : NSArray
// expected-remark@-1 {{Objective-C class interface 'NSMutableArray' imported as class 'NSMutableArray'}}
- (void)addObjects:(nonnull NSArray<ObjectType> *)objects;
// expected-remark@-1 {{Objective-C method '-[NSMutableArray addObjects:]' imported as instance method 'add'}}
@end

@interface NSString (Slicing)
// expected-remark@-1 {{Objective-C category 'NSString(Slicing)' imported as extension of 'NSString'}}
- (nonnull NSString *)sliceFromIndex:(NSInteger)fromIndex toIndex:(NSInteger)toIndex;
// expected-remark@-1 {{Objective-C method '-[NSString(Slicing) sliceFromIndex:toIndex:]' imported as instance method 'slice(from:to:)'}}
@end

@interface NSString (Appending)
// expected-remark@-1 {{Objective-C category 'NSString(Appending)' imported as extension of 'NSString'}}
- (nonnull NSString *)stringByAppendingString:(nonnull NSString *)string;
// expected-remark@-1 {{Objective-C method '-[NSString(Appending) stringByAppendingString:]' imported as instance method 'appending'}}
- (nonnull NSString *)stringWithString:(nonnull NSString *)string;
// expected-remark@-1 {{Objective-C method '-[NSString(Appending) stringWithString:]' imported as instance method 'withString'}}
- (nullable NSURL *)URLWithAddedString:(nonnull NSString *)string;
// expected-remark@-1 {{Objective-C method '-[NSString(Appending) URLWithAddedString:]' imported as instance method 'url(withAddedString:)'}}

// Fake API for testing initialisms.
- (nullable NSGUID *)GUIDWithAddedString:(nonnull NSString *)string;
// expected-remark@-1 {{Objective-C method '-[NSString(Appending) GUIDWithAddedString:]' imported as instance method 'guid(withAddedString:)'}}
- (NSString *)stringForCalendarUnits:(NSCalendarUnit)units;
// expected-remark@-1 {{Objective-C method '-[NSString(Appending) stringForCalendarUnits:]' imported as instance method 'forCalendarUnits'}}
@end

@interface NSURL (Properties)
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
@property (readonly, nullable) NSURL *URLByDeletingLastPathComponent;
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
// expected-FIXME-remark@-2 {{<#diagnostic#>}}
@property (readonly, nonnull) NSURL *URLWithHTTPS;
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
// expected-FIXME-remark@-2 {{<#diagnostic#>}}
@end

@interface NSGUID (Properties)
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
@property (readonly, nullable) NSGUID *GUIDByCanonicalizing;
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
// expected-FIXME-remark@-2 {{<#diagnostic#>}}
@property (readonly, nonnull) NSGUID *GUIDWithContext;
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
// expected-FIXME-remark@-2 {{<#diagnostic#>}}
@end

typedef NS_OPTIONS(NSUInteger, NSEnumerationOptions) {
   // expected-remark@-1 {{C enum 'NSEnumerationOptions' imported as struct 'NSEnumerationOptions'}}
   // expected-remark@-2 {{C typedef 'NSEnumerationOptions' imported as struct 'NSEnumerationOptions'}}
   NSEnumerationConcurrent = (1UL << 0),
   // expected-remark@-1 {{C enum constant 'NSEnumerationConcurrent' imported as static property 'concurrent'}}
   NSEnumerationReverse = (1UL << 1),
   // expected-remark@-1 {{C enum constant 'NSEnumerationReverse' imported as static property 'reverse'}}
};

@interface NSArray (Enumeration)
// expected-remark@-1 {{Objective-C category 'NSArray(Enumeration)' imported as extension of 'NSArray'}}
- (void)enumerateObjectsUsingBlock:(void (^)(id obj,
                                             NSUInteger idx,
                                             BOOL *stop))block;
// expected-remark@-3 {{Objective-C method '-[NSArray(Enumeration) enumerateObjectsUsingBlock:]' imported as instance method 'enumerateObjects'}}

- (void)enumerateObjectsWithOptions:(NSEnumerationOptions)opts
                         usingBlock:(void (^)(id obj, NSUInteger idx,
                                              BOOL *stop))block;
// expected-remark@-3 {{Objective-C method '-[NSArray(Enumeration) enumerateObjectsWithOptions:usingBlock:]' imported as instance method 'enumerateObjects(options:using:)'}}

- (void)enumerateObjectsRandomlyWithBlock:
    (void (^_Nullable)(id obj, NSUInteger idx, BOOL *stop))block;
// expected-remark@-2 {{Objective-C method '-[NSArray(Enumeration) enumerateObjectsRandomlyWithBlock:]' imported as instance method 'enumerateObjectsRandomly(block:)'}}

- (void)enumerateObjectsHaphazardly:(void (^_Nullable)(id obj, NSUInteger idx,
                                                       BOOL *stop))block;
// expected-remark@-2 {{Objective-C method '-[NSArray(Enumeration) enumerateObjectsHaphazardly:]' imported as instance method 'enumerateObjectsHaphazardly'}}

- (void)optionallyEnumerateObjects:(NSEnumerationOptions)opts
                              body:(void (^)(id obj, NSUInteger idx,
                                             BOOL *stop))block;
// expected-remark@-3 {{Objective-C method '-[NSArray(Enumeration) optionallyEnumerateObjects:body:]' imported as instance method 'optionallyEnumerateObjects(_:body:)'}}

- (void)enumerateObjectsWhileOrderingPizza:(BOOL)pizza
                         withOptions:(NSEnumerationOptions)opts
                         usingBlock:(void (^)(id obj, NSUInteger idx,
                                              BOOL *stop))block;
// expected-remark@-4 {{Objective-C method '-[NSArray(Enumeration) enumerateObjectsWhileOrderingPizza:withOptions:usingBlock:]' imported as instance method 'enumerateObjectsWhileOrderingPizza(_:with:using:)'}}

- (void)doSomethingWithCopying:(nonnull id<NSCopying>)copying;
// expected-remark@-1 {{Objective-C method '-[NSArray(Enumeration) doSomethingWithCopying:]' imported as instance method 'doSomething(with:)'}}
- (void)doSomethingElseWithCopying:(nonnull NSObject<NSCopying> *)copying;
// expected-remark@-1 {{Objective-C method '-[NSArray(Enumeration) doSomethingElseWithCopying:]' imported as instance method 'doSomethingElse(with:)'}}

- (void)enumerateObjectsWithNullableBlock:
    (void (^_Nullable)(id obj, NSUInteger idx, BOOL *stop))block;
// expected-remark@-2 {{Objective-C method '-[NSArray(Enumeration) enumerateObjectsWithNullableBlock:]' imported as instance method 'enumerateObjects(nullableBlock:)'}}
@end

@interface NSMutableArray (Sorting)
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
- (void)sortUsingFunction:(NSInteger (*_Nonnull)(_Nonnull id,
                                                 _Nonnull id))fimctopn;
// expected-FIXME-remark@-2 {{<#diagnostic#>}}
@end

@interface NSMutableArray (Removal)
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
- (void)removeObjects:(nonnull NSArray *)objects;
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
@end

@interface NSMutableArray (TypeSuffix)
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
- (void)doSomethingWithUnderlying:(NSUnderlyingType)underlying;
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
- (void)setDefaultEnumerationOptions:(NSEnumerationOptions)options;
// expected-FIXME-remark@-1 {{<#diagnostic#>}}
@end

@interface NSString ()
// expected-remark@-1 {{Objective-C category 'NSString()' imported as extension of 'NSString'}}
- (nonnull NSString *)stringByNormalizingXMLPreservingComments:(BOOL)preserveFlag;
// expected-remark@-1 {{Objective-C method '-[NSString() stringByNormalizingXMLPreservingComments:]' imported as instance method 'normalizingXMLPreservingComments'}}
@end

@interface NSSet ()
// expected-remark@-1 {{Objective-C category 'NSSet()' imported as extension of 'NSSet'}}
- (nonnull NSSet *)setByAddingObject:(nonnull id)object;
// expected-remark@-1 {{Objective-C method '-[NSSet() setByAddingObject:]' imported as instance method 'adding'}}
@property (readonly) BOOL empty;
// expected-remark@-1 {{Objective-C method '-[NSSet() empty]' imported as getter for property 'empty'}}
// expected-remark@-2 {{Objective-C property '-NSSet().empty' imported as property 'empty'}}
- (BOOL)nonEmpty;
// expected-remark@-1 {{Objective-C method '-[NSSet() nonEmpty]' imported as instance method 'nonEmpty()'}}
@property (readonly) BOOL isStringSet;
// expected-remark@-1 {{Objective-C method '-[NSSet() isStringSet]' imported as getter for property 'isStringSet'}}
// expected-remark@-2 {{Objective-C property '-NSSet().isStringSet' imported as property 'isStringSet'}}
@property (readonly) BOOL wantsAUnion;
// expected-remark@-1 {{Objective-C method '-[NSSet() wantsAUnion]' imported as getter for property 'wantsAUnion'}}
// expected-remark@-2 {{Objective-C property '-NSSet().wantsAUnion' imported as property 'wantsAUnion'}}
@property (readonly) BOOL watchesItsLanguage;
// expected-remark@-1 {{Objective-C method '-[NSSet() watchesItsLanguage]' imported as getter for property 'watchesItsLanguage'}}
// expected-remark@-2 {{Objective-C property '-NSSet().watchesItsLanguage' imported as property 'watchesItsLanguage'}}
@property (readonly) BOOL appliesForAJob;
// expected-remark@-1 {{Objective-C method '-[NSSet() appliesForAJob]' imported as getter for property 'appliesForAJob'}}
// expected-remark@-2 {{Objective-C property '-NSSet().appliesForAJob' imported as property 'appliesForAJob'}}
@property (readonly) BOOL setShouldBeInfinite;
// expected-remark@-1 {{Objective-C method '-[NSSet() setShouldBeInfinite]' imported as getter for property 'setShouldBeInfinite'}}
// expected-remark@-2 {{Objective-C property '-NSSet().setShouldBeInfinite' imported as property 'setShouldBeInfinite'}}
@end

int variadicFunc1(int A, ...);
// expected-remark@-1 {{C function 'variadicFunc1' imported as unavailable global function 'variadicFunc1'}}

int variadicFunc2(int A, ...);
// expected-remark@-1 {{C function 'variadicFunc2' imported as unavailable global function 'variadicFunc2'}}

@interface NSString (UTF8)
// expected-remark@-1 {{Objective-C category 'NSString(UTF8)' imported as extension of 'NSString'}}
-(nullable instancetype)initWithUTF8String:(const char *)bytes;
// expected-remark@-1 {{Objective-C method '-[NSString(UTF8) initWithUTF8String:]' imported as initializer 'init(utf8String:)'}}
@end

extern NSString *NSGlobalConstant;
// expected-remark@-1 {{C variable 'NSGlobalConstant' imported as let 'NSGlobalConstant'}}
extern void NSGlobalFunction(void);
// expected-remark@-1 {{C function 'NSGlobalFunction' imported as global function 'NSGlobalFunction()'}}

extern void NS123(void);
// expected-remark@-1 {{C function 'NS123' imported as global function 'NS123()'}}
extern void NSYELLING(void);
// expected-remark@-1 {{C function 'NSYELLING' imported as global function 'NSYELLING()'}}
extern void NS_SCREAMING(void);
// expected-remark@-1 {{C function 'NS_SCREAMING' imported as global function 'NS_SCREAMING()'}}
extern void NS_(void);
// expected-remark@-1 {{C function 'NS_' imported as global function 'NS_()'}}
extern NSString *NSHTTPRequestKey;
// expected-remark@-1 {{C variable 'NSHTTPRequestKey' imported as let 'NSHTTPRequestKey'}}

@interface NSString (URLExtraction)
// expected-remark@-1 {{Objective-C category 'NSString(URLExtraction)' imported as extension of 'NSString'}}
@property (nonnull,copy,readonly) NSArray<NSURL *> *URLsInText;
// expected-remark@-1 {{Objective-C method '-[NSString(URLExtraction) URLsInText]' imported as getter for property 'urlsInText'}}
// expected-remark@-2 {{Objective-C property '-NSString(URLExtraction).URLsInText' imported as property 'urlsInText'}}
@property (nonnull,copy,readonly) NSArray<NSGUID *> *GUIDsInText;
// expected-remark@-1 {{Objective-C method '-[NSString(URLExtraction) GUIDsInText]' imported as getter for property 'guidsInText'}}
// expected-remark@-2 {{Objective-C property '-NSString(URLExtraction).GUIDsInText' imported as property 'guidsInText'}}
@end

@interface NSObject (Selectors)
// expected-remark@-1 {{Objective-C category 'NSObject(Selectors)' imported as extension of 'NSObject'}}
-(void)messageSomeObject:(nonnull id)object selector:(SEL)selector;
// expected-remark@-1 {{Objective-C method '-[NSObject(Selectors) messageSomeObject:selector:]' also imported as class method 'messageSomeObject(_:selector:)'}}
// expected-remark@-2 {{Objective-C method '-[NSObject(Selectors) messageSomeObject:selector:]' imported as instance method 'messageSomeObject(_:selector:)'}}
@end

@interface NSOperation : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSOperation' imported as class 'Operation'}}
@end

@interface NSProgress : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSProgress' imported as class 'NSProgress'}}
@end

@protocol NSProgressReporting <NSObject>
// expected-remark@-1 {{Objective-C protocol 'NSProgressReporting' imported as protocol 'ProgressReporting'}}
@property (readonly) NSProgress *progress;
// expected-remark@-1 {{Objective-C method '-[NSProgressReporting progress]' imported as getter for property 'progress'}}
// expected-remark@-2 {{Objective-C property '-NSProgressReporting.progress' imported as property 'progress'}}
@end

@interface NSIdLover: NSObject
// expected-remark@-1 {{Objective-C class interface 'NSIdLover' imported as class 'NSIdLover'}}

- (id _Nonnull)makesId;
// expected-remark@-1 {{Objective-C method '-[NSIdLover makesId]' imported as instance method 'makesId()'}}
- (void)takesId:(id _Nonnull)x;
// expected-remark@-1 {{Objective-C method '-[NSIdLover takesId:]' imported as instance method 'takesId'}}
- (void)takesArrayOfId:(const id _Nonnull * _Nonnull)x;
// expected-remark@-1 {{Objective-C method '-[NSIdLover takesArrayOfId:]' imported as instance method 'takesArray(ofId:)'}}
- (void)takesNullableId:(id _Nullable)x;
// expected-remark@-1 {{Objective-C method '-[NSIdLover takesNullableId:]' imported as instance method 'takesNullableId'}}

@property (strong) id propertyOfId;
// expected-remark@-1 {{Objective-C method '-[NSIdLover propertyOfId]' imported as getter for property 'propertyOfId'}}
// expected-remark@-2 {{Objective-C method '-[NSIdLover setPropertyOfId:]' imported as setter for property 'propertyOfId'}}
// expected-remark@-3{{Objective-C property '-NSIdLover.propertyOfId' imported as property 'propertyOfId'}}

@end

@protocol NSIdLoving
// expected-remark@-1 {{Objective-C protocol 'NSIdLoving' imported as protocol 'NSIdLoving'}}
- (void)takesIdViaProtocol:(id _Nonnull)x;
// expected-remark@-1 {{Objective-C method '-[NSIdLoving takesIdViaProtocol:]' imported as instance method 'takesId(viaProtocol:)'}}
@end

#define NSTimeIntervalSince1970 978307200.0
#define NS_DO_SOMETHING 17

typedef NS_ENUM(NSUInteger, NSClothingStyle) {
  // expected-remark@-1 {{C enum 'NSClothingStyle' imported as enum 'NSClothingStyle'}}
  // expected-remark@-2 {{C typedef 'NSClothingStyle' imported as enum 'NSClothingStyle'}}

  NSClothingStyleFormal = 0,
  // expected-remark@-1 {{C enum constant 'NSClothingStyleFormal' imported as enum case 'formal'}}
  NSClothingStyleSemiFormal,
  // expected-remark@-1 {{C enum constant 'NSClothingStyleSemiFormal' imported as enum case 'semiFormal'}}
  NSClothingStyleHipster,
  // expected-remark@-1 {{C enum constant 'NSClothingStyleHipster' imported as enum case 'hipster'}}
  NSClothingStyleHippie
  // expected-remark@-1 {{C enum constant 'NSClothingStyleHippie' imported as enum case 'hippie'}}
};
static const NSClothingStyle NSClothingStyleOfficeCasual __attribute__((availability(swift,unavailable,replacement="NSClothingStyleSemiFormal"))) = NSClothingStyleSemiFormal;
// expected-remark@-1 {{C variable 'NSClothingStyleOfficeCasual' imported as unavailable let 'NSClothingStyleOfficeCasual'}}

void acceptError(NSError * _Nonnull error);
// expected-remark@-1 {{C function 'acceptError' imported as global function 'acceptError'}}
NSError * _Nonnull produceError(void);
// expected-remark@-1 {{C function 'produceError' imported as global function 'produceError()'}}
NSError * _Nullable produceOptionalError(void);
// expected-remark@-1 {{C function 'produceOptionalError' imported as global function 'produceOptionalError()'}}

extern NSString * const FictionalServerErrorDomain;
// expected-remark@-1 {{C variable 'FictionalServerErrorDomain' imported as let 'FictionalServerErrorDomain'}}

typedef enum __attribute__((ns_error_domain(FictionalServerErrorDomain))) FictionalServerErrorCode : NSInteger {
  // expected-remark@-1 {{C enum 'FictionalServerErrorCode' also imported as struct 'FictionalServerError'}}
  // expected-remark@-2 {{C enum 'FictionalServerErrorCode' imported as enum 'Code'}}
  FictionalServerErrorMeltedDown = 1
  // expected-remark@-1 {{C enum constant 'FictionalServerErrorMeltedDown' also imported as static property 'meltedDown'}}
  // expected-remark@-2 {{C enum constant 'FictionalServerErrorMeltedDown' imported as enum case 'meltedDown'}}
} FictionalServerErrorCode;
// expected-remark@-1 {{C typedef 'FictionalServerErrorCode' imported as enum 'Code'}}

@protocol Wearable
// expected-remark@-1 {{Objective-C protocol 'Wearable' imported as protocol 'Wearable'}}
- (void)wear;
// expected-remark@-1 {{Objective-C method '-[Wearable wear]' imported as instance method 'wear()'}}
@end

@protocol Garment
// expected-remark@-1 {{Objective-C protocol 'Garment' imported as protocol 'Garment'}}
@end

@protocol Cotton
// expected-remark@-1 {{Objective-C protocol 'Cotton' imported as protocol 'Cotton'}}
@end

@interface Coat : NSObject<Wearable>
// expected-remark@-1 {{Objective-C class interface 'Coat' imported as class 'Coat'}}

- (void)wear;
// expected-remark@-1 {{Objective-C method '-[Coat wear]' imported as instance method 'wear()'}}
@property (class) Coat <Wearable> *fashionStatement;
// expected-remark@-1 {{Objective-C method '+[Coat fashionStatement]' imported as getter for class property 'fashionStatement'}}
// expected-remark@-2 {{Objective-C method '+[Coat setFashionStatement:]' imported as setter for class property 'fashionStatement'}}
// expected-remark@-3 {{Objective-C property '+Coat.fashionStatement' imported as class property 'fashionStatement'}}

@end

@protocol NSLaundry
// expected-remark@-1 {{Objective-C protocol 'NSLaundry' imported as protocol 'NSLaundry'}}
- (void)wash:(Coat <Garment> * _Nonnull)garment;
// expected-remark@-1 {{Objective-C method '-[NSLaundry wash:]' imported as instance method 'wash'}}
- (void)bleach:(Coat <Garment, Cotton> * _Nonnull)garment;
// expected-remark@-1 {{Objective-C method '-[NSLaundry bleach:]' imported as instance method 'bleach'}}
- (Coat <Garment> * _Nonnull)dry;
// expected-remark@-1 {{Objective-C method '-[NSLaundry dry]' imported as instance method 'dry()'}}
@end

@interface NSLaundromat : NSObject
// expected-remark@-1 {{Objective-C class interface 'NSLaundromat' imported as class 'NSLaundromat'}}
@end

extern NSString * const NSLaundryErrorDomain;
// expected-remark@-1 {{C variable 'NSLaundryErrorDomain' imported as let 'NSLaundryErrorDomain'}}

typedef enum __attribute__((ns_error_domain(NSLaundryErrorDomain))) __attribute__((swift_name("NSLaundromat.Error"))) NSLaundryErrorCode {
    // expected-remark@-1 {{C enum 'NSLaundryErrorCode' also imported as struct 'Error'}}
    // expected-remark@-2 {{C enum 'NSLaundryErrorCode' imported as enum 'Code'}}
    NSLaundryErrorTooMuchSoap = 1,
    // expected-remark@-1 {{C enum constant 'NSLaundryErrorTooMuchSoap' also imported as static property 'tooMuchSoap'}}
    // expected-remark@-2 {{C enum constant 'NSLaundryErrorTooMuchSoap' imported as enum case 'tooMuchSoap'}}
    NSLaundryErrorCatInWasher = 2
    // expected-remark@-1 {{C enum constant 'NSLaundryErrorCatInWasher' also imported as static property 'catInWasher'}}
    // expected-remark@-2 {{C enum constant 'NSLaundryErrorCatInWasher' imported as enum case 'catInWasher'}}
};

typedef void (*event_handler)(_Nonnull id);
// expected-remark@-1 {{C typedef 'event_handler' imported as type alias 'event_handler'}}
void install_global_event_handler(_Nullable event_handler handler);
// expected-remark@-1 {{C function 'install_global_event_handler' imported as global function 'install_global_event_handler'}}

@interface NSObject ()
// expected-remark@-1 {{Objective-C category 'NSObject()' imported as extension of 'NSObject'}}
- (void) addObserver: (id) observer
         forKeyPath: (NSString*) keyPath
         options: (NSInteger) options
         context: (void*) context;
// expected-remark@-4 {{Objective-C method '-[NSObject() addObserver:forKeyPath:options:context:]' also imported as class method 'addObserver(_:forKeyPath:options:context:)'}}
// expected-remark@-5 {{Objective-C method '-[NSObject() addObserver:forKeyPath:options:context:]' imported as instance method 'addObserver(_:forKeyPath:options:context:)'}}
- (void) removeObserver: (id) observer
         forKeyPath: (NSString*) keyPath
         context: (void*) options;
// expected-remark@-3 {{Objective-C method '-[NSObject() removeObserver:forKeyPath:context:]' also imported as class method 'removeObserver(_:forKeyPath:context:)'}}
// expected-remark@-4 {{Objective-C method '-[NSObject() removeObserver:forKeyPath:context:]' imported as instance method 'removeObserver(_:forKeyPath:context:)'}}
@end

_Nullable id returnNullableId(void);
// expected-remark@-1 {{C function 'returnNullableId' imported as global function 'returnNullableId()'}}
void takeNullableId(_Nullable id);
// expected-remark@-1 {{C function 'takeNullableId' imported as global function 'takeNullableId'}}

@interface I
// expected-remark@-1 {{Objective-C class interface 'I' imported as class 'I'}}
@end

@protocol OptionalRequirements
// expected-remark@-1 {{Objective-C protocol 'OptionalRequirements' imported as protocol 'OptionalRequirements'}}
@optional
- (Coat *)optional;
// expected-remark@-1 {{Objective-C method '-[OptionalRequirements optional]' imported as instance method 'optional()'}}
@property NSString *name;
// expected-remark@-1 {{Objective-C method '-[OptionalRequirements name]' imported as getter for property 'name'}}
// expected-remark@-2 {{Objective-C method '-[OptionalRequirements setName:]' imported as setter for property 'name'}}
// expected-remark@-3 {{Objective-C property '-OptionalRequirements.name' imported as property 'name'}}
@end

@interface IUOProperty
// expected-remark@-1 {{Objective-C class interface 'IUOProperty' imported as class 'IUOProperty'}}
@property (readonly) id<OptionalRequirements> iuo;
// expected-remark@-1 {{Objective-C method '-[IUOProperty iuo]' also imported as class method 'iuo()'}}
// expected-remark@-2 {{Objective-C method '-[IUOProperty iuo]' imported as getter for property 'iuo'}}
// expected-remark@-3 {{Objective-C property '-IUOProperty.iuo' imported as property 'iuo'}}
@end

@interface ColorDescriptor : NSObject <NSCopying>
// expected-remark@-1 {{Objective-C class interface 'ColorDescriptor' imported as class 'ColorDescriptor'}}
@end

@interface ColorArray : NSObject
// expected-remark@-1 {{Objective-C class interface 'ColorArray' imported as class 'ColorArray'}}
- (ColorDescriptor *)objectAtIndexedSubscript:(NSUInteger)attachmentIndex;
// expected-remark@-1 {{Objective-C method '-[ColorArray objectAtIndexedSubscript:]' imported as unavailable instance method 'objectAtIndexedSubscript'}}
- (void)setObject:(nullable ColorDescriptor *)attachment atIndexedSubscript:(NSUInteger)attachmentIndex;
// expected-remark@-1 {{Objective-C method '-[ColorArray setObject:atIndexedSubscript:]' imported as unavailable instance method 'setObject(_:atIndexedSubscript:)'}}
@end

@interface PaletteDescriptor : NSObject <NSCopying>
// expected-remark@-1 {{Objective-C class interface 'PaletteDescriptor' imported as class 'PaletteDescriptor'}}
@property (readonly, nonnull) ColorArray *colors;
// expected-remark@-1 {{Objective-C property '-PaletteDescriptor.colors' imported as property 'colors'}}
// expected-remark@-2 {{Objective-C method '-[PaletteDescriptor colors]' imported as getter for property 'colors'}}
@end
