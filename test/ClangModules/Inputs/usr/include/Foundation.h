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
