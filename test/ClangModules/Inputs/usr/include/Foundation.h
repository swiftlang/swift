@import ObjectiveC;

typedef struct objc_object { void *isa; } *id;

typedef unsigned long NSUInteger;

@class NSString;

@interface NSArray : NSObject
- (id)objectAtIndexedSubscript:(NSUInteger)idx;
- description;

@property NSString *nsstringProperty;

@end

@interface NSString : NSObject
- (void)onlyOnNSString;
@end

NSString *NSStringToNSString(NSString *str);

@interface Hive
@property __attribute__((iboutletcollection(B))) NSArray *bees;
@end
