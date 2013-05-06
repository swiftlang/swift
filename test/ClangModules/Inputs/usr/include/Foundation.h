@import ObjectiveC;

typedef struct objc_object { void *isa; } *id;

typedef unsigned long NSUInteger;

@interface NSArray : NSObject
- (id)objectAtIndexedSubscript:(NSUInteger)idx;
- description;
@end

@interface NSString : NSObject
- (void)onlyOnNSString;
@end

NSString *NSStringToNSString(NSString *str);

@interface Hive
@property __attribute__((iboutletcollection(B))) NSArray *bees;
@end
