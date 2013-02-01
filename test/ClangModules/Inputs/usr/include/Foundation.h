@import ObjectiveC;

typedef struct objc_object { void *isa; } *id;

typedef unsigned long NSUInteger;

@interface NSArray : NSObject
- (id)objectAtIndexedSubscript:(NSUInteger)idx;
@end

@interface NSString : NSObject
@end


@interface Hive
@property __attribute__((iboutletcollection(B))) NSArray *bees;
@end
