@import objc;

typedef unsigned long NSUInteger;

@interface NSArray : NSObject
- (id)objectAtIndexedSubscript:(NSUInteger)idx;
@end

@interface Hive
@property __attribute__((iboutletcollection(B))) NSArray *bees;
@end
