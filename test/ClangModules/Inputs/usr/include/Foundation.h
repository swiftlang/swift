@import objc;

@interface NSArray : NSObject
- (id)objectAtIndexedSubscript:(unsigned)idx;
@end

@interface Hive
@property __attribute__((iboutletcollection(B))) NSArray *bees;
@end
