@import ObjectiveC;

@class NSArray;

@interface Predicate : NSObject
+ (Predicate *)truePredicate;
+ (Predicate *)not;
+ (Predicate *)and:(NSArray *)subpredicates;
+ (Predicate *)or:(NSArray *)subpredicates;
@end