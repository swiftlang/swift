@import ObjectiveC;

@class NSArray;

@interface Predicate : NSObject
+ (nonnull Predicate *)truePredicate;
+ (nonnull Predicate *)not;
+ (nonnull Predicate *)and:(nonnull NSArray *)subpredicates;
+ (nonnull Predicate *)or:(nonnull NSArray *)subpredicates;
@end
