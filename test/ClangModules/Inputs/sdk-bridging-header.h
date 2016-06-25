@import ObjectiveC;

@class NSArray;

@interface MyPredicate : NSObject
+ (nonnull MyPredicate *)truePredicate;
+ (nonnull MyPredicate *)not;
+ (nonnull MyPredicate *)and:(nonnull NSArray *)subpredicates;
+ (nonnull MyPredicate *)or:(nonnull NSArray *)subpredicates;
@end
