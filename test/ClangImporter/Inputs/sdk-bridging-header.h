@import ObjectiveC;

@class NSArray;

@interface MyPredicate : NSObject {
  int kind : 2;    // Should not cause crash in PCH processing (rdar://85173321)
}

+ (nonnull MyPredicate *)truePredicate;
+ (nonnull MyPredicate *)not;
+ (nonnull MyPredicate *)and:(nonnull NSArray *)subpredicates;
+ (nonnull MyPredicate *)or:(nonnull NSArray *)subpredicates;
@end
