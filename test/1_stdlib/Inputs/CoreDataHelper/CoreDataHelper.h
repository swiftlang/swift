@import CoreData;

NS_ASSUME_NONNULL_BEGIN

@interface NSFetchRequest<__covariant ResultType: id<NSFetchRequestResult>> (SwiftTesting)
+ (NSArray<ResultType> *)testGettingSomeDictionaries;
@end

NS_ASSUME_NONNULL_END
