@import Foundation;

@interface Test : NSObject
@property (nonnull, readonly) NSArray *nonnullArray;
@property (nullable, readonly) NSArray *nullableArray;
@property (null_unspecified, readonly) NSArray *nullUnspecifiedArray;

@property (nonnull, readonly) NSDictionary *nonnullDictionary;
@property (nonnull, readonly) NSSet *nonnullSet;

// Subscripts still have thunks wrapped around them.
- (nonnull NSArray *)objectAtIndexedSubscript:(long)index;
@end
