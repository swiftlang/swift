@import Foundation;

@interface MyIntermediateClass : NSObject
- (nonnull instancetype)initWithDouble:(double)value NS_DESIGNATED_INITIALIZER;
@end

@interface MyGenericClass<__covariant T> : MyIntermediateClass
- (nonnull instancetype)initWithValue:(nonnull T)value;
@end

