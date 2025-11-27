@import Foundation;

@interface GenericClass<T> : NSObject
- (instancetype)initWithValue:(T)value;
@end

@interface AnotherGenericClass<T> : NSObject
- (instancetype)initWithValue:(T)value;
@end
