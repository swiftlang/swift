@import Foundation;

@interface Base: NSObject
- (instancetype)initWithCustomField:(NSInteger)value NS_DESIGNATED_INITIALIZER;
- (instancetype)initConvenience;
@end

@interface Sub: Base
- (instancetype)initWithCustomField:(NSInteger)value NS_DESIGNATED_INITIALIZER;
@end
