@import Foundation;

@interface ClassWithBlockProperty : NSObject
@property (readwrite, nullable) void (^block)(NSString * _Nullable string);
@property (readwrite, nullable) void (^dependentBlock)(NSString * _Nullable string);
@end
