@import Foundation;

@class Foo;

@interface Foo : NSObject

@property (nullable, strong, nonatomic) NSDate *today;

- (void)selectDate:(nullable NSDate *)date;

@end

