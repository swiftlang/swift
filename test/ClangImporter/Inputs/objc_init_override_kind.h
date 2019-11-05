#import <Foundation.h>

@interface Base : NSObject

- (instancetype) init;

- (instancetype) initWithFoo: (int) x NS_DESIGNATED_INITIALIZER;
@end
