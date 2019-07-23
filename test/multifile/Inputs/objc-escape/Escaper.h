#import <Foundation/Foundation.h>

@interface Escaper : NSObject
@property void (^escape)(void);
- (id) init;
@end
