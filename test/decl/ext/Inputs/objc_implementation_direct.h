#import <Foundation/Foundation.h>

@interface DirectImplClass : NSObject

- (void)plainMethod;
- (void)anotherPlainMethod;
- (int)directMethod __attribute__((objc_direct));
- (int)anotherDirectMethod __attribute__((objc_direct));

@end
