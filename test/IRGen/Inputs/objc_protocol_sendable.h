#import <Foundation/Foundation.h>

@interface A: NSObject
- (id <NSObject>)foo __attribute__((swift_attr("@Sendable")));
@end
