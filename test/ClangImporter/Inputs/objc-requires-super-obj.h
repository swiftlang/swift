@import Foundation;

@interface MyCustomViewController : NSObject
- (void)customViewDidLoad __attribute((objc_requires_super));
- (nonnull instancetype)init __attribute((objc_designated_initializer))
__attribute((objc_requires_super));
- (void)dealloc __attribute((objc_requires_super));
@end