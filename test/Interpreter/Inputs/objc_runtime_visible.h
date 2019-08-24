@import Foundation;

__attribute__((visibility("hidden")))
__attribute__((objc_runtime_visible))
@interface HiddenClass : NSObject
@property (readonly, nonnull) NSString *name;
@end

HiddenClass * _Nonnull createHidden() NS_RETURNS_RETAINED;
