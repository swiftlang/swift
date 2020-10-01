#import <Foundation/Foundation.h>

@interface SharedInterface
@end

@protocol SharedProtocol <NSObject>
+ (NSInteger)foo;
@end

// ClangImporter imports the first category as the extension parent of
// SharedInterface.foo, making foo unavailable on macOS when the extension
// unavailability is inherited by its members.
API_UNAVAILABLE(macos)
@interface SharedInterface (IOSCategory) <SharedProtocol>
@end

API_UNAVAILABLE(ios)
@interface SharedInterface (MacOSCategory) <SharedProtocol>
@end
