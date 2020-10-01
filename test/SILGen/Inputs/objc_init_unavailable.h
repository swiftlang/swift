@import Foundation;

@interface ClassWithUnavailableInit : NSObject

- (instancetype)initWithBundleID:(NSString *)bundleID __attribute__((availability(macos, unavailable)));
@end
