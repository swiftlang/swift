@import Foundation;

@interface HasProperties
@property (getter=isEnabled,setter=setIsEnabled:) BOOL enabled __attribute__((swift_name("enabled")));

- (BOOL)isEnabled;
- (void)setIsEnabled:(BOOL)enabled;
@end
