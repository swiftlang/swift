@import Foundation;

@interface HasProperties
@property (getter=isEnabled,setter=setIsEnabled:) BOOL enabled;

- (BOOL)isEnabled;
- (void)setIsEnabled:(BOOL)enabled;
@end
