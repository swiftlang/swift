@import Foundation;

@protocol NSAppearanceCustomization <NSObject>
@end

@interface NSResponder : NSObject
@end

// Specifically testing re-adopting a protocol that's adopted by a base class.
@interface NSWindow : NSResponder <NSAppearanceCustomization>
@end
