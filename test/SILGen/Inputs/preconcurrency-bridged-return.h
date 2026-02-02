@import Foundation;

#define NS_SWIFT_SENDABLE __attribute__((__swift_attr__("@Sendable")))

@interface TestClass: NSObject

- (_Nullable id NS_SWIFT_SENDABLE)foo;

@end
