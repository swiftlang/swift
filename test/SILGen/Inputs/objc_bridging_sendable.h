@import Foundation;

@interface NSBlah: NSObject
- (void) takeSendable: (id __attribute__((swift_attr("@Sendable")))) x;
@end
