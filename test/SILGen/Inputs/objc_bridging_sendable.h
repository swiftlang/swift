@import Foundation;

@interface NSBlah: NSObject
- (void) takeSendable: (id __attribute__((swift_attr("@Sendable")))) x;
@property(readonly) id __attribute__((swift_attr("@Sendable"))) x;
- (nullable __attribute__((swift_attr("@Sendable"))) id)test:(NSError *_Nullable __autoreleasing * _Nullable)error;
- (nullable __attribute__((swift_attr("@Sendable"))) id)sendableNullableNSObject;
- (nullable id)regularNullableNSObject;
- (nonnull id)regularNSObject;
@end