@import ObjectiveC;

#define SWIFT_SENDABLE __attribute__((swift_attr("@Sendable")))
#define SWIFT_SENDING __attribute__((swift_attr("sending")))

SWIFT_SENDABLE
@protocol ObjCHandlerWithSending <NSObject>
- (void)handle:(NSObject * SWIFT_SENDING)value;

- (NSObject* SWIFT_SENDING)identity:(NSObject * SWIFT_SENDING)value;
@end
