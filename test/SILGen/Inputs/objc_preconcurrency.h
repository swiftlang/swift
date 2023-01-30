@import Foundation;

#define SENDABLE __attribute__((__swift_attr__("@Sendable")))

SENDABLE
@interface NSTouchGrass : NSObject
@property (nullable, copy) void (SENDABLE ^cancellationHandler)(void);
@property (nonnull, copy) void (SENDABLE ^exceptionHandler)(void);
@end
