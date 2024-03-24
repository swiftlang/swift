
// Make sure that we have set the -D flag appropriately.
#ifdef __SWIFT_ATTR_SUPPORTS_TRANSFERRING
#if !__SWIFT_ATTR_SUPPORTS_TRANSFERRING
#error "Compiler should have set __SWIFT_ATTR_SUPPORTS_TRANSFERRING to 1"
#endif
#else
#error "Compiler should have defined __SWIFT_ATTR_SUPPORTS_TRANSFERRING"
#endif

#define SWIFT_TRANSFERRING __attribute__((swift_attr("transferring")))

#pragma clang assume_nonnull begin

#ifdef __OBJC__

@import Foundation;

@interface MyType : NSObject
- (NSObject *)getTransferringResult SWIFT_TRANSFERRING;
- (NSObject *)getTransferringResultWithArgument:(NSObject *)arg SWIFT_TRANSFERRING;
- (NSObject *)getResultWithTransferringArgument:(NSObject *) SWIFT_TRANSFERRING arg;
@end

SWIFT_TRANSFERRING
@interface DoesntMakeSense : NSObject
@end

NSObject *returnNSObjectFromGlobalFunction(NSObject *other);
NSObject *transferNSObjectFromGlobalFunction(NSObject *other) SWIFT_TRANSFERRING;
void transferNSObjectToGlobalFunction(NSObject * arg SWIFT_TRANSFERRING);

#endif

typedef struct {
  int state;
} NonSendableCStruct;

NonSendableCStruct returnUserDefinedFromGlobalFunction(NonSendableCStruct other);
NonSendableCStruct transferUserDefinedFromGlobalFunction(NonSendableCStruct other) SWIFT_TRANSFERRING;
void transferUserDefinedIntoGlobalFunction(NonSendableCStruct arg SWIFT_TRANSFERRING);

#pragma clang assume_nonnull end
