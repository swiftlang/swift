@import ObjectiveC;

@protocol IBActionInProtocol
-(void) __attribute__((ibaction)) actionMethod:(_Nullable id)param;
@end

@interface ConformsToIBActionInProtocol : NSObject <IBActionInProtocol>
@end

