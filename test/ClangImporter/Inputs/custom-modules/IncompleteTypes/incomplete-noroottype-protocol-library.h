#import <Foundation/Foundation.h>

@protocol NoRootTypeProtocol;

@interface NoRootTypeProtocolConsumer : NSObject
@property(strong) id<NoRootTypeProtocol>
    propertyUsingAForwardDeclaredNoRootTypeProtocol;
- (id)init;
- (id<NoRootTypeProtocol>)methodReturningForwardDeclaredNoRootTypeProtocol;
- (void)methodTakingAForwardDeclaredNoRootTypeProtocol:
    (id<NoRootTypeProtocol>)param;
@end

id<NoRootTypeProtocol> CFunctionReturningAForwardDeclaredNoRootTypeProtocol();
void CFunctionTakingAForwardDeclaredNoRootTypeProtocol(
    id<NoRootTypeProtocol> param);
