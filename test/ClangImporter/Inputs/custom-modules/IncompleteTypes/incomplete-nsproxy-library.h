#import <Foundation/Foundation.h>

@class ForwardDeclaredNSProxyInterface;

@interface NSProxyConsumer : NSObject
@property ForwardDeclaredNSProxyInterface
    *propertyUsingAForwardDeclaredNSProxyInterface;
- (id)init;
- (ForwardDeclaredNSProxyInterface *)
    methodReturningForwardDeclaredNSProxyInterface;
- (void)methodTakingAForwardDeclaredNSProxyInterface:
    (ForwardDeclaredNSProxyInterface *)param;
@end

ForwardDeclaredNSProxyInterface *
CFunctionReturningAForwardDeclaredNSProxyInterface();
void CFunctionTakingAForwardDeclaredNSProxyInterface(
    ForwardDeclaredNSProxyInterface *param);
