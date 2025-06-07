#import <Foundation/Foundation.h>

@class ForwardDeclaredInterface;
@protocol ForwardDeclaredProtocol;

@interface IncompleteTypeConsumer1 : NSObject
@property id<ForwardDeclaredProtocol> propertyUsingAForwardDeclaredProtocol1;
@property ForwardDeclaredInterface *propertyUsingAForwardDeclaredInterface1;
- (id)init;
- (NSObject<ForwardDeclaredProtocol> *)methodReturningForwardDeclaredProtocol1;
- (ForwardDeclaredInterface *)methodReturningForwardDeclaredInterface1;
- (void)methodTakingAForwardDeclaredProtocol1:
    (id<ForwardDeclaredProtocol>)param;
- (void)methodTakingAForwardDeclaredInterface1:
    (ForwardDeclaredInterface *)param;
@end

ForwardDeclaredInterface *CFunctionReturningAForwardDeclaredInterface1();
void CFunctionTakingAForwardDeclaredInterface1(ForwardDeclaredInterface *param);

NSObject<ForwardDeclaredProtocol> *
CFunctionReturningAForwardDeclaredProtocol1();
void CFunctionTakingAForwardDeclaredProtocol1(
    id<ForwardDeclaredProtocol> param);
