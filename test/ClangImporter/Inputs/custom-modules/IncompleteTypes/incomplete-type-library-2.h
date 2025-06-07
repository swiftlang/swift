#import <Foundation/Foundation.h>

@class ForwardDeclaredInterface;
@protocol ForwardDeclaredProtocol;

@interface IncompleteTypeConsumer2 : NSObject
@property id<ForwardDeclaredProtocol> propertyUsingAForwardDeclaredProtocol2;
@property ForwardDeclaredInterface *propertyUsingAForwardDeclaredInterface2;
- (id)init;
- (NSObject<ForwardDeclaredProtocol> *)methodReturningForwardDeclaredProtocol2;
- (ForwardDeclaredInterface *)methodReturningForwardDeclaredInterface2;
- (void)methodTakingAForwardDeclaredProtocol2:
    (id<ForwardDeclaredProtocol>)param;
- (void)methodTakingAForwardDeclaredInterface2:
    (ForwardDeclaredInterface *)param;
@end

ForwardDeclaredInterface *CFunctionReturningAForwardDeclaredInterface2();
void CFunctionTakingAForwardDeclaredInterface2(ForwardDeclaredInterface *param);

NSObject<ForwardDeclaredProtocol> *
CFunctionReturningAForwardDeclaredProtocol2();
void CFunctionTakingAForwardDeclaredProtocol2(
    id<ForwardDeclaredProtocol> param);
