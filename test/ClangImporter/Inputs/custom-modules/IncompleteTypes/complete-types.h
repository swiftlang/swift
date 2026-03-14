#import <Foundation/Foundation.h>

@interface ForwardDeclaredInterface : NSObject
- (id)init;
- (void)doSomethingForwardDeclaredInterfacesCan;
@end

@protocol ForwardDeclaredProtocol <NSObject>
- (void)doSomethingForwardDeclaredProtocolsCan;
@end

void takeACompleteInterface(ForwardDeclaredInterface *param);
void takeACompleteProtocol(NSObject<ForwardDeclaredProtocol> *param);
