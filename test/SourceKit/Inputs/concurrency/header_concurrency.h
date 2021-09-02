@import Foundation;

@interface ClassWithHandlerMethod
-(void)methodWithHandler:(NSString *)operation completionHandler:(void (^)(NSInteger))handler;
@end
