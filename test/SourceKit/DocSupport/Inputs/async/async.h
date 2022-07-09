@import Foundation;

@interface AsyncImports : NSObject

-(void)methodWithCompletion:(void (^)(void))completionHandler;

-(void)propWithCompletion:(void (^)(BOOL))completionHandler
  __attribute__((swift_async_name("getter:asyncProp()")));

@end
