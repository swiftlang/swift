#include <Foundation/Foundation.h>

@interface Butt: NSObject

- (instancetype _Nonnull)init;

- (void)butt:(NSInteger)x completionHandler:(void (^ _Nonnull)(NSInteger))handler;

@end

@interface Farm: NSObject

-(void)getDogWithCompletion:(void (^ _Nonnull)(NSInteger))completionHandler
  __attribute__((swift_async_name("getter:doggo()")));

-(void)obtainCat:(void (^ _Nonnull)(NSInteger, NSError* _Nullable))completionHandler
__attribute__((swift_async_name("getter:catto()")));

@end

void scheduleButt(Butt *b, NSString *s);
