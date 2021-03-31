@import Foundation;

#pragma clang assume_nonnull begin

typedef void (^CompletionHandler)(NSInteger);

@interface HandlerTest : NSObject
-(void)simpleWithCompletionHandler:(void (^)(NSInteger))handler;
-(void)simpleArg:(NSInteger)arg completionHandler:(void (^)(NSInteger))handler;
-(void)aliasWithCompletionHandler:(CompletionHandler)handler;
-(void)errorWithCompletionHandler:(void (^)(NSString *_Nullable, NSError * _Nullable))handler;
-(BOOL)removedError:(NSError * _Nullable * _Nullable)error completionHandler:(void (^)(NSString *_Nullable, NSError * _Nullable))handler;

-(void)asyncImportSame:(NSInteger)arg completionHandler:(void (^)(NSInteger))handler;
-(void)asyncImportSame:(NSInteger)arg replyTo:(void (^)(NSInteger))handler __attribute__((swift_async(none)));
@end

#pragma clang assume_nonnull end
