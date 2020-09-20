@import Foundation;
@import ctypes;

#pragma clang assume_nonnull begin

@interface SlowServer : NSObject
-(void)doSomethingSlow:(NSString *)operation completionHandler:(void (^)(NSInteger))handler;
-(void)doSomethingDangerous:(NSString *)operation completionHandler:(void (^ _Nullable)(NSString *_Nullable, NSError * _Nullable))handler;
-(void)checkAvailabilityWithCompletionHandler:(void (^)(BOOL isAvailable))completionHandler;
-(void)findAnswerAsynchronously:(void (^)(NSString *_Nullable, NSError * _Nullable))handler __attribute__((swift_name("findAnswer(completionHandler:)")));
-(BOOL)findAnswerFailinglyWithError:(NSError * _Nullable * _Nullable)error completion:(void (^)(NSString *_Nullable, NSError * _Nullable))handler __attribute__((swift_name("findAnswerFailingly(completionHandler:)")));
-(void)doSomethingFun:(NSString *)operation then:(void (^)(void))completionHandler;
@property(readwrite) void (^completionHandler)(NSInteger);
@end

#pragma clang assume_nonnull end
