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
-(void)getFortuneAsynchronouslyWithCompletionHandler:(void (^)(NSString *_Nullable, NSError * _Nullable))handler;
-(void)getMagicNumberAsynchronouslyWithSeed:(NSInteger)seed completionHandler:(void (^)(NSInteger, NSError * _Nullable))handler;
@property(readwrite) void (^completionHandler)(NSInteger);

-(NSString *)magicNameWithSeed:(NSInteger)seed;
-(void)magicNameAsynchronouslyWithSeed:(NSInteger)seed completionHandler:(void (^)(NSString * _Nullable, NSError * _Nullable))handler;

-(NSString * _Nullable)boringNameWithSeed:(NSInteger)seed error:(NSError **)error;
-(void)boringNameAsynchronouslyWithSeed:(NSInteger)seed completionHandler:(void (^)(NSString * _Nullable, NSError * _Nullable))handler;

-(NSString *)confusingName;
-(void)confusingNameAsynchronouslyWithCompletionHandler:(void (^)(NSString * _Nullable, NSError * _Nullable))handler;

-(NSString *)amazingName:(NSInteger)integer;
-(void)amazingNameAsynchronouslyWithCompletionHandler:(void (^)(NSString * _Nullable, NSError * _Nullable))handler;

-(void)terrifyingNameWithCompletionHandler:(void (^)(NSString * _Nullable, NSError * _Nullable))handler;

-(void)doSomethingConflicted:(NSString *)operation completionHandler:(void (^)(NSInteger))handler;
-(NSInteger)doSomethingConflicted:(NSString *)operation;
-(void)server:(NSString *)name restartWithCompletionHandler:(void (^)(void))block;
-(void)server:(NSString *)name atPriority:(double)priority restartWithCompletionHandler:(void (^)(void))block;
@end

@protocol RefrigeratorDelegate<NSObject>
- (void)someoneDidOpenRefrigerator:(id)fridge;
- (void)refrigerator:(id)fridge didGetFilledWithItems:(NSArray *)items;
- (void)refrigerator:(id)fridge didGetFilledWithIntegers:(NSInteger *)items count:(NSInteger)count;
- (void)refrigerator:(id)fridge willAddItem:(id)item;
- (BOOL)refrigerator:(id)fridge didRemoveItem:(id)item;
@end

@protocol ConcurrentProtocol
-(void)askUserToSolvePuzzle:(NSString *)puzzle completionHandler:(void (^)(NSString * _Nullable, NSError * _Nullable))completionHandler;

@optional
-(void)askUserToJumpThroughHoop:(NSString *)hoop completionHandler:(void (^)(NSString *))completionHandler;
@end

#pragma clang assume_nonnull end
