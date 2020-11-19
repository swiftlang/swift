@import Foundation;
@import ctypes;

#pragma clang assume_nonnull begin

@protocol ServiceProvider
@property(readonly) NSArray<NSString *> *allOperations;
-(void)allOperationsWithCompletionHandler:(void (^)(NSArray<NSString *> *))completion;
@end

@interface SlowServer : NSObject <ServiceProvider>
-(void)doSomethingSlow:(NSString *)operation completionHandler:(void (^)(NSInteger))handler;
-(void)doSomethingDangerous:(NSString *)operation completionHandler:(void (^ _Nullable)(NSString *_Nullable, NSError * _Nullable))handler;
-(void)checkAvailabilityWithCompletionHandler:(void (^)(BOOL isAvailable))completionHandler;
-(void)findAnswerAsynchronously:(void (^)(NSString *_Nullable, NSError * _Nullable))handler __attribute__((swift_name("findAnswer(completionHandler:)")));
-(BOOL)findAnswerFailinglyWithError:(NSError * _Nullable * _Nullable)error completion:(void (^)(NSString *_Nullable, NSError * _Nullable))handler __attribute__((swift_name("findAnswerFailingly(completionHandler:)")));
-(void)doSomethingFun:(NSString *)operation then:(void (^)(void))completionHandler;
-(void)getFortuneAsynchronouslyWithCompletionHandler:(void (^)(NSString *_Nullable, NSError * _Nullable))handler;
-(void)getMagicNumberAsynchronouslyWithSeed:(NSInteger)seed completionHandler:(void (^)(NSInteger, NSError * _Nullable))handler;
@property(readwrite) void (^completionHandler)(NSInteger);

-(void)findMultipleAnswersWithCompletionHandler:(void (^)(NSString *_Nullable, NSInteger, NSError * _Nullable))handler __attribute__((swift_name("findMultipleAnswers(completionHandler:)")));

-(void)findDifferentlyFlavoredBooleansWithCompletionHandler:(void (^)(BOOL wholeMilk, _Bool onePercent, NSError *_Nullable))handler __attribute__((swift_name("findDifferentlyFlavoredBooleans(completionHandler:)")));

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
-(void)askUserToSolvePuzzle:(NSString *)puzzle completionHandler:(void (^ _Nullable)(NSString * _Nullable, NSError * _Nullable))completionHandler;

@optional
-(void)askUserToJumpThroughHoop:(NSString *)hoop completionHandler:(void (^ _Nullable)(NSString *))completionHandler;
@end

#pragma clang assume_nonnull end
