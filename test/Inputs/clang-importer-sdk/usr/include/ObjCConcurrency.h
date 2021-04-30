@import Foundation;
@import ctypes;

#pragma clang assume_nonnull begin

#define MAIN_ACTOR __attribute__((__swift_attr__("@MainActor")))
#define MAIN_ACTOR_UNSAFE __attribute__((__swift_attr__("@_unsafeMainActor")))

@protocol ServiceProvider
@property(readonly) NSArray<NSString *> *allOperations;
-(void)allOperationsWithCompletionHandler:(void (^)(NSArray<NSString *> *))completion;
@end

typedef void (^CompletionHandler)(NSString * _Nullable, NSString * _Nullable_result, NSError * _Nullable);

@interface SlowServer : NSObject <ServiceProvider>
-(void)doSomethingSlow:(NSString *)operation completionHandler:(void (^)(NSInteger))handler;
-(void)doSomethingDangerous:(NSString *)operation completionHandler:(void (^ _Nullable)(NSString *_Nullable, NSError * _Nullable))handler;
-(void)checkAvailabilityWithCompletionHandler:(void (^)(BOOL isAvailable))completionHandler;
-(void)anotherExampleWithCompletionBlock:(void (^)(NSString *))block;
-(void)finalExampleWithReplyTo:(void (^)(NSString *))block;
-(void)replyingOperation:(NSString *)operation replyTo:(void (^)(NSString *))block;
-(void)findAnswerAsynchronously:(void (^)(NSString *_Nullable, NSError * _Nullable))handler __attribute__((swift_name("findAnswer(completionHandler:)")));
-(BOOL)findAnswerFailinglyWithError:(NSError * _Nullable * _Nullable)error completion:(void (^)(NSString *_Nullable, NSError * _Nullable))handler __attribute__((swift_name("findAnswerFailingly(completionHandler:)")));
-(void)findQAndAWithCompletionHandler:(void (^)(NSString *_Nullable_result, NSString *_Nullable answer, NSError * _Nullable))handler;
-(void)findQuestionableAnswersWithCompletionHandler:(CompletionHandler)handler;
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

-(void)poorlyNamed:(NSString *)operation completionHandler:(void (^)(NSInteger))handler __attribute__((swift_async_name("bestName(_:)")));

-(void)customizedWithString:(NSString *)operation completionHandler:(void (^)(NSInteger))handler __attribute__((swift_name("customize(with:completionHandler:)"))) __attribute__((swift_async_name("customize(_:)")));

-(void)dance:(NSString *)step andThen:(void (^)(NSString *))doSomething __attribute__((swift_async(not_swift_private,2)));
-(void)leap:(NSInteger)height andThen:(void (^)(NSString *))doSomething __attribute__((swift_async(swift_private,2)));

-(void)repeatTrick:(NSString *)trick completionHandler:(void (^)(NSInteger))handler __attribute__((swift_async(none)));

-(void)doSomethingSlowNullably:(NSString *)operation completionHandler:(void (^ _Nullable)(NSInteger))handler;
-(void)findAnswerNullably:(NSString *)operation completionHandler:(void (^ _Nullable)(NSString *))handler;
-(void)doSomethingDangerousNullably:(NSString *)operation completionHandler:(void (^ _Nullable)(NSString *_Nullable, NSError *_Nullable))handler;

// rdar://72604599
- (void)stopRecordingWithHandler:(nullable void (^)(NSObject *_Nullable_result x, NSError *_Nullable error))handler __attribute__((swift_async_name("stopRecording()"))) __attribute__((swift_async(not_swift_private, 1)));

// rdar://73798726
- (void)getSomeObjectWithCompletionHandler:(nullable void (^)(NSObject *_Nullable x, NSError *_Nullable error))handler;

-(void)oldAPIWithCompletionHandler:(void (^ _Nonnull)(NSString *_Nullable, NSError *_Nullable))handler __attribute__((availability(macosx, deprecated=10.14)));

-(void)someAsyncMethodWithBlock:(void (^ _Nonnull)(NSString *_Nullable, NSError *_Nullable))completionHandler;

// Property & async method overloading
-(void)getOperationsWithCompletionHandler:(void (^)(NSArray<NSString *> *))handler;

@property (readonly, nonatomic) NSArray<NSString *> *operations;

-(void)doSomethingFlaggyWithCompletionHandler:(void (^)(BOOL, NSString *_Nullable, NSError *_Nullable))completionHandler __attribute__((swift_async_error(nonzero_argument, 1)));
-(void)doSomethingZeroFlaggyWithCompletionHandler:(void (^)(NSString *_Nullable, BOOL, NSError *_Nullable))completionHandler __attribute__((swift_async_error(zero_argument, 2)));
-(void)doSomethingMultiResultFlaggyWithCompletionHandler:(void (^)(BOOL, NSString *_Nullable, NSError *_Nullable, NSString *_Nullable))completionHandler __attribute__((swift_async_error(zero_argument, 1)));

-(void)runOnMainThreadWithCompletionHandler:(MAIN_ACTOR void (^ _Nullable)(NSString *))completion;

// Both would be imported as the same decl - require swift_async(none) on one
-(void)asyncImportSame:(NSString *)operation completionHandler:(void (^)(NSInteger))handler;
-(void)asyncImportSame:(NSString *)operation replyTo:(void (^)(NSInteger))handler __attribute__((swift_async(none)));

-(void)overridableButRunsOnMainThreadWithCompletionHandler:(MAIN_ACTOR_UNSAFE void (^ _Nullable)(NSString *))completion;
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

@protocol ProtocolWithSwiftAttributes
-(void)independentMethod __attribute__((__swift_attr__("@actorIndependent")));
-(void)asyncHandlerMethod __attribute__((__swift_attr__("@asyncHandler")));
-(void)mainActorMethod __attribute__((__swift_attr__("@MainActor")));
-(void)uiActorMethod __attribute__((__swift_attr__("@UIActor")));

@optional
-(void)missingAtAttributeMethod __attribute__((__swift_attr__("asyncHandler")));
@end

@protocol OptionalObserver <NSObject>
@optional
- (void)hello:(NSObject *)session completion:(void (^)(BOOL answer))completion;
- (BOOL)hello:(NSObject *)session;
@end

@protocol RequiredObserverOnlyCompletion <NSObject>
- (void)hello:(void (^)(BOOL answer))completion;
@end

@protocol RequiredObserver <RequiredObserverOnlyCompletion>
- (BOOL)hello;
@end

@protocol Rollable <NSObject>
- (void)rollWithCompletionHandler: (void (^)(void))completionHandler;
@end

typedef void ( ^ObjCErrorHandler )( NSError * _Nullable inError );

@protocol ObjCClub
- (void) activateWithCompletion:(ObjCErrorHandler) inCompletion;
@end

@protocol LabellyProtocol
  - (void) myMethod:(NSInteger)value1 newFoo:(NSInteger)value2 completion:(ObjCErrorHandler)completion;
  - (void) myMethod:(NSInteger)value1 foo:(NSInteger)value2;
@end

@interface GenericObject<T> : NSObject
- (void)doSomethingWithCompletionHandler:(void (^)(T _Nullable_result, NSError * _Nullable))completionHandler;
- (void)doAnotherThingWithCompletionHandler:(void (^)(GenericObject<T> *_Nullable))completionHandler;
@end

#define MAGIC_NUMBER 42


__attribute__((__swift_attr__("@MainActor(unsafe)")))
@interface NXView : NSObject
-(void)onDisplay;
@end

@interface NXButton: NXView
-(void)onButtonPress;
@end

// Do something concurrently, but without escaping.
void doSomethingConcurrently(__attribute__((noescape)) __attribute__((swift_attr("@Sendable"))) void (^block)(void));



void doSomethingConcurrentlyButUnsafe(__attribute__((noescape)) __attribute__((swift_attr("@_unsafeSendable"))) void (^block)(void));


MAIN_ACTOR MAIN_ACTOR __attribute__((__swift_attr__("@MainActor(unsafe)"))) @protocol TripleMainActor
@end

@protocol ProtocolWithAsync
- (void)protocolMethodWithCompletionHandler:(void (^)(void))completionHandler;
- (void)customAsyncNameProtocolMethodWithCompletionHandler:(void (^)(void))completionHandler __attribute__((swift_async_name("customAsyncName()")));
@end

@interface ClassWithAsync: NSObject <ProtocolWithAsync>
- (void)instanceMethodWithCompletionHandler:(void (^)(void))completionHandler __attribute__((swift_async_name("instanceAsync()")));
@end

#pragma clang assume_nonnull end
