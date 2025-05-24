@import Foundation;
@import ctypes;

#pragma clang assume_nonnull begin

#define MAIN_ACTOR __attribute__((__swift_attr__("@MainActor")))
#define UI_ACTOR __attribute__((swift_attr("@UIActor")))

#ifdef __SWIFT_ATTR_SUPPORTS_SENDABLE_DECLS
  #define SENDABLE __attribute__((__swift_attr__("@Sendable")))
  #define NONSENDABLE __attribute__((__swift_attr__("@_nonSendable")))
  #define ASSUME_NONSENDABLE_BEGIN _Pragma("clang attribute ASSUME_NONSENDABLE.push (__attribute__((swift_attr(\"@_nonSendable(_assumed)\"))), apply_to = any(objc_interface, record, enum))")
  #define ASSUME_NONSENDABLE_END _Pragma("clang attribute ASSUME_NONSENDABLE.pop")
#else
  // If we take this #else, we should see minor failures of some subtests,
  // but not systematic failures of everything that uses this header.
  #define SENDABLE
  #define NONSENDABLE
  #define ASSUME_NONSENDABLE_BEGIN
  #define ASSUME_NONSENDABLE_END
#endif

#define NS_ENUM(_type, _name) enum _name : _type _name; \
  enum __attribute__((enum_extensibility(open))) _name : _type
#define NS_OPTIONS(_type, _name) enum _name : _type _name; \
  enum __attribute__((enum_extensibility(open), flag_enum)) _name : _type
#define NS_ERROR_ENUM(_type, _name, _domain)  \
  enum _name : _type _name; enum __attribute__((ns_error_domain(_domain))) _name : _type
#define NS_STRING_ENUM __attribute((swift_newtype(enum)))
#define NS_EXTENSIBLE_STRING_ENUM __attribute__((swift_wrapper(struct)))

typedef NSString *Flavor NS_EXTENSIBLE_STRING_ENUM;

@protocol ServiceProvider
@property(readonly) NSArray<NSString *> *allOperations;
-(void)allOperationsWithCompletionHandler:(void (^)(NSArray<NSString *> *))completion;
@end

typedef void (^CompletionHandler)(NSString * _Nullable, NSString * _Nullable_result, NSError * _Nullable) SENDABLE;
typedef void (^NonsendableCompletionHandler)(NSString * _Nullable, NSString * _Nullable_result, NSError * _Nullable);

@interface SlowServer : NSObject <ServiceProvider>
-(void)doSomethingSlow:(NSString *)operation completionHandler:(void (^)(NSInteger))handler;
-(void)doSomethingDangerous:(NSString *)operation completionHandler:(void (^ _Nullable)(NSString *_Nullable, NSError * _Nullable))handler;
-(void)doSomethingReckless:(NSString *)operation completionHandler:(void (^ _Nullable NONSENDABLE)(NSString *_Nullable, NSError * _Nullable))handler;
-(void)checkAvailabilityWithCompletionHandler:(void (^)(BOOL isAvailable))completionHandler;
-(void)anotherExampleWithCompletionBlock:(void (^)(NSString *))block;
-(void)finalExampleWithReplyTo:(void (^)(NSString *))block;
-(void)replyingOperation:(NSString *)operation replyTo:(void (^)(NSString *))block;
-(void)findAnswerAsynchronously:(void (^)(NSString *_Nullable, NSError * _Nullable))handler __attribute__((swift_name("findAnswer(completionHandler:)")));
-(BOOL)findAnswerFailinglyWithError:(NSError * _Nullable * _Nullable)error completion:(void (^)(NSString *_Nullable, NSError * _Nullable))handler __attribute__((swift_name("findAnswerFailingly(completionHandler:)")));
-(void)findQAndAWithCompletionHandler:(void (^)(NSString *_Nullable_result, NSString *_Nullable answer, NSError * _Nullable))handler;
-(void)findQuestionableAnswersWithCompletionHandler:(CompletionHandler)handler;
-(void)findAnswerableQuestionsWithCompletionHandler:(NonsendableCompletionHandler)handler;
-(void)findUnanswerableQuestionsWithCompletionHandler:(NONSENDABLE NonsendableCompletionHandler)handler;
-(void)doSomethingFun:(NSString *)operation then:(void (^)())completionHandler;
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

-(void)unavailableMethod __attribute__((__swift_attr__("@_unavailableFromAsync")));
-(void)unavailableMethodWithMessage __attribute__((__swift_attr__("@_unavailableFromAsync(message: \"Blarpy!\")")));

-(void)dance:(NSString *)step andThen:(void (^)(NSString *))doSomething __attribute__((swift_async(not_swift_private,2)));
-(void)leap:(NSInteger)height andThen:(void (^)(NSString *))doSomething __attribute__((swift_async(swift_private,2)));

-(void)repeatTrick:(NSString *)trick completionHandler:(void (^)(NSInteger))handler __attribute__((swift_async(none)));

-(void)doSomethingSlowNullably:(NSString *)operation completionHandler:(void (^ _Nullable)(NSInteger))handler;
-(void)findAnswerNullably:(NSString *)operation completionHandler:(void (^ _Nullable)(NSString *))handler;
-(void)doSomethingDangerousNullably:(NSString *)operation completionHandler:(void (^ _Nullable)(NSString *_Nullable, NSError *_Nullable))handler;
-(void)doSomethingUnspecifiedNullablyWithCompletionHandler:(void (^ _Nullable)(NSString *_Nullable, NSError *_Nullable))handler;

// rdar://72604599
- (void)stopRecordingWithHandler:(nullable void (^)(NSObject *_Nullable_result x, NSError *_Nullable error))handler __attribute__((swift_async_name("stopRecording()"))) __attribute__((swift_async(not_swift_private, 1)));

// rdar://73798726
- (void)getSomeObjectWithCompletionHandler:(nullable void (^)(NSObject *_Nullable x, NSError *_Nullable error))handler;

- (void)performVoid2VoidWithCompletion:(void (^ _Nonnull)(void (^ _Nonnull)(void)))completion;
- (void)performId2VoidWithCompletion:(void (^ _Nonnull)(void (^ _Nonnull)(id _Nonnull)))completion;
- (void)performId2IdWithCompletion:(void (^ _Nonnull)(id _Nonnull (^ _Nonnull)(id _Nonnull)))completion;
- (void)performNSString2NSStringWithCompletion:(void (^ _Nonnull)(NSString * _Nonnull (^ _Nonnull)(NSString * _Nonnull)))completion;
- (void)performNSString2NSStringNSStringWithCompletion:(void (^ _Nonnull)(NSString * _Nonnull (^ _Nonnull)(NSString * _Nonnull), NSString * _Nonnull))completion;
- (void)performId2VoidId2VoidWithCompletion:(void (^ _Nonnull)(void (^ _Nonnull)(id _Nonnull), void (^ _Nonnull)(id _Nonnull)))completion;

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

-(void)overridableButRunsOnMainThreadWithCompletionHandler:(MAIN_ACTOR void (^ _Nullable)(NSString *))completion;
- (void)obtainClosureWithCompletionHandler:(void (^)(void (^_Nullable)(void),
                                                     NSError *_Nullable,
                                                     BOOL))completionHandler
    __attribute__((swift_async_error(zero_argument, 3)));
- (void)getIceCreamFlavorWithCompletionHandler:
    (void (^)(Flavor flavor, NSError *__nullable error))completionHandler;

@property(class, strong, readonly) SlowServer *standardServer;
- (void)getValueWithKey:(NSString *)valueIdentifier
             completion:(void (^)(NSString *__nullable value,
                                  NSError *__nullable error))completionHandler;
- (void)getMainActorValueWithKey:(NSString *)valueIdentifier
                      completion:
                          (void (^)(NSString *__nullable value,
                                    NSError *__nullable error))completionHandler
    MAIN_ACTOR;
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
-(void)nonisolatedMethod __attribute__((__swift_attr__("nonisolated")));
-(void)mainActorMethod __attribute__((__swift_attr__("@MainActor")));
-(void)uiActorMethod __attribute__((__swift_attr__("@UIActor")));

@optional
-(void)missingAtAttributeMethod __attribute__((__swift_attr__("MainActor")));
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

@interface GenericObject<T> : NSObject // expected-note {{generic class 'GenericObject' does not conform to the 'Sendable' protocol}}
- (void)doSomethingWithCompletionHandler:(void (^)(T _Nullable_result, NSError * _Nullable))completionHandler;
- (void)doAnotherThingWithCompletionHandler:(void (^)(GenericObject<T> *_Nullable))completionHandler;
@end

#define MAGIC_NUMBER 42


__attribute__((__swift_attr__("@MainActor")))
@interface NXView : NSObject
-(void)onDisplay;
@end

@interface NXButton: NXView
-(void)onButtonPress;
@end

// Do something concurrently, but without escaping.
void doSomethingConcurrently(__attribute__((noescape)) SENDABLE void (^block)(void));



void doSomethingConcurrentlyButUnsafe(__attribute__((noescape)) __attribute__((swift_attr("@Sendable"))) void (^block)(void));


MAIN_ACTOR MAIN_ACTOR __attribute__((__swift_attr__("@MainActor"))) @protocol TripleMainActor
@end

@protocol ProtocolWithAsync
- (void)protocolMethodWithCompletionHandler:(void (^)(void))completionHandler;
- (void)customAsyncNameProtocolMethodWithCompletionHandler:(void (^)(void))completionHandler __attribute__((swift_async_name("customAsyncName()")));
@end

@interface ClassWithAsync: NSObject <ProtocolWithAsync>
- (void)instanceMethodWithCompletionHandler:(void (^)(void))completionHandler __attribute__((swift_async_name("instanceAsync()")));
@end

SENDABLE @interface SendableClass : NSObject @end

NONSENDABLE @interface NonSendableClass : NSObject @end // expected-note {{class 'NonSendableClass' does not conform to the 'Sendable' protocol}}

ASSUME_NONSENDABLE_BEGIN

SENDABLE @interface AuditedSendable : NSObject @end
@interface AuditedNonSendable : NSObject @end
NONSENDABLE SENDABLE @interface AuditedBoth : NSObject @end

SENDABLE @protocol SendableProtocol @end
@protocol SendableProtocolRefined <SendableProtocol> @end

typedef NS_ENUM(unsigned, SendableEnum) {
  SendableEnumFoo, SendableEnumBar
};
typedef NS_ENUM(unsigned, NonSendableEnum) { // expected-note {{enum 'NonSendableEnum' does not conform to the 'Sendable' protocol}}
  NonSendableEnumFoo, NonSendableEnumBar
} NONSENDABLE;

typedef NS_OPTIONS(unsigned, SendableOptions) {
  SendableOptionsFoo = 1 << 0, SendableOptionsBar = 1 << 1
};
typedef NS_OPTIONS(unsigned, NonSendableOptions) { // expected-note {{struct 'NonSendableOptions' does not conform to the 'Sendable' protocol}}
  NonSendableOptionsFoo = 1 << 0, NonSendableOptionsBar = 1 << 1
} NONSENDABLE;

NSString *SendableErrorDomain, *NonSendableErrorDomain;
typedef NS_ERROR_ENUM(unsigned, SendableErrorCode, SendableErrorDomain) {
  SendableErrorCodeFoo, SendableErrorCodeBar
};
typedef NS_ERROR_ENUM(unsigned, NonSendableErrorCode, NonSendableErrorDomain) {
  NonSendableErrorCodeFoo, NonSendableErrorCodeBar
} NONSENDABLE;
// expected-warning@-3 {{cannot make error code type 'NonSendableErrorCode' non-Sendable because Swift errors are always sendable}}

UI_ACTOR
@interface PictureFrame : NSObject
- (instancetype)initWithSize:(NSInteger)frame NS_DESIGNATED_INITIALIZER;
- (void)rotate;
@end

@interface NotIsolatedPictureFrame : NSObject
- (instancetype)initWithSize:(NSInteger)frame NS_DESIGNATED_INITIALIZER;
- (void)rotate;
@end

typedef NSString *SendableStringEnum NS_STRING_ENUM;
typedef NSString *NonSendableStringEnum NS_STRING_ENUM NONSENDABLE; // expected-note {{struct 'NonSendableStringEnum' does not conform to the 'Sendable' protocol}}

typedef NSString *SendableStringStruct NS_EXTENSIBLE_STRING_ENUM;
typedef NSString *NonSendableStringStruct NS_EXTENSIBLE_STRING_ENUM NONSENDABLE; // expected-note {{struct 'NonSendableStringStruct' does not conform to the 'Sendable' protocol}}

SENDABLE
typedef struct {
  void *ptr;
} SendableStructWithNonSendable;

ASSUME_NONSENDABLE_END

typedef id ObjectTypedef;
typedef void(^BlockTypedef)(id);

@interface NXSender : NSObject

- (id)sendAny:(SENDABLE id)obj SENDABLE;
- (nullable id)sendOptionalAny:(nullable SENDABLE id)obj SENDABLE;
- (SendableClass *)sendSendable:(SENDABLE SendableClass *)sendable SENDABLE;
- (NonSendableClass *)sendSendableSubclasses:(SENDABLE NonSendableClass *)sendableSubclass SENDABLE;
- (id <LabellyProtocol>)sendProto:(SENDABLE id <LabellyProtocol>)obj SENDABLE;
- (id <LabellyProtocol, ObjCClub>)sendProtos:(SENDABLE id <LabellyProtocol, ObjCClub>)obj SENDABLE;
- (NSArray<id> *)sendAnyArray:(SENDABLE NSArray<id> *)array SENDABLE;
- (GenericObject<SendableClass *> *)sendGeneric:(SENDABLE GenericObject<SendableClass *> *)generic SENDABLE;
- (void *)sendPtr:(SENDABLE void *)val SENDABLE;    // bad
- (NSArray<NSString *> *)sendStringArray:(SENDABLE NSArray<NSString *> *)obj SENDABLE;    // bad
- (ObjectTypedef)sendAnyTypedef:(SENDABLE ObjectTypedef)obj SENDABLE;
- (NSArray<ObjectTypedef> *)sendAnyTypedefs:(SENDABLE NSArray<ObjectTypedef> *)objs SENDABLE;
- (BlockTypedef)sendBlockTypedef:(SENDABLE BlockTypedef)block SENDABLE;
- (NSArray<BlockTypedef> *)sendBlockTypedefs:(SENDABLE NSArray<BlockTypedef> *)blocks SENDABLE;
- (NSArray *)sendUnbound:(SENDABLE NSArray *)array SENDABLE;

@property (strong) SENDABLE id sendableProp;

@end

SENDABLE id NXSendFunc(SENDABLE id arg);
SENDABLE id NXSendGlobal;

struct StructWithSendableContents {
  __unsafe_unretained SENDABLE id sendableField;
  union {
    __unsafe_unretained SENDABLE id sendableIndirectField;
  };
};

SENDABLE id StructWithSendableContentsGetSendableComputed(struct StructWithSendableContents contents)
  __attribute__((swift_name("getter:StructWithSendableContents.sendableComputed(self:)")));

@interface CostcoManager : NSObject
+ (instancetype)sharedManager;
- (void)isCustomerEnrolledInExecutiveProgram:(NSObject *)customer completion:(void(^)(BOOL enrolled))completion;
@end

@interface Person : NSObject
+ (void)getAsCustomer:(void(^_Nonnull)(NSObject *device))completion;
@end


// rdar://97646309
UI_ACTOR
@protocol CoffeeDelegate <NSObject>
@optional
- (void)icedMochaService:(NSObject *)mochaService generateMochaWithCompletion:(void (^)(NSObject *_Nullable ingredient1, NSObject *ingredient2, NSObject *ingredient3))completionHandler;
@end

MAIN_ACTOR
@interface MyView : NSObject
- (void)display;
@property(readonly) BOOL isVisible;
@end

// rdar://114049646
MAIN_ACTOR
@protocol HotdogCompetitor
- (nullable NSString *)pileOfHotdogsToEatWithLimit:(NSObject *)limit
                                                   error:(NSError * __autoreleasing *)error;
@end

@protocol Loadable
- (void)loadStuffWithIdentifier:(NSInteger)identifier reply:(void (^)())reply;
- (void)loadStuffWithOtherIdentifier:(NSInteger)otherIdentifier reply:(void (^)())reply;
- (void)loadStuffWithGroupID:(NSInteger)groupID reply:(void (^)())reply;
@end

@interface ImplementsLoadable : NSObject
- (void)loadStuffWithIdentifier:(NSInteger)identifier reply:(void (^)())reply;
- (void)loadStuffWithGroupID:(NSInteger)groupID reply:(void (^)())reply;
@end

@protocol DictionaryLoader
- (void)loadDictionaryWithCompletionHandler:(void (^)(NSDictionary <NSString *, NSNumber *> * _Nullable))completionHandler;
@end

@protocol FloatLoader
@optional
- (void)loadFloatWithCompletionHandler:(void (^)(float))completionHandler;
@end

@protocol FailableFloatLoader
- (void)loadFloatOrThrowWithCompletionHandler:(void (^)(float, NSError* __nullable)) completionHandler;
@end

#pragma clang assume_nonnull end
