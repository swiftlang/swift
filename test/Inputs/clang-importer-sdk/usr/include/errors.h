/* -*- ObjC -*- */

@import Foundation;

@interface ErrorProne : NSObject
+ (BOOL) fail: (NSError**) error;
+ (BOOL) goAndReturnError: (NSError**) error;
+ (BOOL) tryAndReturnError: (NSError**) error;

+ (BOOL) messUpSignatureAndReturnError: (NSError*) error;

+ (BOOL) consume: (id) object error: (NSError**) error;

- (instancetype) initWithNewtonMessagePad: (NSString *) assistant;
- (instancetype) initWithNewtonMessagePad: (NSString *) assistant error: (NSError**) error;

- (instancetype) initWithOne: (nullable id) other error: (NSError**) error;
+ (instancetype) errorProneWithTwo: (nullable id) other error: (NSError**) error;

- (BOOL) conflict1;
- (BOOL) conflict1: (NSError**) error;

- (BOOL) conflict2;
- (BOOL) conflict2AndReturnError: (NSError**) error;

- (BOOL) conflict3: (id) object;
- (BOOL) conflict3: (id) object error: (NSError**) error;

+ (NSArray*) collectionWithCount: (NSInteger) i error: (NSError**) error;

+ (BOOL) runWithError: (NSError**) err callback: (void(^)(void)) block;
+ (BOOL) runWithError: (NSError**) err count: (NSInteger) n;

+ (BOOL) runWithAnError: (NSError**) err callback: (void(^)(void)) block;
+ (BOOL) runWithAnError: (NSError**) err count: (NSInteger) n;

+ (BOOL) runSwiftly: (NSInteger) speed error: (NSError**) err callback: (void(^)(void)) block;

- (BOOL) integrate: (NSInteger) spec code: (NSInteger) code error: (NSError**) err;

+ (void)doTheThing:(NSInteger)whichThing __attribute__((swift, unavailable));
+ (BOOL)doTheThing:(NSInteger)whichThing error:(NSError **)error;

+ (BOOL) bound: (NSError**) err __attribute__((swift_error(nonnull_error)));
+ (float) bounce: (NSError**) err __attribute__((swift_error(nonnull_error)));
+ (void) flounce: (NSError**) err __attribute__((swift_error(nonnull_error)));
+ (int) ounce: (NSError**) err __attribute__((swift_error(zero_result)));
+ (NSInteger) ounceWord: (NSError**) err __attribute__((swift_error(zero_result)));
+ (int) once: (NSError**) err __attribute__((swift_error(nonzero_result)));
+ (BOOL) sconce: (NSError**) err __attribute__((swift_error(zero_result)));
+ (BOOL) scotch: (NSError**) err __attribute__((swift_error(nonzero_result)));
+ (BOOL) scout: (NSError**) err __attribute__((swift_error(none)));
@end

@interface ReallyErrorProne : ErrorProne
@end

@protocol ErrorProneProtocol
- (BOOL) obliterate: (NSError**) error;
- (BOOL) invigorate: (NSError**) error callback: (void(^)(void)) block;
@end


@interface SensibleErrorBase: NSObject
- (BOOL)performRiskyOperationAndReturnError:(NSError **)error;
- (nullable NSObject *)produceRiskyOutputAndReturnError:(NSError **)error;
- (nullable NSString *)produceRiskyStringAndReturnError:(NSError **)error;

- (nullable NSObject *)badNullResult:(NSError **)err __attribute__((swift_error(null_result)));
- (nullable NSObject *)badNullResult2:(NSError **)err __attribute__((swift_error(null_result)));
- (int)badZeroResult:(NSError **)err __attribute__((swift_error(zero_result)));
- (int)badNonzeroResult:(NSError **)err __attribute__((swift_error(nonzero_result)));
@end

@interface FoolishErrorSub : SensibleErrorBase
// This is invalid, but Swift shouldn't crash when it sees it.
- (void)performRiskyOperationAndReturnError:(NSError **)error;
// This should technically be valid in Objective-C as a covariant return.
- (nonnull FoolishErrorSub *)produceRiskyOutputAndReturnError:(NSError **)error;
- (nonnull NSString *)produceRiskyStringAndReturnError:(NSError **)error;

- (void)badNullResult:(NSError **)err;
- (int)badNullResult2:(NSError **)err;
- (void)badZeroResult:(NSError **)err;
- (void)badNonzeroResult:(NSError **)err;
@end
