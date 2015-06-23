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
@end

@interface ReallyErrorProne : ErrorProne
@end

@protocol ErrorProneProtocol
- (BOOL) obliterate: (NSError**) error;
- (BOOL) invigorate: (NSError**) error callback: (void(^)(void)) block;
@end
