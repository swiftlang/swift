#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@class ImplClassWithResilientStoredProperty;

@interface ImplClass : NSObject

- (instancetype)init;

@property (assign) NSInteger implProperty;
@property (assign) NSInteger defaultIntProperty;

+ (void)runTests;
+ (ImplClassWithResilientStoredProperty *)makeResilientImpl;

- (void)testSelf;
- (void)printSelfWithLabel:(int)label;
- (nonnull NSString *)someMethod;

@end

@interface ImplClassWithResilientStoredProperty : NSObject

- (void)printSelfWithLabel:(int)label;
- (void)mutate;

@end

extern void implFunc(int param);

NS_ASSUME_NONNULL_END
