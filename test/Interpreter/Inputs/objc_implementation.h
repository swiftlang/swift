#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface ImplClass : NSObject

- (instancetype)init;

@property (assign) NSInteger implProperty;
@property (assign) NSInteger defaultIntProperty;

+ (void)runTests;
- (nonnull NSString *)someMethod;

@end

extern void implFunc(int param);

NS_ASSUME_NONNULL_END
