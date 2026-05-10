#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface ImplClass : NSObject

- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithInt:(NSInteger)x;

@end

@interface Rdar114874429 : NSObject

- (instancetype)init;

@property (readonly) NSInteger prop;

@end

NS_ASSUME_NONNULL_END
