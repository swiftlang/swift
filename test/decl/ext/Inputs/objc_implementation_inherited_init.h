#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface ImplsMissingInit : NSObject
- (instancetype)initWithValue:(int)value;
@end

@interface ImplsHasInit : NSObject
- (instancetype)initWithValue:(int)value;
@end

NS_ASSUME_NONNULL_END
