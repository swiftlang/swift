#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface ImplsMissingInit : NSObject
- (instancetype)initWithValue:(int)value;
@end

@interface ImplsHasInit : NSObject
- (instancetype)initWithValue:(int)value;
@end

@interface ImplsUnavailableInit : NSObject
- (instancetype)initWithValue:(int)value;
+ (instancetype)new NS_UNAVAILABLE;
- (instancetype)init NS_UNAVAILABLE;
@end

NS_ASSUME_NONNULL_END
