#import <Foundation/Foundation.h>

@protocol PowerProtocol <NSObject>
@property (nonatomic, getter=getPower, setter=setPower:) int64_t level;
@end
