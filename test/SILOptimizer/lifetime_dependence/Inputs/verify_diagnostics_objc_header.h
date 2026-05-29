#import <Foundation/Foundation.h>

@interface TakeNoEscapeBlock : NSObject
+ (instancetype)takeNoEscapeBlock:(void(NS_NOESCAPE ^)(void))block;
@end
