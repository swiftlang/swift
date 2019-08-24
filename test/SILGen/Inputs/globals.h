@import Foundation;

extern NSString *globalString;
extern NSObject *globalObject;
extern id globalId;
extern NSArray *const globalConstArray;
extern NSArray *globalArray;

@interface Globals : NSObject

+ (instancetype)sharedInstance;

@end
