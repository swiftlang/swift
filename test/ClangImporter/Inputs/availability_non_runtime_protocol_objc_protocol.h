#import <Foundation/Foundation.h>

@interface CustomObject : NSObject
@property(strong, nonatomic) id someProperty;
- (void)someMethod;
@end

__attribute__((objc_non_runtime_protocol))
@protocol ComposerMutationBridging
@end

__attribute__((objc_non_runtime_protocol))
@protocol ObjCNonRuntimeProtocolNotUsedinSwift
@end

@protocol ObjCRuntimeProtocolUsedinSwift
@end
