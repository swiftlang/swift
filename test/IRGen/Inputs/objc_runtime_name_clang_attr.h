@import Foundation;

__attribute__((objc_runtime_name("ObjCRuntimeNameIsDifferent")))
@interface ObjCRuntimeNamed: NSObject

@end

__attribute__((objc_runtime_name("ObjCProtoRuntimeNameIsDifferent")))
@protocol ObjCProtoRuntimeNamed

@end
