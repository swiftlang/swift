@import Foundation;

@interface OBJCGeneric<T> : NSObject
@end

@interface OBJCGenericSubclass<T, U>: OBJCGeneric<T>
@end

@interface OBJCNongenericSubclass: OBJCGenericSubclass<id, id>
@end

@protocol OBJCProtocol1
@end

@protocol OBJCProtocol2
@end

@protocol OBJCProtocol3
@end
