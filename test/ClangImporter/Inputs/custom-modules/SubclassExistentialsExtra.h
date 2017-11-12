@import Foundation;

@interface SomeSpecificSubclass : NSObject
@end

typedef NSObject <NSCopying> CopyableNSObjectBase;
typedef SomeSpecificSubclass <NSCopying> CopyableSpecificBase;

@interface CompositionSubObject : CopyableNSObjectBase
@end
@interface CompositionSubSpecific : CopyableSpecificBase
@end
