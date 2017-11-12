@import Foundation;

@protocol SomeProto
@end

@interface Base: NSObject
#if USE_PROTO
<SomeProto>
#endif

@end
