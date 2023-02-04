@import Foundation;

@protocol P
@end

@interface C: NSObject
@end

@interface Base<T> : NSObject
@end

@interface Derived: Base
@end

@interface BaseWithProtocol<T: id<P>> : NSObject
@end

@interface DerivedWithProtocol: BaseWithProtocol
@end

@interface BaseWithClass<T: C *> : NSObject
@end

@interface DerivedWithClass: BaseWithClass
@end

@interface BaseWithBoth<T: C<P> *> : NSObject
@end

@interface DerivedWithBoth: BaseWithBoth
@end
