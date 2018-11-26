@import Foundation;

@interface Foo<A>: NSObject

- (void)blockInception:(void (^ _Nonnull)(void (^ _Nonnull)(void (^ _Nonnull)(Foo<A> * _Nonnull))))b;

@end
