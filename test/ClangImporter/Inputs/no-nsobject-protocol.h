// Define our own NSObject interface, but not the protocol.
@interface NSObject
  - nsobjectFunc;
@end

@protocol FooProtocol
  - fooFunc;
@end

typedef NSObject<FooProtocol> Foo;

@interface Bar : Foo
- (instancetype)init;
- (NSObject<FooProtocol> *)barFunc;
@end
