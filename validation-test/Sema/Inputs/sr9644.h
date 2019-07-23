@protocol TestProto
@property (nonatomic, readonly, getter=getFoo) int foo __attribute__((swift_name("swiftFoo")));
@property (nonatomic, getter=getTheBar, setter=setTheBar:) int bar __attribute__((swift_name("swiftBar")));
@end
