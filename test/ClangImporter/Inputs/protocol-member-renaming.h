@import ObjectiveC;

@interface Foo : NSObject
@end

@protocol FooDelegate
- (void)foo:(Foo *)foo willConsumeObject:(id)obj __attribute__((swift_name("foo(_:willConsume:)")));
@end

@protocol OptionalButUnavailable
@optional
- (void)doTheThing:(id)thingToDoItWith __attribute__((unavailable));
@end
