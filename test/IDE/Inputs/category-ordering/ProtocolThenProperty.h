__attribute__((objc_root_class))
@interface Base
@end

@protocol Foo
@property (readonly) id foo;
@end

@interface Sub : Base
@end

@interface Sub (Cat1) <Foo>
@end

@interface Sub (Cat2)
@property id foo;
@end
