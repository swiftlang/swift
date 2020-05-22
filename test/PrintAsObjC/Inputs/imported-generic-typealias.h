@interface NSObject
- (void) init;
@end;

@interface Horse<T> : NSObject
@end

@interface Barn : NSObject
@end

typedef int Hay __attribute__((swift_name("Horse.Hay")));
