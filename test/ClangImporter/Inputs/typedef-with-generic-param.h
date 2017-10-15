#import <Foundation/Foundation.h>

@interface Foo <__covariant T>
typedef void (^CompletionBlock)(T _Nullable result, NSError *_Nullable error);
@end

@interface Foo <T> (Convenience)
+ (Foo<T> *)fooWithCompletionBlock: (void (^)(CompletionBlock adapter))block;
@end
