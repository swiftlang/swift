#define __counted_by(x) __attribute__((__counted_by__(x)))

@interface SomeClass
- (int)numberOfSegments;
@end

void autoreleaseParam(SomeClass * __autoreleasing * __counted_by(len) p, int len); // expected-note{{declared here}}

SomeClass * __autoreleasing * __counted_by(len) autoreleaseReturn(int len);

@interface NSError
@end

@interface NSData
@end

@protocol Foo
- (void)errorAsTry:(int) len : (int * __counted_by(len)) p error:(NSError * *) error;
- (void)completionHandlerAsAsync:(int) len : (int * __counted_by(len)) p completionHandler:(void (^)(NSData * data, NSError *)) completionHandler;
@end
