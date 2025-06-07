#define __counted_by(x) __attribute__((__counted_by__(x)))

@interface SomeClass
- (int)numberOfSegments;
@end

void autoreleaseParam(SomeClass * __autoreleasing * __counted_by(len) p, int len); // expected-note{{declared here}}

SomeClass * __autoreleasing * __counted_by(len) autoreleaseReturn(int len);
