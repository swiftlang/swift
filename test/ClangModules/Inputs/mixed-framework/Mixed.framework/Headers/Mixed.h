struct PureClangType {
  int x;
  int y;
};

#ifndef SWIFT_CLASS
#define SWIFT_CLASS
#endif

SWIFT_CLASS
__attribute__((objc_root_class))
@interface SwiftClass
@end

@interface SwiftClass (Category)
- (void)categoryMethod:(struct PureClangType)arg;
@end
