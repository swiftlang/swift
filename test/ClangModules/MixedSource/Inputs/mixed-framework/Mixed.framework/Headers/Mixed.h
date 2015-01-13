struct PureClangType {
  int x;
  int y;
};

#ifndef SWIFT_CLASS_EXTRA
#  define SWIFT_CLASS_EXTRA
#endif

#ifndef SWIFT_CLASS
#  define SWIFT_CLASS(SWIFT_NAME) SWIFT_CLASS_EXTRA
#endif

SWIFT_CLASS("SwiftClass")
__attribute__((objc_root_class))
@interface SwiftClass
@end

@interface SwiftClass (Category)
- (void)categoryMethod:(struct PureClangType)arg;
@end


SWIFT_CLASS("BOGUS")
@interface BogusClass
@end
