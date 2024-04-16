@import Categories_A;

@interface X (BridgingHeader)
- (void)fromBridgingHeader;
@end

struct StructInBridgingHeader {
  int member;
};
