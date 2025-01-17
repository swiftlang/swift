@import Categories_A;

@interface NSObject (BridgingHeader)
- (void)overridesCategoryMethodOnNSObject;
@end

@interface X (BridgingHeader)
- (void)fromBridgingHeader;
- (void)overridesCategoryMethodOnNSObject;
@end

struct StructInBridgingHeader {
  int member;
};

@interface ObjectInBridgingHeader : NSObject
- (void)overridesCategoryMethodOnNSObject;
@end
