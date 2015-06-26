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

#ifndef SWIFT_CLASS_NAMED
#  define SWIFT_CLASS_NAMED(SWIFT_NAME) \
    __attribute__((swift_name(SWIFT_NAME))) SWIFT_CLASS_EXTRA
#endif

#ifndef SWIFT_PROTOCOL_NAMED
#  define SWIFT_PROTOCOL_NAMED(SWIFT_NAME) \
    __attribute__((swift_name(SWIFT_NAME))) SWIFT_PROTOCOL_EXTRA
#endif

SWIFT_CLASS("SwiftClass")
__attribute__((objc_root_class))
@interface SwiftClass
@end

@interface SwiftClass (Category)
- (void)categoryMethod:(struct PureClangType)arg;
@end

SWIFT_PROTOCOL_NAMED("CustomNameType")
@protocol SwiftProtoWithCustomName
@end

SWIFT_CLASS_NAMED("CustomNameClass")
__attribute__((objc_root_class))
@interface SwiftClassWithCustomName <SwiftProtoWithCustomName>
@end
  
id <SwiftProtoWithCustomName> __nonnull convertToProto(SwiftClassWithCustomName * __nonnull obj);


SWIFT_CLASS("BOGUS")
@interface BogusClass
@end
