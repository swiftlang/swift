// Manual PrintAsObjC for testing purposes.

struct PureClangType {
  int x;
  int y;
};

#ifndef SWIFT_CLASS_EXTRA
#  define SWIFT_CLASS_EXTRA
#endif
#ifndef SWIFT_PROTOCOL_EXTRA
#  define SWIFT_PROTOCOL_EXTRA
#endif

#ifndef SWIFT_CLASS
#  define SWIFT_CLASS(SWIFT_NAME) SWIFT_CLASS_EXTRA
#endif

#ifndef SWIFT_PROTOCOL
#  define SWIFT_PROTOCOL(SWIFT_NAME) SWIFT_PROTOCOL_EXTRA
#endif

#ifndef SWIFT_EXTENSION
#  define SWIFT_EXTENSION(X) X##__LINE__
#endif

#ifndef SWIFT_CLASS_NAMED
#  define SWIFT_CLASS_NAMED(SWIFT_NAME) \
    __attribute__((swift_name(SWIFT_NAME))) SWIFT_CLASS_EXTRA
#endif

#ifndef SWIFT_PROTOCOL_NAMED
#  define SWIFT_PROTOCOL_NAMED(SWIFT_NAME) \
    __attribute__((swift_name(SWIFT_NAME))) SWIFT_PROTOCOL_EXTRA
#endif

#pragma clang attribute push( \
  __attribute__((external_source_symbol(language="Swift", \
                 defined_in="Mixed",generated_declaration))), \
  apply_to=any(function,enum,objc_interface,objc_category,objc_protocol))

SWIFT_CLASS("SwiftClass")
__attribute__((objc_root_class))
@interface SwiftClass
- (void)method;
@property (nonatomic) long integerProperty;
@end

@interface SwiftClass (SWIFT_EXTENSION(foo))
- (void)extensionMethod;
@end

SWIFT_PROTOCOL_NAMED("CustomName")
@protocol SwiftProtoWithCustomName
@end

SWIFT_CLASS_NAMED("CustomNameClass")
__attribute__((objc_root_class))
@interface SwiftClassWithCustomName <SwiftProtoWithCustomName>
@end

SWIFT_PROTOCOL("SwiftProto")
@protocol SwiftProto
- (void)protoMethod;
@property (nonatomic) long protoProperty;
@end

id<SwiftProtoWithCustomName> _Nonnull
convertToProto(SwiftClassWithCustomName *_Nonnull obj);

SWIFT_CLASS("BOGUS")
@interface BogusClass
@end

# pragma clang attribute pop

@interface SwiftClass (Category)
- (void)categoryMethod:(struct PureClangType)arg;
@end
