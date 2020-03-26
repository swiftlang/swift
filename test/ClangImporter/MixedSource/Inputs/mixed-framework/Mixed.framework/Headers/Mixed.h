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

SWIFT_CLASS("ObjCClass")
__attribute__((objc_root_class))
@interface ObjCClass
@end

SWIFT_CLASS("ImplicitlyObjCClass")
@interface ImplicitlyObjCClass : ObjCClass
@end
void consumeImplicitlyObjCClass(ImplicitlyObjCClass *_Nonnull obj);

SWIFT_CLASS("ExplicitlyObjCClass")
__attribute__((objc_root_class))
@interface ExplicitlyObjCClass
@end
void consumeExplicitlyObjCClass(ExplicitlyObjCClass *_Nonnull obj);

SWIFT_CLASS_NAMED("HasSameCustomNameClass")
__attribute__((objc_root_class))
@interface HasSameCustomNameClass
@end
void consumeHasSameCustomNameClass(HasSameCustomNameClass *_Nonnull obj);

SWIFT_CLASS_NAMED("SwiftNativeTypeHasDifferentCustomNameClass")
__attribute__((objc_root_class))
@interface NativeTypeHasDifferentCustomNameClass
@end
SWIFT_CLASS_NAMED("NativeTypeHasDifferentCustomNameClass")
__attribute__((objc_root_class))
@interface ObjCNativeTypeHasDifferentCustomNameClass
@end
void consumeNativeTypeHasDifferentCustomNameClass(NativeTypeHasDifferentCustomNameClass *_Nonnull obj);
void consumeObjCNativeTypeHasDifferentCustomNameClass(ObjCNativeTypeHasDifferentCustomNameClass *_Nonnull obj);

SWIFT_CLASS_NAMED("SwiftNativeTypeIsNonObjCClass")
__attribute__((objc_root_class))
@interface NativeTypeIsNonObjCClass
@end
void consumeNativeTypeIsNonObjCClass(NativeTypeIsNonObjCClass *_Nonnull obj);

@class ForwardImplicitlyObjCClass;
void consumeForwardImplicitlyObjCClass(ForwardImplicitlyObjCClass *_Nonnull obj);

@class ForwardExplicitlyObjCClass;
void consumeForwardExplicitlyObjCClass(ForwardExplicitlyObjCClass *_Nonnull obj);

@class ForwardHasSameCustomNameClass;
void consumeForwardHasSameCustomNameClass(ForwardHasSameCustomNameClass *_Nonnull obj);

@class ForwardNativeTypeHasDifferentCustomNameClass;
@class ObjCForwardNativeTypeHasDifferentCustomNameClass;
void consumeForwardNativeTypeHasDifferentCustomNameClass(ForwardNativeTypeHasDifferentCustomNameClass *_Nonnull obj);
void consumeObjCForwardNativeTypeHasDifferentCustomNameClass(ObjCForwardNativeTypeHasDifferentCustomNameClass *_Nonnull obj);

@class ForwardNativeTypeIsNonObjCClass;
void consumeForwardNativeTypeIsNonObjCClass(ForwardNativeTypeIsNonObjCClass *_Nonnull obj);

@class ForwardNativeTypeIsUnambiguouslyNonObjCClass;
void consumeForwardNativeTypeIsUnambiguouslyNonObjCClass(ForwardNativeTypeIsUnambiguouslyNonObjCClass *_Nonnull obj);

SWIFT_CLASS("BOGUS")
@interface BogusClass
@end

# pragma clang attribute pop

@interface SwiftClass (Category)
- (void)categoryMethod:(struct PureClangType)arg;
@end
