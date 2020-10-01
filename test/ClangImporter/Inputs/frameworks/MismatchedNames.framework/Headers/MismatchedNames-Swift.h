#import <objc/NSObject.h>

#  define OBJC_DESIGNATED_INITIALIZER __attribute__((objc_designated_initializer))
#  define SWIFT_COMPILE_NAME(X) __attribute__((swift_name(X)))

#  define SWIFT_CLASS_NAMED(SWIFT_NAME) __attribute__((objc_subclassing_restricted)) SWIFT_COMPILE_NAME(SWIFT_NAME)

# pragma clang attribute push(__attribute__((external_source_symbol(language="Swift", defined_in="ObjCExportingFramework",generated_declaration))), apply_to=any(function,enum,objc_interface,objc_category,objc_protocol))

SWIFT_CLASS_NAMED("SwiftClass")
@interface SwiftClass : NSObject
+ (id _Nullable)usingIndex:(id _Nonnull)index;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end

# pragma clang attribute pop
