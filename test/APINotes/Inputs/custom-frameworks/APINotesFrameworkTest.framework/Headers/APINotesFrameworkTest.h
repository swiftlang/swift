void jumpToLocation(double x, double y, double z);

void acceptDoublePointer(double* _Nonnull ptr) __attribute__((swift_name("accept(_:)")));

#ifdef __OBJC__

__attribute__((objc_root_class))
@interface A
@end

__attribute__((objc_root_class))
@interface TypeChanges
-(nonnull id)methodWithA:(nonnull id)a;
@end

#import <APINotesFrameworkTest/Properties.h>
#import <APINotesFrameworkTest/Protocols.h>

#endif
