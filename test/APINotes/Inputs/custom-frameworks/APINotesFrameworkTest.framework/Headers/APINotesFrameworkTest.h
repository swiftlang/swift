void jumpToLocation(double x, double y, double z);

void acceptDoublePointer(double* _Nonnull ptr) __attribute__((swift_name("accept(_:)")));

void oldAcceptDoublePointer(double* _Nonnull ptr) __attribute__((availability(swift, unavailable, replacement="acceptDoublePointer")));

void normallyUnchanged(void);
void normallyChangedOriginal(void) __attribute__((swift_name("normallyChanged()")));

#ifdef __OBJC__

__attribute__((objc_root_class))
@interface A
@end

__attribute__((objc_root_class))
@interface TypeChanges
-(nonnull id)methodWithA:(nonnull id)a;
@end

__attribute__((objc_root_class))
@interface Base
@end

@interface B : A
@end

@interface C : B
@end

#endif // __OBJC__

#include <APINotesFrameworkTest/Classes.h>
#include <APINotesFrameworkTest/Enums.h>
#include <APINotesFrameworkTest/Globals.h>
#include <APINotesFrameworkTest/ImportAsMember.h>
#include <APINotesFrameworkTest/Properties.h>
#include <APINotesFrameworkTest/Protocols.h>
#include <APINotesFrameworkTest/Types.h>
#include <APINotesFrameworkTest/SwiftWrapper.h>
