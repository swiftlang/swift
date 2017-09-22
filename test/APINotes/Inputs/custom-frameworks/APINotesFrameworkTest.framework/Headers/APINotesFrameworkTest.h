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

#import <APINotesFrameworkTest/Classes.h>
#import <APINotesFrameworkTest/Enums.h>
#import <APINotesFrameworkTest/Globals.h>
#import <APINotesFrameworkTest/ImportAsMember.h>
#import <APINotesFrameworkTest/Properties.h>
#import <APINotesFrameworkTest/Protocols.h>
#import <APINotesFrameworkTest/Types.h>
#import <APINotesFrameworkTest/SwiftWrapper.h>
