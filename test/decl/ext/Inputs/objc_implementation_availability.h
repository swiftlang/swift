@import Foundation;

__attribute__((availability(macosx,introduced=99.0)))
@interface MacOS99Class1 : NSObject
@end

__attribute__((availability(macosx,introduced=99.0)))
@interface MacOS99Class2 : NSObject
@end

__attribute__((availability(macosx,introduced=99.0)))
@interface MacOS99Class3 : NSObject
@end

__attribute__((availability(macosx,unavailable)))
@interface MacOSUnavailableClass1 : NSObject
@end

__attribute__((availability(macosx,unavailable)))
@interface MacOSUnavailableClass2 : NSObject
@end

__attribute__((availability(macosx,unavailable)))
@interface MacOSUnavailableClass3 : NSObject
@end

__attribute__((availability(macosx,introduced=99.0)))
@interface MacOS99Class4 : NSObject
@end

// A class extension has no availability of its own; the availability of the
// class it extends applies to it.
@interface MacOS99Class4 ()
- (void)macOS99ClassExtensionMethod;
@end

@interface AlwaysAvailableClass : NSObject

- (void)macOS99Method1 __attribute__((availability(macosx,introduced=99.0)));
- (void)macOS99Method2 __attribute__((availability(macosx,introduced=99.0)));
- (void)macOS99Method3 __attribute__((availability(macosx,introduced=99.0)));
- (void)macOS99Method4 __attribute__((availability(macosx,introduced=99.0)));

@property int macOS99Property1 __attribute__((availability(macosx,introduced=99.0)));
@property int macOS99Property2 __attribute__((availability(macosx,introduced=99.0)));
@property int macOS99Property3 __attribute__((availability(macosx,introduced=99.0)));

- (void)macOSUnavailableMethod1 __attribute__((availability(macosx,unavailable)));
- (void)macOSUnavailableMethod2 __attribute__((availability(macosx,unavailable)));
- (void)macOSUnavailableMethod3 __attribute__((availability(macosx,unavailable)));

@end

__attribute__((availability(macosx, deprecated = 10.10)))
@interface MacOSDeprecated10_10Class1 : NSObject
@end

__attribute__((availability(macosx, deprecated = 10.10)))
@interface MacOSDeprecated10_10Class2 : NSObject
@end

__attribute__((availability(macosx, deprecated = 99.0)))
@interface MacOSDeprecated99Class1 : NSObject
@end

@interface DeprecatedMembersClass : NSObject

- (void)macOSDeprecated10_10Method1
    __attribute__((availability(macosx, deprecated = 10.10)));
- (void)macOSDeprecated10_10Method2
    __attribute__((availability(macosx, deprecated = 10.10)));
- (void)macOSDeprecated99Method1
    __attribute__((availability(macosx, deprecated = 99.0)));

@property int macOSDeprecated10_10Property1
    __attribute__((availability(macosx, deprecated=10.10)));

- (void)alwaysAvailableMethod1;

@end

@interface AsyncMembersClass : NSObject

- (void)macOS99Method1WithCompletionHandler:
    (void (^_Nonnull)(void))completionHandler
    __attribute__((availability(macosx, introduced = 99.0)));
- (void)macOS99Method2WithCompletionHandler:
    (void (^_Nonnull)(void))completionHandler
    __attribute__((availability(macosx, introduced = 99.0)));
- (void)macOS99Method3WithCompletionHandler:
    (void (^_Nonnull)(void))completionHandler
    __attribute__((availability(macosx, introduced = 99.0)));
- (void)macOS99Method4WithCompletionHandler:
    (void (^_Nonnull)(void))completionHandler
    __attribute__((availability(macosx, introduced = 99.0)));
- (void)macOSUnavailableMethod1WithCompletionHandler:
    (void (^_Nonnull)(void))completionHandler
    __attribute__((availability(macosx, unavailable)));
- (void)alwaysAvailableMethod1WithCompletionHandler:
    (void (^_Nonnull)(void))completionHandler;
- (void)alwaysAvailableMethod2WithCompletionHandler:
    (void (^_Nonnull)(void))completionHandler;

@end

@interface AccessorMembersClass : NSObject

@property int macOS99Property1
    __attribute__((availability(macosx, introduced=99.0)));
@property int macOS99Property2
    __attribute__((availability(macosx, introduced=99.0)));
@property int macOS99Property3
    __attribute__((availability(macosx, introduced=99.0)));
@property(readonly) int macOS99Property4
    __attribute__((availability(macosx, introduced=99.0)));
@property int alwaysAvailableProperty1;
@property int alwaysAvailableProperty2;

@end

// C functions implemented with '@implementation @_cdecl'.

void macOS99CDeclFunc1(int param)
    __attribute__((availability(macosx, introduced=99.0)));
void macOS99CDeclFunc2(int param)
    __attribute__((availability(macosx, introduced=99.0)));
void macOS99CDeclFunc3(int param)
    __attribute__((availability(macosx, introduced=99.0)));
void macOSUnavailableCDeclFunc1(int param)
    __attribute__((availability(macosx, unavailable)));
void alwaysAvailableCDeclFunc1(int param);

// C functions implemented with '@implementation @c'.

void macOS99CFunc1(int param)
    __attribute__((availability(macosx, introduced=99.0)));
void macOS99CFunc2(int param)
    __attribute__((availability(macosx, introduced=99.0)));
void macOS99CFunc3(int param)
    __attribute__((availability(macosx, introduced=99.0)));
void macOSUnavailableCFunc1(int param)
    __attribute__((availability(macosx, unavailable)));
void alwaysAvailableCFunc1(int param);
