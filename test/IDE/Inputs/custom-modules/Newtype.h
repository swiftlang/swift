@import Foundation;

typedef NSString *__nonnull SNTErrorDomain __attribute((swift_newtype(struct)))
__attribute((swift_name("ErrorDomain")));

extern void SNTErrorDomainProcess(SNTErrorDomain d)
    __attribute((swift_name("ErrorDomain.process(self:)")));

typedef struct {} Foo;

extern const SNTErrorDomain SNTErrOne
    __attribute((swift_name("ErrorDomain.one")));
extern const SNTErrorDomain SNTErrTwo;
extern const SNTErrorDomain SNTErrorDomainThree;
extern const SNTErrorDomain SNTFourErrorDomain;
extern const SNTErrorDomain SNTFive
    __attribute((swift_name("stillAMember")));
extern const SNTErrorDomain SNTElsewhere
    __attribute((swift_name("Foo.err")));

typedef NSString *__nullable SNTClosedEnum __attribute((swift_newtype(enum)))
__attribute((swift_name("ClosedEnum")));

extern const SNTClosedEnum SNTFirstClosedEntryEnum;
extern const SNTClosedEnum SNTSecondEntry;
extern const SNTClosedEnum SNTClosedEnumThirdEntry;

typedef NSString * IUONewtype __attribute((swift_newtype(struct)));

typedef float MyFloat __attribute((swift_newtype(struct)));
extern const MyFloat globalFloat;
extern const MyFloat kPI;
extern const MyFloat kVersion;

typedef NSString * NSURLResourceKey __attribute((swift_newtype(struct)));
extern NSURLResourceKey const NSURLIsRegularFileKey;
extern NSURLResourceKey const NSURLIsDirectoryKey;
extern NSURLResourceKey const NSURLLocalizedNameKey;
