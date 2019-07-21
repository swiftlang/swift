@import Foundation;
@import CoreFoundation;

typedef NSString *_Nonnull SNTErrorDomain __attribute((swift_newtype(struct)))
__attribute((swift_name("ErrorDomain")));

extern void SNTErrorDomainProcess(SNTErrorDomain d)
    __attribute((swift_name("ErrorDomain.process(self:)")));

typedef struct {} Food;

extern const SNTErrorDomain SNTErrOne
    __attribute((swift_name("ErrorDomain.one")));
extern const SNTErrorDomain SNTErrTwo;
extern const SNTErrorDomain SNTErrorDomainThree;
extern const SNTErrorDomain SNTFourErrorDomain;
extern const SNTErrorDomain SNTFive
    __attribute((swift_name("stillAMember")));
extern const SNTErrorDomain SNTElsewhere
    __attribute((swift_name("Food.err")));

typedef NSString *_Nullable SNTClosedEnum __attribute((swift_newtype(enum)))
__attribute((swift_name("ClosedEnum")));

extern const SNTClosedEnum SNTFirstClosedEntryEnum;
extern const SNTClosedEnum SNTSecondEntry;
extern const SNTClosedEnum SNTClosedEnumThirdEntry;

typedef NSString * IUONewtype __attribute((swift_newtype(struct)));

typedef float MyFloat __attribute((swift_newtype(struct)));
extern const MyFloat globalFloat;
extern const MyFloat kPI;
extern const MyFloat kVersion;

typedef int MyInt __attribute((swift_newtype(struct)));
extern const MyInt kMyIntZero;
extern const MyInt kMyIntOne;
extern const int kRawInt;
extern void takesMyInt(MyInt);

typedef NSString * NSURLResourceKey __attribute((swift_newtype(struct)));
extern _Null_unspecified NSURLResourceKey const NSURLIsRegularFileKey;
extern _Null_unspecified NSURLResourceKey const NSURLIsDirectoryKey;
extern _Null_unspecified NSURLResourceKey const NSURLLocalizedNameKey;

// Special case: Notifications
extern const NSString * _Null_unspecified FooNotification;
extern const NSString * _Null_unspecified kBarNotification;
extern const NSString * _Null_unspecified NSWibbleNotification;

// But not just 'Notification'
extern const NSString * _Null_unspecified kNotification;
extern const NSString * _Null_unspecified Notification;

// Nor when explicitly swift_name-ed
extern const NSString * _Null_unspecified kSNNotification
    __attribute((swift_name("swiftNamedNotification")));

// Test CFStringRef
typedef CFStringRef CFNewType __attribute((swift_newtype(struct)));

// CF audited
_Pragma("clang arc_cf_code_audited begin")
extern _Null_unspecified const CFNewType MyCFNewTypeValue;
extern _Null_unspecified CFNewType FooAudited(void);
_Pragma("clang arc_cf_code_audited end")
extern _Null_unspecified const CFNewType MyCFNewTypeValueUnauditedButConst;

// un-audited CFStringRef
extern _Null_unspecified CFNewType MyCFNewTypeValueUnaudited;
extern _Null_unspecified CFNewType FooUnaudited(void);

// Tests to show identical calling convention / binary representation for
// new_type and non-new_type
typedef CFStringRef MyABINewType __attribute((swift_newtype(struct)));
typedef CFStringRef MyABIOldType;
_Pragma("clang arc_cf_code_audited begin")
extern _Null_unspecified const MyABINewType kMyABINewTypeGlobal;
extern _Null_unspecified const MyABIOldType kMyABIOldTypeGlobal;
extern _Null_unspecified MyABINewType getMyABINewType(void);
extern _Null_unspecified MyABIOldType getMyABIOldType(void);
extern void takeMyABINewType(_Null_unspecified MyABINewType);
extern void takeMyABIOldType(_Null_unspecified MyABIOldType);

extern void takeMyABINewTypeNonNull(_Nonnull MyABINewType);
extern void takeMyABIOldTypeNonNull(_Nonnull MyABIOldType);
_Pragma("clang arc_cf_code_audited end")

typedef NSString *MyABINewTypeNS __attribute((swift_newtype(struct)));
typedef NSString *MyABIOldTypeNS;
extern _Null_unspecified MyABINewTypeNS getMyABINewTypeNS(void);
extern _Null_unspecified MyABIOldTypeNS getMyABIOldTypeNS(void);
extern void takeMyABINewTypeNonNullNS(_Nonnull MyABINewTypeNS);
extern void takeMyABIOldTypeNonNullNS(_Nonnull MyABIOldTypeNS);

// Nested types
typedef struct {int i;} NSSomeContext;

typedef NSString *NSSomeContextName __attribute((swift_newtype(struct)))
__attribute((swift_name("NSSomeContext.Name")));

extern _Null_unspecified const NSSomeContextName NSMyContextName;

typedef struct T *TRef __attribute((swift_newtype(struct)));
typedef const struct T *ConstTRef __attribute((swift_newtype(struct)));
extern _Nonnull TRef create_T(void);
extern _Nonnull ConstTRef create_ConstT(void);
extern void destroy_T(_Null_unspecified TRef);
extern void destroy_ConstT(_Null_unspecified ConstTRef);

extern void mutate_TRef_Pointee(_Null_unspecified TRef) __attribute((swift_name("TRef.mutatePointee(self:)")));
extern void mutate_TRef(_Null_unspecified TRef * _Null_unspecified) __attribute((swift_name("TRef.mutate(self:)")));
extern void use_ConstT(_Null_unspecified ConstTRef)
    __attribute((swift_name("ConstTRef.use(self:)")));

typedef struct T *_Nonnull *TRefRef __attribute((swift_newtype(struct)));
typedef struct T *_Nonnull const *ConstTRefRef
    __attribute((swift_newtype(struct)));
extern _Nonnull TRefRef create_TRef(void);
extern _Nonnull ConstTRefRef create_ConstTRef(void);
extern void destroy_TRef(_Null_unspecified TRefRef);
extern void destroy_ConstTRef(_Null_unspecified ConstTRefRef);

extern void mutate_TRefRef_Pointee(_Null_unspecified TRefRef)
    __attribute((swift_name("TRefRef.mutatePointee(self:)")));
extern void mutate_TRefRef(_Null_unspecified TRefRef* _Null_unspecified)
    __attribute((swift_name("TRefRef.mutate(self:)")));
extern void use_ConstTRef(_Null_unspecified ConstTRefRef)
    __attribute((swift_name("ConstTRefRef.use(self:)")));

typedef NSString *MyString __attribute__((__swift_newtype__(struct)));

