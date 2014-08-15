void unavail1(void) __attribute__((unavailable("first")));
void unavail1(void);
void unavail1(void);

void unavail2(void);
void unavail2(void) __attribute__((unavailable("middle")));
void unavail2(void);

void unavail3(void);
void unavail3(void);
void unavail3(void) __attribute__((unavailable("last")));


struct __attribute__((unavailable("first"))) UnavailStruct1 {};
struct UnavailStruct1;
struct UnavailStruct1;

struct __attribute__((unavailable("first"))) UnavailStruct2;
struct UnavailStruct2 {};
struct UnavailStruct2;

struct __attribute__((unavailable("first"))) UnavailStruct3;
struct UnavailStruct3;
struct UnavailStruct3 {};

struct UnavailStruct4;
struct __attribute__((unavailable("middle"))) UnavailStruct4 {};
struct UnavailStruct4;

struct UnavailStruct5;
struct __attribute__((unavailable("middle"))) UnavailStruct5;
struct UnavailStruct5 {};

struct UnavailStruct6;
struct UnavailStruct6;
struct __attribute__((unavailable("last"))) UnavailStruct6 {};


__attribute__((unavailable("first")))
@interface UnavailClass1
@end
@class UnavailClass1;
@class UnavailClass1;

@class UnavailClass2;
__attribute__((unavailable("middle")))
@interface UnavailClass2
@end
@class UnavailClass2;

@class UnavailClass3;
@class UnavailClass3;
__attribute__((unavailable("last")))
@interface UnavailClass3
@end


__attribute__((unavailable("first")))
@protocol UnavailProto1
@end
@protocol UnavailProto1;
@protocol UnavailProto1;

@protocol UnavailProto2;
__attribute__((unavailable("middle")))
@protocol UnavailProto2
@end
@protocol UnavailProto2;

@protocol UnavailProto3;
@protocol UnavailProto3;
__attribute__((unavailable("last")))
@protocol UnavailProto3
@end
