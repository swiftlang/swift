// This file is meant to be used with the mock SDK, not the real one.
#import <Foundation.h>

#define SWIFT_NAME(X) __attribute__((swift_name(#X)))

@interface Wrapper : NSObject
@end

enum TopLevelRaw { TopLevelRawA };
enum MemberRaw { MemberRawA } SWIFT_NAME(Wrapper.Raw);

typedef enum { TopLevelAnonA } TopLevelAnon;
typedef enum { MemberAnonA } MemberAnon SWIFT_NAME(Wrapper.Anon);
typedef enum SWIFT_NAME(Wrapper.Anon2) { MemberAnon2A } MemberAnon2;

typedef enum TopLevelTypedef { TopLevelTypedefA } TopLevelTypedef;
typedef enum SWIFT_NAME(Wrapper.Typedef) MemberTypedef { MemberTypedefA } MemberTypedef;

typedef NS_ENUM(long, TopLevelEnum) { TopLevelEnumA };
typedef NS_ENUM(long, MemberEnum) { MemberEnumA } SWIFT_NAME(Wrapper.Enum);

typedef NS_OPTIONS(long, TopLevelOptions) { TopLevelOptionsA = 1 };
typedef NS_OPTIONS(long, MemberOptions) { MemberOptionsA = 1} SWIFT_NAME(Wrapper.Options);
