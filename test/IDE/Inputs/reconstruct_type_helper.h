#include "swift_name.h"

typedef int Unwrapped;
typedef int Wrapped __attribute__((swift_wrapper(struct)));

typedef int TagTypedefCollision __attribute__((swift_name("TTCollisionTypedef")));
enum TagTypedefCollision {
  TagTypedefCollisionX
} __attribute__((swift_name("TTCollisionTag")));


enum EnumByTag {
  EnumByTagX
};
typedef enum {
  EnumByTypedefX
} EnumByTypedef;
typedef enum EnumByBoth {
  EnumByBothX
} EnumByBoth;


#if __OBJC__
#include "swift_name_objc.h"

@compatibility_alias SomeClassAlias SNSomeClass;

extern NSString *SomeErrorDomain;
enum SomeError {
  SomeErrorBadness
} __attribute__((ns_error_domain(SomeErrorDomain)));

extern NSString *SomeOtherErrorDomain;
typedef enum __attribute__((ns_error_domain(SomeOtherErrorDomain))) {
  SomeOtherErrorBadness
} SomeOtherError __attribute__((swift_name("SomeRenamedError")));

#endif // __OBJC__

