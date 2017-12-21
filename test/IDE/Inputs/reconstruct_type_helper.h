#include "swift_name.h"

typedef int Unwrapped;
typedef int Wrapped __attribute__((swift_wrapper(struct)));

typedef int TagTypedefCollision __attribute__((swift_name("TTCollisionTypedef")));
enum TagTypedefCollision {
  TagTypedefCollisionX
} __attribute__((swift_name("TTCollisionTag")));

#if __OBJC__
#include "swift_name_objc.h"

@compatibility_alias SomeClassAlias SNSomeClass;

#endif // __OBJC__

