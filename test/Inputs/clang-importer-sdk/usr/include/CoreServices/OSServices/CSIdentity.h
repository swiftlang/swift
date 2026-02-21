#ifndef __CSIDENTITY__
#define __CSIDENTITY__

#ifndef __COREFOUNDATION__
#include <CoreFoundation.h>
#endif

typedef struct __CSIdentity* CSIdentityRef;

extern CSIdentityRef 
CSIdentityCreateCopy(
  CFAllocatorRef   allocator,
  CSIdentityRef    identity);

#endif
