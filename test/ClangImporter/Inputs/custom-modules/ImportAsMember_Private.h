#ifndef IMPORT_AS_MEMBER_PRIVATE_H
#define IMPORT_AS_MEMBER_PRIVATE_H

#include <ImportAsMember.h>

@class IAMPrivateChild;
// CHECK-NOT: ImportAsMember_Private.h:[[@LINE-1]]:{{[0-9]+}}: warning: imported declaration 'IAMPrivateChild' could not be mapped to 'IAMPrivateParent.Child'

#endif // IMPORT_AS_MEMBER_PRIVATE_H
