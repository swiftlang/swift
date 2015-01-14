#ifndef __COLLECTIONS__
#define __COLLECTIONS__

#ifndef __MACTYPES__
#include <MacTypes.h>
#endif

enum {
  kCollectionNoAttributes       = 0x00000000,
  kCollectionAllAttributes      = (int)0xFFFFFFFF,
  kCollectionUserAttributes     = 0x0000FFFF,
  kCollectionDefaultAttributes  = 0x40000000
};

/* abstract data type for a collection */
typedef struct OpaqueCollection*        Collection;
/* collection member 4 byte tag */
typedef FourCharCode                    CollectionTag;

#endif
