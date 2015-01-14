#ifndef __BACKUPCORE__
#define __BACKUPCORE__

#ifndef __COREFOUNDATION__
#include <CoreFoundation.h>
#endif

extern Boolean 
CSBackupIsItemExcluded(
  CFURLRef   item,
  Boolean *  excludeByPath);

#endif
