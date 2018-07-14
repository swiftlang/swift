#ifndef __BACKUPCORE__
#define __BACKUPCORE__

#ifndef __COREFOUNDATION__
#include <CoreFoundation.h>
#endif
#include <MacTypes.h>

extern Boolean 
CSBackupIsItemExcluded(
  CFURLRef   item,
  Boolean *  excludeByPath);

#endif
