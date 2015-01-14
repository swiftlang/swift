#ifndef __LSINFO__
#define __LSINFO__

#ifndef __COREFOUNDATION__
#include <CoreFoundation.h>
#endif

extern OSStatus 
LSCopyDisplayNameForURL(
  CFURLRef       inURL,
  CFStringRef *  outDisplayName);
  
extern OSStatus 
LSGetExtensionInfo(
  UniCharCount    inNameLen,
  const UniChar   inNameBuffer[],
  UniCharCount *  outExtStartIndex);

#endif