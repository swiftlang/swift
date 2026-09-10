#include <CoreFoundation.h>

#define CF_RETURNS_RETAINED __attribute__((cf_returns_retained))
#define CF_RETURNS_NOT_RETAINED __attribute__((cf_returns_not_retained))
#define CF_CONSUMED __attribute__((cf_consumed))

void CImplTakesCFArray(CFArrayRef arr);
void CImplTakesCFTree(CFTreeRef tree);
void CImplTakesConsumedCFString(CF_CONSUMED CFStringRef string);

CF_RETURNS_RETAINED
CFStringRef CImplReturnsRetainedCFString(void);

CF_RETURNS_NOT_RETAINED
CFStringRef CImplReturnsNotRetainedCFString(void);

// Not ownership-audited: imported as returning 'Unmanaged<CFString>?'.
CFStringRef CImplReturnsUnauditedCFString(void);

// 'CFTypeRef' is imported as 'AnyObject'.
void CImplTakesCFTypeRef(CFTypeRef obj);
