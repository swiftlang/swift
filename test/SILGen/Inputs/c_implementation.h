@import Foundation;

#define CF_RETURNS_RETAINED __attribute__((cf_returns_retained))
#define CF_RETURNS_NOT_RETAINED __attribute__((cf_returns_not_retained))
#define CF_CONSUMED __attribute__((cf_consumed))

CF_RETURNS_RETAINED
CFStringRef returns_retained(void);

CF_RETURNS_NOT_RETAINED
CFStringRef returns_not_retained(void);

void passes_borrowed(CFStringRef string);

void passes_consumed(CF_CONSUMED CFStringRef string);

// Not ownership-audited, so this is imported as returning
// 'Unmanaged<CFString>?'.
CFStringRef returns_unaudited(void);

// 'CFTypeRef' is imported as 'AnyObject', and uses the same reference counting.
void passes_any_borrowed(CFTypeRef obj);

CF_RETURNS_RETAINED
CFTypeRef returns_any_retained(void);
