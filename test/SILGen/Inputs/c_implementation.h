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
