#include <CoreFoundation.h>

// Audited, so these are imported as returning 'CFString' rather than
// 'Unmanaged<CFString>'.
#pragma clang arc_cf_code_audited begin
CFStringRef _Nonnull CImplReturnsAuditedCFString(void);
CFStringRef _Nullable CImplReturnsAuditedNullableCFString(void);
CFStringRef _Nonnull CImplReturnsAuditedCFStringWrongType(void);
CFStringRef _Nonnull CImplReturnsAuditedCFStringDroppedOptional(void);
void CImplTakesAuditedCFString(CFStringRef _Nonnull string);
CFTypeRef _Nonnull CImplReturnsAuditedCFTypeRef(void);
#pragma clang arc_cf_code_audited end
