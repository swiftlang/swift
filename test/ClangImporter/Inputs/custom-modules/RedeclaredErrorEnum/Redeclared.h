@import Foundation;
#ifndef NO_IMPORT_BASE_FROM_REDECLARED
@import Base;
#endif

extern NSString * const SomeErrorDomain;
// typedef NS_ERROR_ENUM(SomeErrorDomain, SomeErrorCode);
typedef enum SomeErrorCode : long SomeErrorCode;
enum __attribute__((ns_error_domain(SomeErrorDomain))) SomeErrorCode : long
#ifdef NO_IMPORT_BASE_FROM_REDECLARED
{
  SomeErrorX,
  SomeErrorY
}
#endif
;