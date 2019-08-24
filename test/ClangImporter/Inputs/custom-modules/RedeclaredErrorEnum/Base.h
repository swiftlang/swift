@import Foundation;

extern NSString * const SomeErrorDomain;
// typedef NS_ERROR_ENUM(SomeErrorDomain, SomeErrorCode) { ... }
typedef enum SomeErrorCode : long SomeErrorCode;
enum __attribute__((ns_error_domain(SomeErrorDomain))) SomeErrorCode : long {
  SomeErrorX,
  SomeErrorY
};
