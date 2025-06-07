@import Foundation;

extern NSString *const CustomErrorDomain;

// typedef NS_ERROR_ENUM(CustomErrorDomain, CustomErrorCode) { ... }
typedef enum CustomErrorCode : long CustomErrorCode;
enum __attribute__((ns_error_domain(CustomErrorDomain))) CustomErrorCode : long {
  CustomErrorA,
};
