@import Foundation;

#define NS_ERROR_ENUM(_type, _name, _domain)  \
  enum _name : _type _name; enum __attribute__((ns_error_domain(_domain))) _name : _type

@class NSString;
extern const NSString *const MyErrorDomain;
/// This is my cool error code.
typedef NS_ERROR_ENUM(int, MyErrorCode, MyErrorDomain) {
	/// This is first error.
	MyErrFirst,
	/// This is second error.
	MyErrSecond,
};
