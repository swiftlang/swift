#import <Foundation/Foundation.h>

#define MY_ERROR_ENUM(_type, _name, _domain)                                   \
  enum _name : _type _name;                                                    \
  enum __attribute__((ns_error_domain(_domain))) _name : _type

@class NSString;
extern NSString *const TestErrorDomain;
typedef MY_ERROR_ENUM(int, TestError, TestErrorDomain) {
  TENone,
  TEOne,
  TETwo,
};

extern NSString *const ExhaustiveErrorDomain;
typedef MY_ERROR_ENUM(int, ExhaustiveError, ExhaustiveErrorDomain) {
  EENone,
  EEOne,
  EETwo,
} __attribute__((enum_extensibility(closed)));

extern NSString *const OtherErrorDomain;
typedef MY_ERROR_ENUM(int, OtherErrorCode, OtherErrorDomain) {
  OtherA,
  OtherB,
  OtherC,
};

extern NSString *TypedefOnlyErrorDomain;
typedef enum __attribute__((ns_error_domain(TypedefOnlyErrorDomain))) {
  TypedefOnlyErrorBadness
} TypedefOnlyError;


TestError getErr(void);
ExhaustiveError getExhaustiveErr(void);
