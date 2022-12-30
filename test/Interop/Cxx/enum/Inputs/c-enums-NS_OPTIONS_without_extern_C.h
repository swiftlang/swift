#if __has_attribute(enum_extensibility)
#define __CF_ENUM_ATTRIBUTES __attribute__((enum_extensibility(open)))
#define __CF_CLOSED_ENUM_ATTRIBUTES __attribute__((enum_extensibility(closed)))
#define __CF_OPTIONS_ATTRIBUTES __attribute__((flag_enum,enum_extensibility(open)))
#else
#define __CF_ENUM_ATTRIBUTES
#define __CF_CLOSED_ENUM_ATTRIBUTES
#define __CF_OPTIONS_ATTRIBUTES
#endif

#define CF_OPTIONS(_type, _name) _type __attribute__((availability(swift, unavailable))) _name; enum __CF_OPTIONS_ATTRIBUTES : _name
#define NS_OPTIONS(_type, _name) CF_OPTIONS(_type, _name)
#define UIKIT_EXTERN extern "C" __attribute__((visibility("default")))

typedef long NSInteger;

UIKIT_EXTERN
@interface UIPrinter

typedef NS_OPTIONS(NSInteger, UIPrinterJobTypes) {
  UIPrinterJobTypeUnknown = 0,
  UIPrinterJobTypeDocument = 1 << 0,
  UIPrinterJobTypeEnvelope = 1 << 1,
  UIPrinterJobTypeLabel = 1 << 2,
  UIPrinterJobTypePhoto = 1 << 3,
  UIPrinterJobTypeReceipt = 1 << 4,
  UIPrinterJobTypeRoll = 1 << 5,
  UIPrinterJobTypeLargeFormat = 1 << 6,
  UIPrinterJobTypePostcard = 1 << 7
};

@end
