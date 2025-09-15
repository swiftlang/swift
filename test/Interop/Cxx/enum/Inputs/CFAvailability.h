#if __has_attribute(enum_extensibility)
#define __CF_ENUM_ATTRIBUTES __attribute__((enum_extensibility(open)))
#define __CF_CLOSED_ENUM_ATTRIBUTES __attribute__((enum_extensibility(closed)))
#define __CF_OPTIONS_ATTRIBUTES                                                \
  __attribute__((flag_enum, enum_extensibility(open)))
#else
#define __CF_ENUM_ATTRIBUTES
#define __CF_CLOSED_ENUM_ATTRIBUTES
#define __CF_OPTIONS_ATTRIBUTES
#endif

#if (__cplusplus)
#define CF_OPTIONS(_type, _name) __attribute__((availability(swift,unavailable))) _type _name; enum __CF_OPTIONS_ATTRIBUTES : _name
#else
#define CF_OPTIONS(_type, _name) enum __CF_OPTIONS_ATTRIBUTES _name : _type _name; enum _name : _type
#endif

#define NS_OPTIONS(_type, _name) CF_OPTIONS(_type, _name)

typedef NS_OPTIONS(int, StandardNSOption) {
  StandardNSOption1,
  StandardNSOption2
};
