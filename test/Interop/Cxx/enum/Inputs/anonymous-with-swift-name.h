#ifndef TEST_INTEROP_CXX_ENUM_INPUTS_ANONYMOUS_WITH_SWIFT_NAME_H
#define TEST_INTEROP_CXX_ENUM_INPUTS_ANONYMOUS_WITH_SWIFT_NAME_H

#define SOME_OPTIONS(_type, _name) __attribute__((availability(swift, unavailable))) _type _name; enum __attribute__((flag_enum,enum_extensibility(open))) : _name
#define CF_OPTIONS(_type, _name) __attribute__((availability(swift, unavailable))) _type _name; enum : _name

typedef SOME_OPTIONS(unsigned, SOColorMask) {
    kSOColorMaskRed = (1 << 1),
    kSOColorMaskGreen = (1 << 2),
    kSOColorMaskBlue = (1 << 3),
    kSOColorMaskAll = ~0U
};


typedef CF_OPTIONS(unsigned, CFColorMask) {
  kCFColorMaskRed = (1 << 1),
  kCFColorMaskGreen = (1 << 2),
  kCFColorMaskBlue = (1 << 3),
  kCFColorMaskAll = ~0U
};

#endif // TEST_INTEROP_CXX_ENUM_INPUTS_ANONYMOUS_WITH_SWIFT_NAME_H