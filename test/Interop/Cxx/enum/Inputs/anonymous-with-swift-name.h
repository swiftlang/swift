#ifndef TEST_INTEROP_CXX_ENUM_INPUTS_ANONYMOUS_WITH_SWIFT_NAME_H
#define TEST_INTEROP_CXX_ENUM_INPUTS_ANONYMOUS_WITH_SWIFT_NAME_H

#define CF_OPTIONS(_type, _name) __attribute__((availability(swift, unavailable))) _type _name; enum : _name

typedef CF_OPTIONS(unsigned, CFColorMask) {
  kCFColorMaskRed = (1 << 1),
  kCFColorMaskGreen = (1 << 2),
  kCFColorMaskBlue = (1 << 3),
  kCFColorMaskAll = ~0U
};

inline CFColorMask useCFColorMask(CFColorMask mask) { return mask; }

struct ParentStruct { };

inline CFColorMask renameCFColorMask(ParentStruct parent)
    __attribute__((swift_name("ParentStruct.childFn(self:)")))
{ return kCFColorMaskRed; }

inline CFColorMask getCFColorMask(ParentStruct parent)
    __attribute__((swift_name("getter:ParentStruct.colorProp(self:)")))
{ return kCFColorMaskRed; }

inline void setCFColorMask(ParentStruct parent, CFColorMask newValue)
    __attribute__((swift_name("setter:ParentStruct.colorProp(self:newValue:)")))
{ }

typedef __attribute__((availability(swift, unavailable))) __attribute__((swift_name("ParentStruct.NewName"))) unsigned OldName;

enum __attribute__((flag_enum,enum_extensibility(open))) : OldName {
  kOldNameOne = (1 << 1),
  kOldNameTwo = (1 << 2)
};

typedef __attribute__((availability(swift, unavailable))) __attribute__((swift_name("GlobalNewName"))) unsigned GlobalOldName;

enum __attribute__((flag_enum,enum_extensibility(open))) : GlobalOldName {
  kGlobalOldNameOne = (1 << 1),
  kGlobalOldNameTwo = (1 << 2)
};

#if __OBJC__
@interface ColorMaker
- (void)makeOtherColorWithInt:(int) x withOptions:(CFColorMask)opts;
@end
#endif // SWIFT_OBJC_INTEROP

#endif // TEST_INTEROP_CXX_ENUM_INPUTS_ANONYMOUS_WITH_SWIFT_NAME_H
