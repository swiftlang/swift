#define CF_ENUM(_type, _name) enum _name : _type _name; \
  enum __attribute__((enum_extensibility(open))) _name : _type
#define CF_OPTIONS(_type, _name) enum _name : _type _name; \
  enum __attribute__((enum_extensibility(open), flag_enum)) _name : _type

typedef CF_ENUM(int, CFEnumWithAttr) {
  CFEnumWithAttrFirst = 1,
};
typedef CF_ENUM(int, UnknownEnumThanksToAPINotes) {
  UnknownEnumThanksToAPINotesFirst = 1,
};

typedef CF_OPTIONS(int, CFOptionsWithAttr) {
  CFOptionsWithAttrFirst = 1,
};
typedef CF_OPTIONS(int, UnknownOptionsThanksToAPINotes) {
  UnknownOptionsThanksToAPINotesFirst = 1,
};

enum EnumByTag {
  EnumByTagX
};
typedef enum {
  EnumByTypedefX
} EnumByTypedef;
typedef enum EnumByBoth {
  EnumByBothX
} EnumByBoth;


typedef enum {
  kEventInit,
  kEventReset
} SystemEvent;
