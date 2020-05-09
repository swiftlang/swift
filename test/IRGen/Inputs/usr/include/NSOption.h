#define CF_OPTIONS(_type, _name) enum _name : _type _name; enum _name : _type
#define NS_OPTIONS(_type, _name) CF_OPTIONS(_type, _name)

typedef NS_OPTIONS(int, SomeOptions) {
  SomeOptionsFoo = 1,
  SomeOptionsBar = 2,
};
