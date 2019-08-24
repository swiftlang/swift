#define CF_ENUM(_type, _name) enum _name : _type _name; enum _name : _type
typedef CF_ENUM(int, ComparisonResult) {
  Ascending,
  Same,
  Descending
};
