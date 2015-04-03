// Enums and Options
#define __CF_ENUM_GET_MACRO(_1, _2, NAME, ...) NAME
#if (__cplusplus && __cplusplus >= 201103L && (__has_extension(cxx_strong_enums) || __has_feature(objc_fixed_enum))) || (!__cplusplus && __has_feature(objc_fixed_enum))
#define __CF_NAMED_ENUM(_type, _name)     enum _name : _type _name; enum _name : _type
#define __CF_ANON_ENUM(_type)             enum : _type
#if (__cplusplus)
#define CF_OPTIONS(_type, _name) _type _name; enum : _type
#else
#define CF_OPTIONS(_type, _name) enum _name : _type _name; enum _name : _type
#endif
#else
#define __CF_NAMED_ENUM(_type, _name) _type _name; enum
#define __CF_ANON_ENUM(_type) enum
#define CF_OPTIONS(_type, _name) _type _name; enum
#endif

/* CF_ENUM supports the use of one or two arguments. The first argument is always the integer type used for the values of the enum. The second argument is an optional type name for the macro. When specifying a type name, you must precede the macro with 'typedef' like so:

typedef CF_ENUM(CFIndex, CFComparisonResult) {
    ...
};

If you do not specify a type name, do not use 'typdef', like so:

CF_ENUM(CFIndex) {
    ...
};
*/
#define CF_ENUM(...) __CF_ENUM_GET_MACRO(__VA_ARGS__, __CF_NAMED_ENUM, __CF_ANON_ENUM)(__VA_ARGS__)

#define NS_ENUM(...) CF_ENUM(__VA_ARGS__)
#define NS_OPTIONS(_type, _name) CF_OPTIONS(_type, _name)

CF_ENUM(short) {
  Zero, One, Two
};

typedef CF_ENUM(unsigned, Color) {
  Red, Green, Blue
};
Color getColor();

typedef NS_ENUM(unsigned char, MoreColor) {
  Cyan, Magenta, Yellow, Black
};
MoreColor getMoreColor();


typedef CF_OPTIONS(unsigned, ColorOptions) {
  Pastel, Swift
};
ColorOptions getColorOptions();
