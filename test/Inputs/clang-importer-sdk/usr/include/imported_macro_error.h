#if __SWIFT_ATTR_SUPPORTS_MACROS
#define ERROR_MACRO __attribute__((swift_attr("@macro_library.ExpandTypeError")))
#else
#define ERROR_MACRO
#endif

void foo() ERROR_MACRO;
