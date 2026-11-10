// NOTE: This header is used on platforms that do not support Obj-C.

extern void clangFunc(int x);
extern int clangGlobalVar;
struct ClangStruct {
  int x;
};
#define CONSTANT_MACRO 42
enum ClangEnum { ClangEnumMember = 0 };
static int clangStaticGlobalVar;
static const int clangStaticConstGlobalVar = 42;
const int clangConstGlobalVar = 42;
int clangDefinedGlobalVar = 1;
extern _Thread_local int clangThreadLocalGlobalVar;
