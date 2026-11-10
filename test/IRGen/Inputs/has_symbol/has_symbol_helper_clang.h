// NOTE: This header is used on platforms that do not support Obj-C.

extern void clangFunc(int x);
extern int clangGlobalVar;
extern int clangWeakImportGlobalVar __attribute__((weak_import));
extern void clangHiddenFunc(void) __attribute__((visibility("hidden")));
extern int clangHiddenGlobalVar __attribute__((visibility("hidden")));
