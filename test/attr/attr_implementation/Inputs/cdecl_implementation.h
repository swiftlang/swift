void CImplFunc1(int param);
void CImplFunc2(int param);

void CImplFuncMismatch1(int param);
void CImplFuncMismatch2(int param);

void CImplFuncMismatch3(void *_Nullable param);
void CImplFuncMismatch4(void *_Nullable param);
void CImplFuncMismatch5(void *_Nonnull param);
void CImplFuncMismatch6(void *_Nonnull param);
void *_Nullable CImplFuncMismatch3a(int param);
void *_Nullable CImplFuncMismatch4a(int param);
void *_Nonnull CImplFuncMismatch5a(int param);
void *_Nonnull CImplFuncMismatch6a(int param);

void CImplFuncNameMismatch1(int param);
void CImplFuncNameMismatch2(int param);

int CImplGetComputedGlobal1(void) __attribute__((swift_name("getter:cImplComputedGlobal1()")));
void CImplSetComputedGlobal1(int param) __attribute__((swift_name("setter:cImplComputedGlobal1(newValue:)")));

typedef struct CImplStruct {} CImplStruct;

void CImplStructStaticFunc1(int param) __attribute__((swift_name("CImplStruct.staticFunc1(_:)")));
