// RUN: %target-typecheck-verify-swift

#if _runtime(_ObjC)
#endif

#if _runtime(_Native)
#endif

#if _runtime(ObjC) // expected-error{{unexpected argument for the '_runtime' condition; expected '_Native' or '_ObjC'}}
#endif

#if _runtime(Native) // expected-error{{unexpected argument for the '_runtime' condition; expected '_Native' or '_ObjC'}}
#endif

#if _runtime(Invalid) // expected-error{{unexpected argument for the '_runtime' condition; expected '_Native' or '_ObjC'}}
#endif
