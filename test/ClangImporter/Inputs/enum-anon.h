#if defined(_WIN32)
typedef __INTPTR_TYPE__ intptr_t;

#define MAKE_UNSIGNED : unsigned
#define MAKE_SIGNEDLL : intptr_t
#else
#define MAKE_UNSIGNED
#endif

enum MAKE_SIGNEDLL {
  Constant1,
  Constant2
};

enum MAKE_UNSIGNED {
  VarConstant1,
  VarConstant2
} global;

typedef struct SR2511 {
    int x;

    enum MAKE_UNSIGNED {
      SR2511A = 0, SR2511B, SR2511C
    } y;

    int z;
} SR2511;

#if __OBJC__
enum : unsigned short {
  USConstant1,
  USConstant2
};

enum : unsigned short {
  USVarConstant1,
  USVarConstant2
} usGlobal;
#endif // __OBJC__
