enum {
  Constant1,
  Constant2
};

enum {
  VarConstant1,
  VarConstant2
} global;

// NB: The order of fields here is important.
typedef struct Struct {
    int firstField;

    enum {
      NestedConstant1 = 0, NestedConstant2, NestedConstant3
    } adhocAnonEnumField;

    int lastField;
} Struct;

typedef struct {
  enum { TdNestedInStruct } field;
} TdStruct;

typedef union {
  enum { TdNestedInUnion } field;
} TdUnion;

struct NestedTagless {
  union {
    enum { TLEnumField } e;
    enum { TLEnumDecl };
    int paddingInner;
  } f;
  int paddingOuter;
};

struct DoubleNestedTagless {
  union {
    union {
      enum { TL2EnumField } e;
      enum { TL2EnumDecl };
      int paddingInner;
    } inner;
    int paddingOuter;
  } outerAnon;
  int paddingTop;
};

struct NestedAnon {
  union {
    enum { AnonEnumField } e;
    enum { AnonEnumDecl };
    int paddingInner;
  };
  int paddingOuter;
};

struct DoubleNestedAnon {
  union {
    union {
      enum { Anon2EnumField } e;
      enum { Anon2EnumDecl };
      int paddingInner;
    };
    int paddingOuter;
  };
  int paddingTop;
};

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
