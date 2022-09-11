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
