#ifndef TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_NULLABLE_H
#define TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_NULLABLE_H

struct IntHolder { int value; };

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:immortal")))
    __attribute__((swift_attr("release:immortal")))
    IntBase : IntHolder {
  int i;
};

struct NoDtorThreeByte {
  char x;
  char y;
  char z;
  ~NoDtorThreeByte() = delete;
};

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:immortal")))
    __attribute__((swift_attr("release:immortal")))
    IntCharRef {
  int i;
  char b;
};

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:immortal")))
    __attribute__((swift_attr("release:immortal")))
    IntCharValue {
  int i;
  char b;
};


struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:immortal")))
    __attribute__((swift_attr("release:immortal")))
    UnimportableMemberRef {
  int z; int zz; NoDtorThreeByte x; NoDtorThreeByte xx; int y;
};

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:immortal")))
    __attribute__((swift_attr("release:immortal")))
    UnimportableMemberValue {
  int z; int zz; NoDtorThreeByte x; NoDtorThreeByte xx; int y;
};

#endif // TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_NULLABLE_H
