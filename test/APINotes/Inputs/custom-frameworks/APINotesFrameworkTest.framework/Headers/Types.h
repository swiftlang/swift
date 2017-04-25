#pragma clang assume_nonnull begin

struct __attribute__((swift_name("VeryImportantCStruct"))) SomeCStruct {
    int field;
};

typedef int __attribute__((swift_name("VeryImportantCAlias"))) SomeCAlias;

#pragma clang assume_nonnull end
