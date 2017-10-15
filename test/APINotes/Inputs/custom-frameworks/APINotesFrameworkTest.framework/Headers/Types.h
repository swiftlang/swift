#pragma clang assume_nonnull begin

struct __attribute__((swift_name("VeryImportantCStruct"))) SomeCStruct {
    int field;
};

typedef int __attribute__((swift_name("VeryImportantCAlias"))) SomeCAlias;

struct NormallyUnchangedWrapper {
  int value;
};
struct NormallyChangedOriginalWrapper{
  int value;
} __attribute__((swift_name("NormallyChangedWrapper")));

#pragma clang assume_nonnull end
