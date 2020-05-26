#define MY_ENUM(NAME) \
  enum NAME : int NAME; \
  enum NAME : int

MY_ENUM(macro_enum) {
  zero = 0
};
