#ifdef __cplusplus
#define MY_ENUM(NAME) \
  enum NAME : int
#else
#define MY_ENUM(NAME) \
  enum NAME : int; \
  typedef enum NAME NAME; \
  enum NAME : int
#endif

MY_ENUM(macro_enum) {
  zero = 0
};
