#if NEW
# define NEW_NAME(x) __attribute__((swift_name(#x)))
#else
# define NEW_NAME(x)
#endif

struct BeforeStruct {
  int value;
} NEW_NAME(AfterStruct);

typedef int BeforeTypedef NEW_NAME(AfterTypedef);
typedef int BeforeWrappedTypedef __attribute__((swift_wrapper(struct))) NEW_NAME(AfterWrappedTypedef);

#if NEW
typedef struct DifferentStruct {
  int value;
} BeforeReplacedType NEW_NAME(AfterReplacedType);
#else
struct BeforeReplacedType {
  float value;
};
#endif
