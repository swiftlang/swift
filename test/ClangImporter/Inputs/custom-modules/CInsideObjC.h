@interface Base
@end

struct AlreadyDeclaredStruct {
  int value;
};

#if defined(CLASS)
@interface Wrapper : Base 
#elif defined(CATEGORY)
@interface Wrapper : Base
@end
@interface Wrapper (Category)
#elif defined(PROTOCOL)
@protocol Wrapper
#else
# error "Must pick a variant"
#endif

extern void nestedFunc(void);

@property struct ForwardDeclaredStruct forward;
@property struct AlreadyDeclaredStruct backward;

struct NestedDeclaredStruct {
  int value;
};
typedef int NestedTypedef;
extern const int nestedGlobal;

@end

struct ForwardDeclaredStruct {
  int value;
};
