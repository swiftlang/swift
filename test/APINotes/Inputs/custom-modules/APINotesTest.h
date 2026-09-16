void moveToPoint(int, int, int);
void jumpToPoint(double x);

extern int ANTGlobalValue;
extern int ANTGlobalValue2;
extern int ANTGlobalValue3;
extern int ANTGlobalValue4;
extern int ANTGlobalValue5;
typedef unsigned APINotesUnsigned;
typedef int APINotesSigned;
#define APINOTES_TYPED_MACRO 42
#define APINOTES_TYPED_MACRO_ALIAS_TARGET 314
#define APINOTES_TYPED_MACRO_ALIAS APINOTES_TYPED_MACRO_ALIAS_TARGET
#define APINOTES_RENAMED_MACRO 17
#define APINOTES_RETYPED_RENAMED_MACRO 19

struct PointStruct {
  double x, y;
};

typedef double real_t;

typedef struct {
  struct PointStruct topLeft, bottomRight;
} RectStruct;

extern double will_be_private;
