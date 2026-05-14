void moveToPoint(int, int, int);
void jumpToPoint(double x);

extern int ANTGlobalValue;
extern int ANTGlobalValue2;
extern int ANTGlobalValue3;
extern int ANTGlobalValue4;
extern int ANTGlobalValue5;

struct PointStruct {
  double x, y;
};

typedef double real_t;

typedef struct {
  struct PointStruct topLeft, bottomRight;
} RectStruct;

extern double will_be_private;
