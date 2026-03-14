
struct __attribute__((swift_attr("sensitive"))) SmallCStruct {
    unsigned a;
    unsigned b;
    unsigned c;
};

struct __attribute__((swift_attr("sensitive"))) LargeCStruct {
    unsigned a;
    unsigned b;
    unsigned c;
    unsigned d;
    unsigned e;
    unsigned f;
    unsigned g;
};

struct SmallCStruct getSmallStruct(int x);
struct LargeCStruct getLargeStruct(int x);

void printSmallStruct(int x, struct SmallCStruct s, int y);
struct SmallCStruct forwardSmallStruct(struct SmallCStruct s);
void printLargeStruct(int x, struct LargeCStruct s, int y);
struct LargeCStruct forwardLargeStruct(struct LargeCStruct s);

