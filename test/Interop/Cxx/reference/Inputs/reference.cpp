#include "reference.h"
#include <utility>

static int staticInt = 42;

int getStaticInt() { return staticInt; }
int &getStaticIntRef() { return staticInt; }
int &&getStaticIntRvalueRef() { return std::move(staticInt); }
const int &getConstStaticIntRef() { return staticInt; }
const int &&getConstStaticIntRvalueRef() { return std::move(staticInt); }

void setStaticInt(int i) { staticInt = i; }
void setStaticIntRef(int &i) { staticInt = i; }
void setStaticIntRvalueRef(int &&i) { staticInt = i; }
void setConstStaticIntRef(const int &i) { staticInt = i; }
void setConstStaticIntRvalueRef(const int &&i) { staticInt = i; }

auto getFuncRef() -> int (&)() { return getStaticInt; }
auto getFuncRvalueRef() -> int (&&)() { return getStaticInt; }
