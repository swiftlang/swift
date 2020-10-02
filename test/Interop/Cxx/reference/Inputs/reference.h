#ifndef TEST_INTEROP_CXX_REFERENCE_INPUTS_REFERENCE_H
#define TEST_INTEROP_CXX_REFERENCE_INPUTS_REFERENCE_H

int getStaticInt();
int &getStaticIntRef();
int &&getStaticIntRvalueRef();
const int &getConstStaticIntRef();
const int &&getConstStaticIntRvalueRef();

void setStaticInt(int);
void setStaticIntRef(int &);
void setStaticIntRvalueRef(int &&);
void setConstStaticIntRef(const int &);
void setConstStaticIntRvalueRef(const int &&);

auto getFuncRef() -> int (&)();
auto getFuncRvalueRef() -> int (&&)();

#endif // TEST_INTEROP_CXX_REFERENCE_INPUTS_REFERENCE_H
