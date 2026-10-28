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

using ConstIntRefTypealias = const int &;

void setConstStaticIntRefTypealias(ConstIntRefTypealias ref);

using IntRefTypealias = int &;

void setStaticIntRefTypealias(IntRefTypealias ref);

template<class T>
struct ClassTemplate {};

template<class T>
const ClassTemplate<T> &refToDependent() { return ClassTemplate<T>(); }

// We cannot import "_Atomic" types. Make sure we fail gracefully instead of
// crashing when we have an "_Atomic" type or a reference to one.
void dontImportAtomicRef(_Atomic(int)&) { }

void takeConstRef(const int &);
inline bool takeConstRefBool(const bool &b) { return b; }
inline void takeRefBool(bool &b) { b = true; }

template<class T>
T &refToTemplate(T &t) { return t; }

template<class T>
const T &constRefToTemplate(const T &t) { return t; }

template<class T>
void refToDependentParam(ClassTemplate<T> &param) { }

inline unsigned sumArrayRef4(const unsigned char (&a)[4]) {
  return a[0] + a[1] + a[2] + a[3];
}

inline unsigned sumArrayRValueRef4(unsigned char (&&a)[4]) {
  return a[0] + a[1] + a[2] + a[3];
}

// A typedef of a typedef of an array, mirroring the shape of Darwin's uuid_t.
typedef unsigned char ByteArray16[16];
typedef ByteArray16 ByteArray16Typealias;

inline unsigned firstByteOfArrayRefTypealias(const ByteArray16Typealias &a) {
  return a[0];
}

inline void callWithIntRef(void (*callback)(int &)) {
  int value = 42;
  callback(value);
  setStaticInt(value);
}

inline void callWithConstIntRef(void (*callback)(const int &)) {
  int value = 43;
  callback(value);
}

inline void callWithIntRvalueRef(void (*callback)(int &&)) {
  int value = 44;
  callback(static_cast<int &&>(value));
}

inline void callWithConstIntRvalueRef(void (*callback)(const int &&)) {
  int value = 45;
  callback(static_cast<const int &&>(value));
}

#endif // TEST_INTEROP_CXX_REFERENCE_INPUTS_REFERENCE_H
