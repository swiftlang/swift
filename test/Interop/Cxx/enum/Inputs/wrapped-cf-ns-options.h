#ifndef TEST_INTEROP_CXX_ENUM_INPUTS_WRAPPED_CF_NS_OPTIONS_H
#define TEST_INTEROP_CXX_ENUM_INPUTS_WRAPPED_CF_NS_OPTIONS_H

// Taken from CoreFoundation's CFAvailability.h.
#define __CF_OPTIONS_ATTRIBUTES                                                \
  __attribute__((flag_enum, enum_extensibility(open)))
#ifdef __cplusplus
#define CF_OPTIONS(_type, _name)                                               \
  __attribute__((availability(swift, unavailable))) _type _name;               \
  enum __CF_OPTIONS_ATTRIBUTES : _name
#else
#define CF_OPTIONS(_type, _name)                                               \
  enum __CF_OPTIONS_ATTRIBUTES _name : _type _name;                            \
  enum _name : _type
#endif

// Taken from Foundation's NSObjCRuntime.h.
#define NS_OPTIONS(_type, _name) CF_OPTIONS(_type, _name)

// Single function-like wrappers

#define INT_CF_OPTS(_type, _name) CF_OPTIONS(_type, _name)
#define INT_NS_OPTS(_type, _name) NS_OPTIONS(_type, _name)

typedef INT_CF_OPTS(unsigned int, MyCFOpts){
    MyCFOptsNone = 0,
    MyCFOptsFoo = 1,
    MyCFOptsBar = 2,
};

typedef INT_NS_OPTS(unsigned int, MyNSOpts){
    MyNSOptsNone = 0,
    MyNSOptsFoo = 1,
    MyNSOptsBar = 2,
};

// Two levels of function-like wrappers

#define INT_CF_OPTS2(_type, _name) INT_CF_OPTS(_type, _name)
#define INT_NS_OPTS2(_type, _name) INT_NS_OPTS(_type, _name)

typedef INT_CF_OPTS2(unsigned int, MyCFOpts2){
    MyCFOpts2None = 0,
    MyCFOpts2Foo = 1,
    MyCFOpts2Bar = 2,
};

typedef INT_NS_OPTS2(unsigned int, MyNSOpts2){
    MyNSOpts2None = 0,
    MyNSOpts2Foo = 1,
    MyNSOpts2Bar = 2,
};

// Object-like outer, function-like inner

#define OBJ_CF_OPTS INT_CF_OPTS
#define OBJ_NS_OPTS INT_NS_OPTS

typedef OBJ_CF_OPTS(unsigned int, MyCFOptsObj){
    MyCFOptsObjNone = 0,
    MyCFOptsObjFoo = 1,
    MyCFOptsObjBar = 2,
};

typedef OBJ_NS_OPTS(unsigned int, MyNSOptsObj){
    MyNSOptsObjNone = 0,
    MyNSOptsObjFoo = 1,
    MyNSOptsObjBar = 2,
};

// Function-like outer, object-like inner

#define CF_OPTIONS_ALIAS CF_OPTIONS
#define NS_OPTIONS_ALIAS NS_OPTIONS
#define FUNC_CF_OPTS(_type, _name) CF_OPTIONS_ALIAS(_type, _name)
#define FUNC_NS_OPTS(_type, _name) NS_OPTIONS_ALIAS(_type, _name)

typedef FUNC_CF_OPTS(unsigned int, MyCFOptsFunc){
    MyCFOptsFuncNone = 0,
    MyCFOptsFuncFoo = 1,
    MyCFOptsFuncBar = 2,
};

typedef FUNC_NS_OPTS(unsigned int, MyNSOptsFunc){
    MyNSOptsFuncNone = 0,
    MyNSOptsFuncFoo = 1,
    MyNSOptsFuncBar = 2,
};

// C stuff

#ifdef __cplusplus
extern "C" {
#endif
void printCFOpts(MyCFOpts opts);
MyCFOpts makeCFOpts(void);
void printNSOpts(MyNSOpts opts);
MyNSOpts makeNSOpts(void);

void printCFOpts2(MyCFOpts2 opts);
MyCFOpts2 makeCFOpts2(void);
void printNSOpts2(MyNSOpts2 opts);
MyNSOpts2 makeNSOpts2(void);

void printCFOptsObj(MyCFOptsObj opts);
MyCFOptsObj makeCFOptsObj(void);
void printNSOptsObj(MyNSOptsObj opts);
MyNSOptsObj makeNSOptsObj(void);

void printCFOptsFunc(MyCFOptsFunc opts);
MyCFOptsFunc makeCFOptsFunc(void);
void printNSOptsFunc(MyNSOptsFunc opts);
MyNSOptsFunc makeNSOptsFunc(void);
#ifdef __cplusplus
}
#endif

#endif // TEST_INTEROP_CXX_ENUM_INPUTS_WRAPPED_CF_NS_OPTIONS_H
