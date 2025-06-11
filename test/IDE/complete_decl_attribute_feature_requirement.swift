// This contains code completion test cases for features covered by experimental
// feature flags, and tests both the case when the feature is disabled and when
// it's enabled. When a feature becomes non-experimental, move its test cases
// into the normal complete_decl_attribute.swift test file.

// NOTE: There are currently no experimental features that need code completion
// testing, but this test file is being left in place for when it's needed
// again. At that time, please remove the ABIAttribute tests.
// REQUIRES: new_use_case

// REQUIRES: asserts

// RUN: %batch-code-completion -filecheck-additional-suffix _DISABLED
// RUN: %batch-code-completion -filecheck-additional-suffix _ENABLED \
// RUN:        -enable-experimental-feature ABIAttribute

// NOTE: Please do not include the ", N items" after "Begin completions". The
// item count creates needless merge conflicts given that an "End completions"
// line exists for each test.

@#^KEYWORD2^# func method(){}

// KEYWORD2:              Begin completions
// KEYWORD2_ENABLED-DAG:  Keyword/None:              abi[#Func Attribute#]; name=abi
// KEYWORD2_DISABLED-NOT: Keyword/None:              abi[#{{.*}} Attribute#]; name=abi
// KEYWORD2:              End completions

@#^KEYWORD3^# class C {}

// KEYWORD3:              Begin completions
// KEYWORD3_ENABLED-NOT:  Keyword/None:              abi[#{{.*}} Attribute#]; name=abi
// KEYWORD3_DISABLED-NOT: Keyword/None:              abi[#{{.*}} Attribute#]; name=abi
// KEYWORD3:              End completions

@#^KEYWORD3_2?check=KEYWORD3^#IB class C2 {}
// Same as KEYWORD3.

@#^KEYWORD4^# enum E {}
// KEYWORD4:              Begin completions
// KEYWORD4_ENABLED-NOT:  Keyword/None:              abi[#{{.*}} Attribute#]; name=abi
// KEYWORD4_DISABLED-NOT: Keyword/None:              abi[#{{.*}} Attribute#]; name=abi
// KEYWORD4:              End completions

@#^KEYWORD5^# struct S{}
// KEYWORD5:              Begin completions
// KEYWORD5_ENABLED-NOT:  Keyword/None:              abi[#{{.*}} Attribute#]; name=abi
// KEYWORD5_DISABLED-NOT: Keyword/None:              abi[#{{.*}} Attribute#]; name=abi
// KEYWORD5:              End completions

@#^ON_GLOBALVAR^# var globalVar
// ON_GLOBALVAR:              Begin completions
// ON_GLOBALVAR_ENABLED-DAG:  Keyword/None:              abi[#Var Attribute#]; name=abi
// ON_GLOBALVAR_DISABLED-NOT: Keyword/None:              abi[#{{.*}} Attribute#]; name=abi
// ON_GLOBALVAR:              End completions

struct _S {
  @#^ON_INIT^# init()
// ON_INIT:              Begin completions
// ON_INIT_ENABLED-DAG:  Keyword/None:              abi[#Constructor Attribute#]; name=abi
// ON_INIT_DISABLED-NOT: Keyword/None:              abi[#{{.*}} Attribute#]; name=abi
// ON_INIT:              End completions

  @#^ON_PROPERTY^# var foo
// ON_PROPERTY:              Begin completions
// ON_PROPERTY_ENABLED-DAG:  Keyword/None:              abi[#Var Attribute#]; name=abi
// ON_PROPERTY_DISABLED-NOT: Keyword/None:              abi[#{{.*}} Attribute#]; name=abi
// ON_PROPERTY:              End completions

  @#^ON_SUBSCR^# subscript
// ON_SUBSCR:              Begin completions
// ON_SUBSCR_ENABLED-DAG:  Keyword/None:              abi[#Declaration Attribute#]; name=abi
// ON_SUBSCR_DISABLED-NOT: Keyword/None:              abi[#{{.*}} Attribute#]; name=abi
// ON_SUBSCR:              End completions

  @#^ON_METHOD^# private
  func foo()
// ON_METHOD:              Begin completions
// ON_METHOD_ENABLED-DAG:  Keyword/None:              abi[#Func Attribute#]; name=abi
// ON_METHOD_DISABLED-NOT: Keyword/None:              abi[#{{.*}} Attribute#]; name=abi
// ON_METHOD:              End completions


  func bar(@#^ON_PARAM_1?check=ON_PARAM^#)
// ON_PARAM:              Begin completions
// ON_PARAM_ENABLED-NOT:  Keyword/None:              abi[#{{.*}} Attribute#]; name=abi
// ON_PARAM_DISABLED-NOT: Keyword/None:              abi[#{{.*}} Attribute#]; name=abi
// ON_PARAM:              End completions

  func bar(
    @#^ON_PARAM_2?check=ON_PARAM^#

    arg: Int
  )
// Same as ON_PARAM.

  @#^ON_MEMBER_INDEPENDENT_1?check=ON_MEMBER_LAST^#

  func dummy1() {}
// Same as ON_MEMBER_LAST.

  @#^ON_MEMBER_INDEPENDENT_2?check=ON_MEMBER_LAST^#
  func dummy2() {}
// Same as ON_MEMBER_LAST.


  @#^ON_MEMBER_LAST^#
// ON_MEMBER_LAST:              Begin completions
// ON_MEMBER_LAST_ENABLED-DAG:  Keyword/None:              abi[#Declaration Attribute#]; name=abi
// ON_MEMBER_LAST_DISABLED-NOT: Keyword/None:              abi[#{{.*}} Attribute#]; name=abi
// ON_MEMBER_LAST:              End completions
}

func takeClosure(_: () -> Void) {
  takeClosure { @#^IN_CLOSURE^# in
    print("x")
  }
}
// IN_CLOSURE:              Begin completions
// FIXME: Not valid in this position (but CompletionLookup can't tell that)
// IN_CLOSURE_ENABLED-DAG:  Keyword/None:              abi[#Declaration Attribute#]; name=abi
// IN_CLOSURE_DISABLED-NOT: Keyword/None:              abi[#{{.*}} Attribute#]; name=abi
// IN_CLOSURE:              End completions

@#^KEYWORD_INDEPENDENT_1?check=KEYWORD_LAST^#

func dummy1() {}
// Same as KEYWORD_LAST.

@#^KEYWORD_INDEPENDENT_2?check=KEYWORD_LAST^#
func dummy2() {}
// Same as KEYWORD_LAST.

@#^KEYWORD_LAST^#

// KEYWORD_LAST:              Begin completions
// KEYWORD_LAST_ENABLED-DAG:  Keyword/None:              abi[#Declaration Attribute#]; name=abi
// KEYWORD_LAST_DISABLED-NOT: Keyword/None:              abi[#Declaration Attribute#]; name=abi
// KEYWORD_LAST:              End completions
