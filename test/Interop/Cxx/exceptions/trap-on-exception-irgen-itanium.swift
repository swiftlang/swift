// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-emit-ir %t/test.swift -I %t/Inputs -enable-experimental-cxx-interop | %FileCheck %s
// RUN: %target-swift-emit-ir %t/test.swift -I %t/Inputs -enable-experimental-cxx-interop -g | %FileCheck --check-prefix=DEBUG %s

// UNSUPPORTED: OS=windows-msvc

//--- Inputs/module.modulemap
module CxxModule {
    header "cxxHeader.h"
    requires cplusplus
}

//--- Inputs/cxxHeader.h

inline int freeFunctionThrows(int x) {
  if (x > 0)
    throw 2;
  return -x;
}

inline int freeFunctionNoThrow(int x) noexcept {
  return -x;
}

inline int freeFunctionCatchesException(int x) {
  try {
    return freeFunctionThrows(x);
  } catch (...) {
    return x;
  }
}

class TestClass {
  int m = 0;
public:
  int method(int x) const {
    return freeFunctionThrows(x);
  }
  int noExceptMethod(int x) noexcept {
    return freeFunctionNoThrow(x);
  }
};

template<class T>
struct IsNoExcept { const static bool value = false; };

template<>
struct IsNoExcept<int> { const static bool value = true; };

template<class T>
class TestTemplate {
  int m = 0;
public:
  void method(T x) const {
    throw x;
  }
  void noExceptMethod() noexcept {
  }
  void dependentNoExceptMethod() noexcept(IsNoExcept<T>::value) {
    if (!IsNoExcept<T>::value)
      throw 2;
  }
};

using TestTemplateInt = TestTemplate<int>;
using TestTemplateBool = TestTemplate<bool>;

//--- test.swift

import CxxModule

func makeCInt() -> CInt {
  return 42
}

func testFreeFunctionNoThrowOnly() -> CInt {
  return freeFunctionNoThrow(makeCInt())
}

func testFreeFunctionCalls() -> CInt {
  let p = freeFunctionThrows(0)
  freeFunctionNoThrow(1)
  freeFunctionThrows(makeCInt())
  return p
}

func testMethodCalls() -> CInt {
  var v = TestClass()
  let p = v.method(makeCInt())
  v.noExceptMethod(1)
  v.method(0)
  return p
}

func testTemplateCalls() {
  var v1 = TestTemplateInt()
  v1.method(2)
  v1.noExceptMethod()
  v1.dependentNoExceptMethod()
  var v2 = TestTemplateBool()
  v2.method(false)
  v2.noExceptMethod()
  v2.dependentNoExceptMethod()
}

let _ = testFreeFunctionNoThrowOnly()
let _ = testFreeFunctionCalls()
let _ = testMethodCalls()
testTemplateCalls()

// CHECK: define {{.*}} @"$s4test0A23FreeFunctionNoThrowOnlys5Int32VyF"() #[[#SWIFTMETA:]] {
// CHECK-NEXT: :
// CHECK-NEXT:  call swiftcc i32 @"$s4test8makeCInts5Int32VyF"()
// CHECK-NEXT:  call i32 @_Z19freeFunctionNoThrowi(i32 {{.*}})
// CHECK-NEXT:  ret i32
// CHECK-NEXT: }

// CHECK: define {{.*}} @"$s4test0A17FreeFunctionCallss5Int32VyF"() #[[#SWIFTUWMETA:]] personality i32 (...)* @__gxx_personality_v0
// CHECK:   invoke i32 @_Z18freeFunctionThrowsi(i32 0)
// CHECK-NEXT:  to label %[[CONT1:.*]] unwind label %[[UNWIND1:.*]]
// CHECK-EMPTY:
// CHECK: [[CONT1]]:
// CHECK: call i32 @_Z19freeFunctionNoThrowi(i32 1)
// CHECK-NEXT: %[[V1:.*]] = call swiftcc i32 @"$s4test8makeCInts5Int32VyF"()
// CHECK-NEXT: invoke i32 @_Z18freeFunctionThrowsi(i32 %[[V1]])
// CHECK-NEXT:    to label %[[CONT2:.*]] unwind label %[[UNWIND2:.*]]
// CHECK-EMPTY:
// CHECK: [[CONT2]]:
// CHECK:  ret
// CHECK-EMPTY:
// CHECK-NEXT: [[UNWIND1]]:
// CHECK-NEXT: landingpad { i8*, i32 }
// CHECK-NEXT:    catch i8* null
// CHECK-NEXT: call void @llvm.trap()
// CHECK-NEXT: unreachable
// CHECK-EMPTY:
// CHECK-NEXT: [[UNWIND2]]:
// CHECK-NEXT: landingpad { i8*, i32 }
// CHECK-NEXT:    catch i8* null
// CHECK-NEXT: call void @llvm.trap()
// CHECK-NEXT: unreachable
// CHECK-NEXT: }

// CHECK: define {{.*}} @"$s4test0A11MethodCallss5Int32VyF"() #[[#SWIFTUWMETA]] personality
// CHECK: call swiftcc i32 @"$s4test8makeCInts5Int32VyF"()
// CHECK: invoke i32 @_ZNK9TestClass6methodEi
// CHECK-NEXT:  to label %[[CONT3:.*]] unwind label %[[UNWIND3:.*]]
// CHECK: [[CONT3]]:
// CHECK: call i32 @_ZN9TestClass14noExceptMethodEi
// CHECK: invoke i32 @_ZNK9TestClass6methodEi
// CHECK-NEXT: to label %[[CONT4:.*]] unwind label %[[UNWIND4:.*]]
// CHECK: [[CONT4]]:
// CHECK: ret
// CHECK-EMPTY:
// CHECK-NEXT: [[UNWIND3]]:
// CHECK-NEXT: landingpad { i8*, i32 }
// CHECK-NEXT:    catch i8* null
// CHECK-NEXT: call void @llvm.trap()
// CHECK-NEXT: unreachable
// CHECK-EMPTY:
// CHECK-NEXT: [[UNWIND4]]:
// CHECK-NEXT: landingpad { i8*, i32 }
// CHECK-NEXT:    catch i8* null
// CHECK-NEXT: call void @llvm.trap()
// CHECK-NEXT: unreachable
// CHECK-NEXT: }

// CHECK: define {{.*}} @"$s4test0A13TemplateCallsyyF"() #[[#SWIFTUWMETA]] personality
// CHECK: invoke void @_ZNK12TestTemplateIiE6methodEi
// CHECK-NEXT:  to label %[[CONT10:.*]] unwind label
// CHECK: [[CONT10]]:
// CHECK: call void @_ZN12TestTemplateIiE14noExceptMethodEv
// CHECK: call void @_ZN12TestTemplateIiE23dependentNoExceptMethodEv
// CHECK: invoke void @_ZNK12TestTemplateIbE6methodEb
// CHECK-NEXT:  to label %[[CONT11:.*]] unwind label
// CHECK: [[CONT11]]:
// CHECK: call void @_ZN12TestTemplateIbE14noExceptMethodEv
// CHECK: invoke void @_ZN12TestTemplateIbE23dependentNoExceptMethodEv
// CHECK-NEXT:  to label %[[CONT12:.*]] unwind label
// CHECK: [[CONT12]]:
// CHECK: ret

// CHECK: i32 @__gxx_personality_v0(...)

// CHECK: attributes #[[#SWIFTMETA]] = {
// CHECK-NOT: uwtable
// CHECK: attributes #[[#SWIFTUWMETA]] = {
// CHECK-SAME: uwtable

// DEBUG: define {{.*}} @"$s4test0A17FreeFunctionCallss5Int32VyF"()
// DEBUG:   invoke i32 @_Z18freeFunctionThrowsi(i32 0)
// DEBUG-NEXT:  to label %[[CONT1:.*]] unwind label %[[UNWIND1:.*]], !dbg ![[#DEBUGLOC_FREEFUNCTIONTHROWS1:]]
// DEBUG-EMPTY:
// DEBUG: [[CONT1]]:
// DEBUG: call i32 @_Z19freeFunctionNoThrowi(i32 1)
// DEBUG-NEXT: %[[V1:.*]] = call swiftcc i32 @"$s4test8makeCInts5Int32VyF"()
// DEBUG-NEXT: invoke i32 @_Z18freeFunctionThrowsi(i32 %[[V1]])
// DEBUG-NEXT:    to label %[[CONT2:.*]] unwind label %[[UNWIND2:.*]], !dbg ![[#DEBUGLOC_FREEFUNCTIONTHROWS2:]]
// DEBUG-EMPTY:
// DEBUG: [[CONT2]]:
// DEBUG:  ret
// DEBUG-EMPTY:
// DEBUG-NEXT: [[UNWIND1]]:
// DEBUG-NEXT: landingpad { i8*, i32 }
// DEBUG-NEXT:    catch i8* null, !dbg ![[#DEBUGLOC_FREEFUNCTIONTHROWS1]]
// DEBUG-NEXT: call void @llvm.trap(), !dbg ![[#DEBUGLOC_TRAP1:]]
// DEBUG-NEXT: unreachable, !dbg ![[#DEBUGLOC_TRAP1]]
// DEBUG-EMPTY:
// DEBUG-NEXT: [[UNWIND2]]:
// DEBUG-NEXT: landingpad { i8*, i32 }
// DEBUG-NEXT:    catch i8* null, !dbg ![[#DEBUGLOC_FREEFUNCTIONTHROWS2]]
// DEBUG-NEXT: call void @llvm.trap(), !dbg ![[#DEBUGLOC_TRAP2:]]
// DEBUG-NEXT: unreachable, !dbg ![[#DEBUGLOC_TRAP2]]
// DEBUG-NEXT: }

// DEBUG: ![[#DEBUGLOC_FREEFUNCTIONTHROWS1]] = !DILocation(line: 13, column: 11,
// DEBUG: ![[#DEBUGLOC_FREEFUNCTIONTHROWS2]] = !DILocation(line: 15, column: 3,
// DEBUG: ![[#DEBUGLOC_TRAP1]] = !DILocation(line: 0, scope: ![[#TRAPSCOPE:]], inlinedAt: ![[#DEBUGLOC_FREEFUNCTIONTHROWS1]])
// DEBUG: ![[#TRAPSCOPE]] = distinct !DISubprogram(name: "Swift runtime failure: unhandled C++{{ / Objective-C | }}exception"
// DEBUG: ![[#DEBUGLOC_TRAP2]] = !DILocation(line: 0, scope: ![[#TRAPSCOPE]], inlinedAt: ![[#DEBUGLOC_FREEFUNCTIONTHROWS2]])
