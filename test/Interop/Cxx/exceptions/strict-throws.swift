// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck %t/check.swift -I %t -cxx-interoperability-mode=default -cxx-exception-mode=strict -enable-experimental-feature CxxExceptionBridging -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -typecheck %t/annotated.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging
// RUN: %target-swift-frontend -emit-sil %t/use.swift -I %t -cxx-interoperability-mode=default -cxx-exception-mode=strict -enable-experimental-feature CxxExceptionBridging -strict-memory-safety -warnings-as-errors

// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

//--- module.modulemap
module StrictThrows {
  header "functions.h"
  requires cplusplus
}

//--- functions.h
#include <functional>
#include <stdexcept>
#define SWIFT_THROWS __attribute__((swift_attr("import_throws")))
inline int checkedValue(int value) {
  if (value < 0)
    throw std::runtime_error("negative value");
  return value;
}
inline void unknownException() { throw 17; }
inline int noThrow(int value) noexcept { return value; }
inline int mayThrow(int value) noexcept(false) { return checkedValue(value); }
inline int dynamicNone(int value) throw() { return value; }
inline int annotatedNoexcept(int value) noexcept SWIFT_THROWS { return value; }
inline int defaultValue() { throw std::runtime_error("default argument"); }
inline int safeWithDefault(int value = defaultValue()) noexcept { return value; }
inline int checkedWithDefault(int value = 42) { return checkedValue(value); }
struct StaticApi {
  static int checked(int value) { return checkedValue(value); }
  static int safe(int value) noexcept { return value; }
};
template <bool DoesNotThrow> struct ConditionalStatic {
  static int get(int value) noexcept(DoesNotThrow) {
    return checkedValue(value);
  }
};
using SafeTemplate = ConditionalStatic<true>;
using ThrowingTemplate = ConditionalStatic<false>;
static_assert(sizeof(SafeTemplate) > 0 && sizeof(ThrowingTemplate) > 0,
              "instantiate conditional exception specifications");
using SafeCallback = int (*)(int) noexcept;
inline SafeCallback safeCallback() noexcept { return &noThrow; }
using Callback = int (*)();
Callback callbackResult() noexcept;
Callback *callbackPointerResult() noexcept;
void callbackParameter(Callback callback) noexcept;
extern Callback globalCallback;
extern Callback callbackArray[2];
struct CallbackHolder { Callback callback; };
using FunctionIntToInt = std::function<int(int)>;
static_assert(sizeof(FunctionIntToInt) > 0, "instantiate the callable type");
extern "C" {
inline int cFunction(int value) { return value; }
static inline int cStaticFunction(int value) { return value; }
static inline void cCallback(void (*callback)()) {}
}

//--- check.swift
import StrictThrows
func check() throws {
  _ = checkedValue(1) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'}} expected-note {{did you mean to handle error as optional value}} expected-note {{did you mean to disable error propagation}}
  _ = mayThrow(1) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'}} expected-note {{did you mean to handle error as optional value}} expected-note {{did you mean to disable error propagation}}
  _ = StaticApi.checked(1) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'}} expected-note {{did you mean to handle error as optional value}} expected-note {{did you mean to disable error propagation}}
  _ = annotatedNoexcept(1) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'}} expected-note {{did you mean to handle error as optional value}} expected-note {{did you mean to disable error propagation}}
  _ = ThrowingTemplate.get(1) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'}} expected-note {{did you mean to handle error as optional value}} expected-note {{did you mean to disable error propagation}}
  _ = SafeTemplate.get(1)
  _ = noThrow(1)
  _ = dynamicNone(1)
  _ = StaticApi.safe(1)
  _ = cFunction(1)
  _ = cStaticFunction(1)
  cCallback(nil)
  _ = safeWithDefault() // expected-error {{missing argument for parameter #1 in call}}
  _ = try checkedWithDefault() // expected-error {{missing argument for parameter #1 in call}}
  _ = callbackResult() // expected-error {{cannot find 'callbackResult' in scope}}
  _ = callbackPointerResult() // expected-error {{cannot find 'callbackPointerResult' in scope}}
  callbackParameter(nil) // expected-error {{cannot find 'callbackParameter' in scope}}
  _ = globalCallback // expected-error {{'globalCallback' is unavailable: potentially throwing C++ callable types are not supported in strict C++ exception mode}}
  _ = callbackArray // expected-error {{'callbackArray' is unavailable: potentially throwing C++ callable types are not supported in strict C++ exception mode}}
}
func checkField(_ holder: CallbackHolder) {
  _ = holder.callback // expected-error {{'callback' is unavailable: potentially throwing C++ callable types are not supported in strict C++ exception mode}}
}
func checkClosureConstructor() {
  _ = FunctionIntToInt { $0 } // expected-error {{'init(_:)' is unavailable: constructing C++ function objects from Swift closures is not supported in strict C++ exception mode}}
}

//--- annotated.swift
import StrictThrows
let _: (CInt) -> CInt = checkedValue
let _: (CInt) -> CInt = StaticApi.checked
let _: (CInt) throws -> CInt = annotatedNoexcept

//--- use.swift
import StrictThrows
public func use(_ value: CInt) throws -> CInt {
  let result = try checkedValue(value)
  let next = try mayThrow(result)
  let last = try StaticApi.checked(next)
  return noThrow(last)
}
