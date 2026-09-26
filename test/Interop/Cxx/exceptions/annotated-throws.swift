// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck %t/check.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -typecheck %t/disabled.swift -I %t -cxx-interoperability-mode=default -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -typecheck %t/c-only.swift -I %t -enable-experimental-feature CxxExceptionBridging -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -emit-sil %t/use.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -strict-memory-safety -warnings-as-errors | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend -emit-ir %t/use.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging | %FileCheck %s --check-prefix=IR
// RUN: %target-swift-ide-test -print-module -module-to-print=AnnotatedThrows -I %t -source-filename=x -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging | %FileCheck %s --check-prefix=INTERFACE

// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

//--- module.modulemap
module AnnotatedThrows {
  header "functions.h"
  requires cplusplus
}

module AnnotatedThrowsC {
  header "c-functions.h"
}

//--- c-functions.h
int cChecked(int value) __attribute__((swift_attr("import_throws")));

//--- functions.h
#include <iterator>
#include <stdexcept>

// The importer defines this only when it imports SWIFT_THROWS.
#ifdef __swift_cxx_throws__
inline int cxxThrowsMacroIsDefined() { return 1; }
#endif

#define SWIFT_THROWS __attribute__((swift_attr("import_throws")))

inline int checkedDivide(int numerator, int denominator) SWIFT_THROWS {
  if (!denominator)
    throw std::runtime_error("division by zero");
  return numerator / denominator;
}

inline void checkedVoid(bool fail) SWIFT_THROWS {
  if (fail)
    throw 42;
}

inline int unchanged(int value) { return value; }

namespace Numbers {
inline double checked(double value) SWIFT_THROWS {
  if (value < 0)
    throw std::runtime_error("negative floating point value");
  return value;
}
}
inline int checkedNoexcept(int value) noexcept SWIFT_THROWS { return value; }
__attribute__((const)) inline int checkedConst(int value) SWIFT_THROWS {
  return value;
}
inline int checkedUnnamed(int, int value) SWIFT_THROWS { return value; }
inline void failUnnamed(int, int) SWIFT_THROWS {
  throw std::runtime_error("unnamed parameters");
}
inline void violateNoexcept() noexcept SWIFT_THROWS {
  checkedVoid(true);
}

inline int cleanupCount = 0;
struct CountCleanup {
  ~CountCleanup() noexcept { ++cleanupCount; }
};
inline void throwWithCleanup() SWIFT_THROWS {
  CountCleanup cleanup;
  throw std::runtime_error("cleanup");
}
inline int getCleanupCount() noexcept { return cleanupCount; }
int checkedDefault(int value = 1) SWIFT_THROWS;

// The annotation may be on any declaration of the function.
int annotatedFirst(int value) SWIFT_THROWS;
inline int annotatedFirst(int value) {
  if (value < 0)
    throw std::runtime_error("annotated first");
  return value;
}
int annotatedLast(int value);
inline int annotatedLast(int value) SWIFT_THROWS {
  if (value < 0)
    throw std::runtime_error("annotated last");
  return value;
}

__attribute__((swift_name("renamedChecked(_:)"))) inline int
originalChecked(int value) SWIFT_THROWS {
  if (value < 0)
    throw std::runtime_error("renamed");
  return value;
}

inline int overloaded(int value) SWIFT_THROWS {
  if (value < 0)
    throw std::runtime_error("int overload");
  return value;
}
inline double overloaded(double value) SWIFT_THROWS {
  if (value < 0)
    throw std::runtime_error("double overload");
  return value;
}

enum class Sign { negative = -1, positive = 1 };
inline int checkedSign(Sign sign) SWIFT_THROWS {
  if (sign == Sign::negative)
    throw std::runtime_error("negative sign");
  return 1;
}

struct StaticFunctions {
  static int checked(int value) SWIFT_THROWS {
    if (value < 0)
      throw std::runtime_error("negative value");
    return value;
  }
  static int unnamed(int, int value) SWIFT_THROWS { return value; }
};

template <class T>
struct Wrapper {
  static T checked(T value) SWIFT_THROWS {
    if (value < 0)
      throw std::runtime_error("negative wrapped value");
    return value;
  }
};
using IntWrapper = Wrapper<int>;

struct Unsupported {
  int value;
  int instance() SWIFT_THROWS;
};
struct AnnotatedConstructor {
  int value;
  AnnotatedConstructor(int value) SWIFT_THROWS : value(value) {}
};
void referenceParameter(int &value) SWIFT_THROWS;
void constReferenceParameter(const int &value) SWIFT_THROWS;
void pointerParameter(int *value) SWIFT_THROWS;
int *pointerResult() SWIFT_THROWS;
void variadic(int count, ...) SWIFT_THROWS;
Unsupported aggregateResult() SWIFT_THROWS;
void aggregateParameter(Unsupported value) SWIFT_THROWS;
[[noreturn]] void neverReturns() SWIFT_THROWS;
enum class EnumResult { one = 1, two = 2 };
EnumResult enumResult() SWIFT_THROWS;
enum UnscopedEnumResult { unscopedOne = 1 };
UnscopedEnumResult unscopedEnumResult() SWIFT_THROWS;
template <class T>
T checkedTemplate(T value) SWIFT_THROWS;
extern int throwingVariable SWIFT_THROWS;

struct UnsupportedOperators {
  int operator*() const SWIFT_THROWS;
  UnsupportedOperators &operator++() SWIFT_THROWS;
  explicit operator bool() const SWIFT_THROWS;
  int operator[](int index) const SWIFT_THROWS;
};

// Annotated members must not witness the nonthrowing requirements of the
// automatically derived iterator and collection conformances.
struct Iterator {
  using iterator_category = std::random_access_iterator_tag;
  using value_type = int;
  using pointer = const int *;
  using reference = const int &;
  using difference_type = int;
  const int *value;
  const int &operator*() const { return *value; }
  Iterator &operator++() {
    ++value;
    return *this;
  }
  bool operator==(const Iterator &other) const { return value == other.value; }
  int operator-(const Iterator &other) const { return value - other.value; }
  void operator+=(int offset) { value += offset; }
};

struct ThrowingEqualIterator {
  using iterator_category = std::input_iterator_tag;
  using value_type = int;
  using pointer = const int *;
  using reference = const int &;
  using difference_type = int;
  const int *value;
  const int &operator*() const { return *value; }
  ThrowingEqualIterator &operator++() {
    ++value;
    return *this;
  }
  bool operator==(const ThrowingEqualIterator &other) const SWIFT_THROWS;
};

struct ThrowingGlobalEqualIterator {
  using iterator_category = std::input_iterator_tag;
  using value_type = int;
  using pointer = const int *;
  using reference = const int &;
  using difference_type = int;
  const int *value;
  const int &operator*() const { return *value; }
  ThrowingGlobalEqualIterator &operator++() {
    ++value;
    return *this;
  }
};
bool operator==(const ThrowingGlobalEqualIterator &,
                const ThrowingGlobalEqualIterator &) SWIFT_THROWS;

struct ThrowingMinusIterator {
  using iterator_category = std::random_access_iterator_tag;
  using value_type = int;
  using pointer = const int *;
  using reference = const int &;
  using difference_type = int;
  const int *value;
  const int &operator*() const { return *value; }
  ThrowingMinusIterator &operator++() {
    ++value;
    return *this;
  }
  bool operator==(const ThrowingMinusIterator &other) const {
    return value == other.value;
  }
  int operator-(const ThrowingMinusIterator &other) const SWIFT_THROWS;
  void operator+=(int offset) { value += offset; }
};

struct MutableIterator {
  using iterator_category = std::random_access_iterator_tag;
  using value_type = int;
  using pointer = int *;
  using reference = int &;
  using difference_type = int;
  int *value;
  int &operator*() const { return *value; }
  MutableIterator &operator++() {
    ++value;
    return *this;
  }
  bool operator==(const MutableIterator &other) const {
    return value == other.value;
  }
  int operator-(const MutableIterator &other) const {
    return value - other.value;
  }
  void operator+=(int offset) { value += offset; }
};

struct Collection {
  int values[2];
  Iterator begin() const { return {values}; }
  Iterator end() const { return {values + 2}; }
  MutableIterator begin() { return {values}; }
  MutableIterator end() { return {values + 2}; }
};

struct ThrowingCollection {
  int values[2];
  Iterator begin() const SWIFT_THROWS;
  Iterator end() const SWIFT_THROWS;
};

struct ThrowingMutableCollection {
  int values[2];
  Iterator begin() const { return {values}; }
  Iterator end() const { return {values + 2}; }
  MutableIterator begin() SWIFT_THROWS;
  MutableIterator end() SWIFT_THROWS;
};

//--- check.swift
import AnnotatedThrows
import Cxx

func check() throws {
  _ = checkedDivide(12, 3) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  checkedVoid(false) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  _ = StaticFunctions.checked(1) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  _ = unchanged(1)
  let _: (Double) throws -> Double = Numbers.checked
  _ = checkedNoexcept(1) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  _ = checkedDefault() // expected-error {{'checkedDefault' is unavailable: SWIFT_THROWS on functions with default arguments is not yet supported}}
  let _: (CInt, CInt) throws -> CInt = checkedDivide
  let _: (Bool) throws -> Void = checkedVoid
  let _: (CInt) throws -> CInt = StaticFunctions.checked
  _ = aggregateResult() // expected-error {{'aggregateResult()' is unavailable: SWIFT_THROWS currently requires arithmetic or enum parameters and an arithmetic or void result}}
  aggregateParameter(Unsupported()) // expected-error {{'aggregateParameter' is unavailable: SWIFT_THROWS currently requires arithmetic or enum parameters and an arithmetic or void result}}
  _ = enumResult() // expected-error {{'enumResult()' is unavailable: SWIFT_THROWS currently requires arithmetic or enum parameters and an arithmetic or void result}}
  _ = unscopedEnumResult() // expected-error {{'unscopedEnumResult()' is unavailable: SWIFT_THROWS currently requires arithmetic or enum parameters and an arithmetic or void result}}
  _ = checkedTemplate(CInt(1)) // expected-error {{'checkedTemplate' is unavailable: SWIFT_THROWS is not supported on this kind of declaration}}
  _ = throwingVariable // expected-error {{'throwingVariable' is unavailable: SWIFT_THROWS is not supported on this kind of declaration}}
  let operators = UnsupportedOperators()
  _ = operators.pointee // expected-error {{value of type 'UnsupportedOperators' has no member 'pointee'}}
  _ = operators.successor() // expected-error {{value of type 'UnsupportedOperators' has no member 'successor'}}
  _ = operators[1] // expected-error {{value of type 'UnsupportedOperators' has no subscripts}}
  _ = Bool(fromCxx: operators) // expected-error {{initializer 'init(fromCxx:)' requires that 'UnsupportedOperators' conform to 'CxxConvertibleToBool'}}
  var unsupported = Unsupported()
  _ = try unsupported.instance()
}

func checkDeclarationVariants() throws {
  _ = annotatedFirst(1) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  _ = annotatedLast(1) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  _ = IntWrapper.checked(1) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  _ = try renamedChecked(1)
  _ = originalChecked(1) // expected-error {{'originalChecked' has been renamed to 'renamedChecked(_:)'}}
  let _: (CInt) throws -> CInt = overloaded
  let _: (Double) throws -> Double = overloaded
  let _: (Sign) throws -> CInt = checkedSign
  _ = cxxThrowsMacroIsDefined()
}

func checkUnsupportedSignatures() {
  _ = AnnotatedConstructor(1) // expected-error {{'init(_:)' is unavailable: SWIFT_THROWS is not supported on this kind of declaration}}
  var value: CInt = 0
  referenceParameter(&value) // expected-error {{'referenceParameter' is unavailable: SWIFT_THROWS currently requires arithmetic or enum parameters and an arithmetic or void result}}
  constReferenceParameter(value) // expected-error {{'constReferenceParameter' is unavailable: SWIFT_THROWS currently requires arithmetic or enum parameters and an arithmetic or void result}}
  pointerParameter(&value) // expected-error {{'pointerParameter' is unavailable: SWIFT_THROWS currently requires arithmetic or enum parameters and an arithmetic or void result}}
  _ = pointerResult() // expected-error {{'pointerResult()' is unavailable: SWIFT_THROWS currently requires arithmetic or enum parameters and an arithmetic or void result}}
  variadic(1) // expected-error {{'variadic' is unavailable: Variadic function is unavailable}}
}

func requiresIterator<T: UnsafeCxxInputIterator>(_: T) {} // expected-note {{where 'T' = 'ThrowingEqualIterator'}} expected-note {{where 'T' = 'ThrowingGlobalEqualIterator'}}
func requiresRandomAccessIterator<T: UnsafeCxxRandomAccessIterator>(_: T) {} // expected-note {{where 'T' = 'ThrowingMinusIterator'}}
func requiresCollection<T: CxxRandomAccessCollection>(_: T) {} // expected-note {{where 'T' = 'ThrowingCollection'}}
func requiresMutableCollection<T: CxxMutableRandomAccessCollection>(_: T) {} // expected-note {{where 'T' = 'ThrowingMutableCollection'}}

func checkConformances() {
  requiresRandomAccessIterator(Iterator())
  requiresIterator(ThrowingEqualIterator()) // expected-error {{global function 'requiresIterator' requires that 'ThrowingEqualIterator' conform to 'UnsafeCxxInputIterator'}}
  requiresIterator(ThrowingGlobalEqualIterator()) // expected-error {{global function 'requiresIterator' requires that 'ThrowingGlobalEqualIterator' conform to 'UnsafeCxxInputIterator'}}
  requiresIterator(ThrowingMinusIterator())
  requiresRandomAccessIterator(ThrowingMinusIterator()) // expected-error {{global function 'requiresRandomAccessIterator' requires that 'ThrowingMinusIterator' conform to 'UnsafeCxxRandomAccessIterator'}}
  requiresMutableCollection(Collection())
  requiresCollection(ThrowingCollection()) // expected-error {{global function 'requiresCollection' requires that 'ThrowingCollection' conform to 'CxxRandomAccessCollection'}}
  requiresCollection(ThrowingMutableCollection())
  requiresMutableCollection(ThrowingMutableCollection()) // expected-error {{global function 'requiresMutableCollection' requires that 'ThrowingMutableCollection' conform to 'CxxMutableRandomAccessCollection'}}
}

func checkNoReturn() {
  neverReturns() // expected-error {{'neverReturns()' is unavailable: SWIFT_THROWS is not supported on this kind of declaration}}
}

//--- disabled.swift
import AnnotatedThrows
import Cxx

// Without CxxExceptionBridging the importer ignores the annotation, as older
// compilers do. <swift/bridging> makes SWIFT_THROWS declarations unavailable
// in that case, because __swift_cxx_throws__ is not defined.
func requiresIterator<T: UnsafeCxxInputIterator>(_: T) {}
func requiresRandomAccessIterator<T: UnsafeCxxRandomAccessIterator>(_: T) {}
func requiresCollection<T: CxxRandomAccessCollection>(_: T) {}
func requiresMutableCollection<T: CxxMutableRandomAccessCollection>(_: T) {}

func checkDisabled() {
  let _: (CInt, CInt) -> CInt = checkedDivide
  checkedVoid(false)
  _ = StaticFunctions.checked(1)
  _ = checkedDefault()
  _ = AnnotatedConstructor(1)
  var unsupported = Unsupported()
  _ = unsupported.instance()
  _ = throwingVariable
  let operators = UnsupportedOperators()
  _ = operators.pointee
  _ = operators[1]
  _ = Bool(fromCxx: operators)
  requiresIterator(ThrowingEqualIterator())
  requiresIterator(ThrowingGlobalEqualIterator())
  requiresRandomAccessIterator(ThrowingMinusIterator())
  requiresCollection(ThrowingCollection())
  requiresMutableCollection(ThrowingMutableCollection())
  _ = cxxThrowsMacroIsDefined() // expected-error {{cannot find 'cxxThrowsMacroIsDefined' in scope}}
}

//--- c-only.swift
import AnnotatedThrowsC

func checkCOnly() {
  _ = cChecked(1) // expected-error {{'cChecked' is unavailable: SWIFT_THROWS requires C++ interoperability}}
}

//--- use.swift
import AnnotatedThrows

public func direct(_ numerator: CInt, _ denominator: CInt) throws -> CInt {
  return try checkedDivide(numerator, denominator)
}

public func captured() -> (CInt, CInt) throws -> CInt {
  return checkedDivide
}

public func voidResult(_ fail: Bool) throws {
  try checkedVoid(fail)
}

public func staticMethod(_ value: CInt) throws -> CInt {
  try StaticFunctions.checked(value)
}

public func unnamedArguments(_ value: CInt) throws -> CInt {
  try checkedUnnamed(0, value) + StaticFunctions.unnamed(0, value)
}

public func constFunction(_ value: CInt) throws -> CInt {
  try checkedConst(value)
}

public func enumParameter() throws -> CInt {
  try checkedSign(.positive)
}

// SIL: sil shared [transparent] {{.*}}checkedDivide{{.*}} : $@convention(thin) (Int32, Int32) -> (Int32, @error any Error)
// SIL: try_apply

// Unnamed parameters get internal names so the closure can capture them.
// SIL-LABEL: sil shared [transparent] @$sSC14checkedUnnamedys5Int32VAC_ACtKF : $@convention(thin) (Int32, Int32) -> (Int32, @error any Error)
// SIL-NEXT: // %0 "__cxx_arg0"
// SIL-NEXT: // %1 "value"
// SIL: partial_apply [callee_guaranteed] [on_stack] %{{[0-9]+}}(%0, %1)
// SIL-LABEL: sil shared [transparent] @$sSo15StaticFunctionsV7unnamedys5Int32VAE_AEtKFZ : $@convention(method) (Int32, Int32, @thin StaticFunctions.Type) -> (Int32, @error any Error)
// SIL-NEXT: // %0 "__cxx_arg0"
// SIL-NEXT: // %1 "value"
// SIL: partial_apply [callee_guaranteed] [on_stack] %{{[0-9]+}}(%0, %1)
// The facade must not inherit the effects of __attribute__((const)).
// SIL-LABEL: sil shared [transparent] @$sSC12checkedConstys5Int32VACKF : $@convention(thin) (Int32) -> (Int32, @error any Error)
// SIL-LABEL: sil shared [transparent] @$sSC11checkedSignys5Int32VSo0B0VKF : $@convention(thin) (Sign) -> (Int32, @error any Error)
// SIL-LABEL: sil shared @$sSC14checkedUnnamedys5Int32VAC_ACtKFACSvSg_yAD_SPys4Int8VGSgtXCtXEfU_ :
// SIL: apply %{{[0-9]+}}(%3, %4, %1, %{{[0-9]+}}) : $@convention(c) (Int32, Int32, Optional<UnsafeMutableRawPointer>,
// An enum parameter is passed to the adapter unchanged.
// SIL-LABEL: sil shared @$sSC11checkedSignys5Int32VSo0B0VKFACSvSg_yAF_SPys4Int8VGSgtXCtXEfU_ :
// SIL: apply %{{[0-9]+}}(%3, %1, %{{[0-9]+}}) : $@convention(c) (Sign, Optional<UnsafeMutableRawPointer>,
// IR: define internal{{.*}} @{{.*}}__swift_cxx_exception_
// IR: invoke{{.*}} @_Z13checkedDivideii
// IR: catch ptr null
// IR: __swift_cxx_report_current_exception

// INTERFACE-DAG: func checkedDivide(_ numerator: CInt, _ denominator: CInt) throws -> CInt
// INTERFACE-DAG: func checkedVoid(_ fail: CBool) throws
// INTERFACE-DAG: func checkedNoexcept(_ value: CInt) throws -> CInt
// INTERFACE-DAG: static func checked(_ value: CDouble) throws -> CDouble
// INTERFACE-DAG: static func checked(_ value: CInt) throws -> CInt
// INTERFACE-DAG: func unchanged(_ value: CInt) -> CInt
// INTERFACE-DAG: func annotatedFirst(_ value: CInt) throws -> CInt
// INTERFACE-DAG: func annotatedLast(_ value: CInt) throws -> CInt
// INTERFACE-DAG: func renamedChecked(_ value: CInt) throws -> CInt
// INTERFACE-DAG: func originalChecked(_ value: CInt) -> CInt
// INTERFACE-DAG: func overloaded(_ value: CInt) throws -> CInt
// INTERFACE-DAG: func overloaded(_ value: CDouble) throws -> CDouble
// INTERFACE-DAG: func checkedSign(_ sign: Sign) throws -> CInt
// INTERFACE-DAG: typealias IntWrapper = Wrapper<CInt>
