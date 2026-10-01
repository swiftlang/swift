// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck %t/check.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -enable-experimental-feature CxxExceptionBridgingStrict -verify -verify-ignore-unrelated
// RUN: %target-build-swift %t/main.swift -I %t -o %t/test -cxx-interoperability-mode=default -Xfrontend -enable-experimental-feature -Xfrontend CxxExceptionBridging -Xfrontend -enable-experimental-feature -Xfrontend CxxExceptionBridgingStrict
// RUN: %target-codesign %t/test
// RUN: %target-run %t/test
// RUN: %target-build-swift %t/main.swift -I %t -o %t/test-opt -O -cxx-interoperability-mode=default -Xfrontend -enable-experimental-feature -Xfrontend CxxExceptionBridging -Xfrontend -enable-experimental-feature -Xfrontend CxxExceptionBridgingStrict
// RUN: %target-codesign %t/test-opt
// RUN: %target-run %t/test-opt

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: swift_feature_CxxExceptionBridgingStrict
// REQUIRES: OS=macosx || OS=linux-gnu

//--- module.modulemap
module StrictMembers {
  header "members.h"
  requires cplusplus
  export *
}

//--- members.h
#include <iterator>
#include <optional>
#include <stdexcept>
#include <vector>
inline int checkedMemberValue(int value) {
  if (value < 0)
    throw std::runtime_error("negative member value");
  return value;
}
struct Value {
  int value;
  Value(int value) : value(checkedMemberValue(value)) {}
  int read() const { return checkedMemberValue(value); }
  void update(int next) { value = checkedMemberValue(next); }
  int consume() && { return checkedMemberValue(value); }
  int safeRead() const noexcept { return value; }
};
struct Derived : Value { using Value::Value; };
inline int throwingDefault() { throw std::runtime_error("default member"); }
struct Defaulted {
  int value = throwingDefault();
  Defaulted() = default;
};
struct ImplicitDefaulted {
  int value = throwingDefault();
};
struct NoThrowDefault { int value = 17; };
struct DefaultArgument {
  int value;
  DefaultArgument(int value = throwingDefault()) noexcept : value(value) {}
};
struct NoThrowIteratorBase { const int *value; };
inline bool operator==(const NoThrowIteratorBase &lhs,
                       const NoThrowIteratorBase &rhs) noexcept {
  return lhs.value == rhs.value;
}
// operator== takes the base class, so the importer synthesizes a wrapper
// around it for the derived iterator.
struct InheritedEqualIterator : NoThrowIteratorBase {
  using iterator_category = std::input_iterator_tag;
  using value_type = int;
  using pointer = const int *;
  using reference = const int &;
  using difference_type = int;
  const int &operator*() const noexcept { return *value; }
  InheritedEqualIterator &operator++() noexcept {
    ++value;
    return *this;
  }
};
struct ThrowingEqualIterator {
  using iterator_category = std::input_iterator_tag;
  using value_type = int;
  using pointer = const int *;
  using reference = const int &;
  using difference_type = int;
  const int *value;
  const int &operator*() const noexcept { return *value; }
  ThrowingEqualIterator &operator++() noexcept {
    ++value;
    return *this;
  }
  bool operator==(const ThrowingEqualIterator &other) const {
    return value == other.value;
  }
};
struct NoThrowSequence {
  int values[2] = {1, 2};
  InheritedEqualIterator begin() const noexcept { return {{values}}; }
  InheritedEqualIterator end() const noexcept { return {{values + 2}}; }
};
struct ThrowingSequence {
  int values[2] = {1, 2};
  InheritedEqualIterator begin() const { return {{values}}; }
  InheritedEqualIterator end() const { return {{values + 2}}; }
};
struct __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:immortal")))
    __attribute__((swift_attr("release:immortal"))) NoThrowReference {
  explicit NoThrowReference(int) noexcept {}
};
struct __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:immortal")))
    __attribute__((swift_attr("release:immortal"))) VirtualReference {
  virtual int read(int value) const { return checkedMemberValue(value); }
  virtual ~VirtualReference() = default;
  static VirtualReference &create() noexcept {
    static VirtualReference value;
    return value;
  }
};

// Operators that back nonthrowing conveniences.
struct ThrowingBool {
  explicit operator bool() const { return checkedMemberValue(1) > 0; }
};
struct NoThrowBool {
  explicit operator bool() const noexcept { return true; }
};
struct ThrowingSubscript {
  int values[2] = {1, 2};
  int operator[](int index) const { return checkedMemberValue(values[index]); }
};
struct NoThrowSubscript {
  int values[2] = {1, 2};
  const int &operator[](int index) const noexcept { return values[index]; }
  int &operator[](int index) noexcept { return values[index]; }
};
struct ThrowingPointee {
  int value = 3;
  const int &operator*() const {
    checkedMemberValue(value);
    return value;
  }
};
struct NoThrowPointee {
  int value = 4;
  const int &operator*() const noexcept { return value; }
};

// A block type has no exception specification.
struct BlockHolder {
  int (^block)(int);
};

using IntVector = std::vector<int>;
using IntOptional = std::optional<int>;

//--- check.swift
@_spi(CxxExceptionBridging) import Cxx
import StrictMembers
func check(_ value: inout Value) throws {
  _ = Value(1) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'}} expected-note {{did you mean to handle error as optional value}} expected-note {{did you mean to disable error propagation}}
  _ = value.read() // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'}} expected-note {{did you mean to handle error as optional value}} expected-note {{did you mean to disable error propagation}}
  value.update(2) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'}} expected-note {{did you mean to handle error as optional value}} expected-note {{did you mean to disable error propagation}}
  _ = value.safeRead()
  _ = NoThrowDefault()
  _ = ImplicitDefaulted() // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'}} expected-note {{did you mean to handle error as optional value}} expected-note {{did you mean to disable error propagation}}
  _ = DefaultArgument() // expected-error {{missing argument for parameter #1 in call}}
}

func requiresIterator<T: UnsafeCxxInputIterator>(_: T) {} // expected-note {{where 'T' = 'ThrowingEqualIterator'}}
func requiresSequence<T: CxxConvertibleToCollection>(_: T) {} // expected-note {{where 'T' = 'ThrowingSequence'}}
func checkConformances(_ inherited: InheritedEqualIterator,
                       _ throwing: ThrowingEqualIterator,
                       _ sequence: NoThrowSequence,
                       _ throwingSequence: ThrowingSequence) {
  requiresIterator(inherited)
  requiresIterator(throwing) // expected-error {{global function 'requiresIterator' requires that 'ThrowingEqualIterator' conform to 'UnsafeCxxInputIterator'}}
  requiresSequence(sequence)
  requiresSequence(throwingSequence) // expected-error {{global function 'requiresSequence' requires that 'ThrowingSequence' conform to 'CxxConvertibleToCollection'}}
}

@available(SwiftStdlib 5.8, *)
func checkReference() {
  // The synthesized factory may allocate even when its constructor is noexcept.
  _ = NoThrowReference(1) // expected-error {{'init(_:)' is unavailable: C++ exception bridging is not supported on this kind of declaration}}
}

@available(SwiftStdlib 5.8, *)
func checkVirtualReference() throws {
  _ = VirtualReference.create().read(1) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'}} expected-note {{did you mean to handle error as optional value}} expected-note {{did you mean to disable error propagation}}
}

func requiresBool<T: CxxConvertibleToBool>(_: T) {} // expected-note {{where 'T' = 'ThrowingBool'}}
func checkOperators(_ throwingBool: ThrowingBool, _ bool: NoThrowBool,
                    _ throwingSubscript: ThrowingSubscript,
                    _ noThrowSubscript: inout NoThrowSubscript,
                    _ throwingPointee: ThrowingPointee,
                    _ pointee: NoThrowPointee) {
  requiresBool(bool)
  requiresBool(throwingBool) // expected-error {{global function 'requiresBool' requires that 'ThrowingBool' conform to 'CxxConvertibleToBool'}}
  _ = throwingSubscript[0] // expected-error {{value of type 'ThrowingSubscript' has no subscripts}}
  noThrowSubscript[1] = noThrowSubscript[0]
  _ = throwingPointee.pointee // expected-error {{value of type 'ThrowingPointee' has no member 'pointee'}}
  _ = pointee.pointee
}

func checkBlocks(_ holder: BlockHolder) {
  _ = holder.block // expected-error {{'block' is unavailable: potentially throwing C++ callable types are not supported in strict C++ exception mode}}
}

// The CxxStdlib conformances need members that strict mode imports as
// throwing, so std types don't get them.
func requiresVector<T: CxxVector>(_: T) {} // expected-note {{where 'T' = 'IntVector'}}
func requiresOptional<T: CxxOptional>(_: T) {} // expected-note {{where 'T' = 'IntOptional'}}
func checkStdConformances(_ vector: IntVector, _ optional: IntOptional) {
  requiresVector(vector) // expected-error {{global function 'requiresVector' requires that 'IntVector'}}
  requiresOptional(optional) // expected-error {{global function 'requiresOptional' requires that 'IntOptional'}}
}

//--- main.swift
@_spi(CxxExceptionBridging) import Cxx
import StrictMembers

var value = try Value(42)
let read = try value.read()
precondition(read == 42)
try value.update(7)
precondition(value.safeRead() == 7)
let capturedRead = value.read
let captured = try capturedRead()
precondition(captured == 7)
let consumed = try value.consume()
precondition(consumed == 7)
let inheritedConstructor: (CInt) throws -> Derived = Derived.init
let derived = try inheritedConstructor(19)
let inheritedRead = try derived.read()
precondition(inheritedRead == 19)
precondition(NoThrowDefault().value == 17)
precondition(DefaultArgument(23).value == 23)
precondition(Array(NoThrowSequence()) == [1, 2])

do {
  _ = try Value(-1)
  fatalError("expected constructor failure")
} catch let error as CxxException {
  precondition(error.message == "negative member value")
}
do {
  try value.update(-1)
  fatalError("expected method failure")
} catch let error as CxxException {
  precondition(error.message == "negative member value")
  precondition(value.safeRead() == 7)
}
do {
  _ = try Defaulted()
  fatalError("expected default member initializer failure")
} catch let error as CxxException {
  precondition(error.message == "default member")
}
do {
  _ = try ImplicitDefaulted()
  fatalError("expected implicit default constructor failure")
} catch let error as CxxException {
  precondition(error.message == "default member")
}

precondition(Bool(fromCxx: NoThrowBool()))
var subscripts = NoThrowSubscript()
subscripts[1] = subscripts[0]
precondition(subscripts[1] == 1)
precondition(NoThrowPointee().pointee == 4)
if #available(SwiftStdlib 5.8, *) {
  let reference = VirtualReference.create()
  let referenceRead = try reference.read(5)
  precondition(referenceRead == 5)
  do {
    _ = try reference.read(-1)
    fatalError("expected virtual method failure")
  } catch let error as CxxException {
    precondition(error.message == "negative member value")
  }
}
