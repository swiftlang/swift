//===--- Demangle.cpp - Swift Name Demangling -----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===---------------------------------------------------------------------===//
//
//  This file implements declaration name demangling in Swift.
//
//===---------------------------------------------------------------------===//

#include "swift/Basic/Demangle.h"
#include "swift/Basic/LLVM.h"
#include "llvm/Support/raw_ostream.h"
#include <functional>
#include <tuple>
#include <vector>

using namespace swift;
using namespace Demangle;

class DemanglerPrinter {
public:
  DemanglerPrinter() : Buffer(), BackendPrinter(Buffer) {}

  DemanglerPrinter &operator<<(std::string s) {
    BackendPrinter << s;
    return *this;
  }

  DemanglerPrinter &operator<<(const char *cstr) {
    BackendPrinter << cstr;
    return *this;
  }

  DemanglerPrinter &operator<<(char c) {
    BackendPrinter << c;
    return *this;
  }

  DemanglerPrinter &operator<<(size_t sz) {
    BackendPrinter << sz;
    return *this;
  }

  std::string &str() { return BackendPrinter.str(); }

private:
  std::string Buffer;
  llvm::raw_string_ostream BackendPrinter;
};

static size_t stringToNumber(const StringRef &Text) {
  size_t result = 0;
  Text.getAsInteger<size_t>(0, result);
  return result;
}

static std::string parenthesize(const std::string &S) {
  if (S[0] == '(')
    return S;
  DemanglerPrinter printer;
  printer << '(' << S << ')';
  return printer.str();
}

static bool isStartOfIdentifier(char c) {
  if (c >= '0' && c <= '9')
    return true;
  return c == 'o';
}

enum class SwiftContextType {
  swiftClass,
  swiftStruct,
  swiftOneOf,
  swiftModule,
  swiftProtocol,
  swiftSubstitution
};

static bool isStartOfNominalType(char c, SwiftContextType *context = nullptr) {
  if (c == 'C') {
    if (context)
      *context = SwiftContextType::swiftClass;
    return true;
  }
  if (c == 'V') {
    if (context)
      *context = SwiftContextType::swiftStruct;
    return true;
  }
  if (c == 'O') {
    if (context)
      *context = SwiftContextType::swiftOneOf;
    return true;
  }
  return false;
}

static std::string archetypeName(size_t i) {
  DemanglerPrinter name;
  do {
    name << (char)('A' + (i % 26));
    i /= 26;
  } while (i);
  return name.str();
}

class Demangler {
public:

  Demangler(StringRef mangled);

  ~Demangler();

  class Substitution {
  public:
    Substitution(std::string X, bool Y);

    Substitution(DemanglerPrinter &X, bool Y);

    const std::string &first() const;

    const bool &second() const;

    explicit operator bool();

    static Substitution success(std::string s);

    static Substitution failure(std::string s);

    typedef std::function<Substitution(std::string)> SubstitutionMonadOperator;

    Substitution monad(SubstitutionMonadOperator f);

  private:
    typedef std::pair<std::string, bool> ImplType;
    ImplType Impl;
  };

  Substitution demangleSymbol();

  Substitution demangleType();

private:

  enum class IsProtocol : bool {
    yes = true, no = false
  };

  enum class IsVariadic : bool {
    yes = true, no = false
  };
  

  /// This type represents a Substitution that also declares whether or not it
  /// contains a Swift protocol
  typedef std::tuple<std::string, bool, IsProtocol> SubstitutionWithProtocol;

  class MangledNameSource {
  public:
    MangledNameSource(StringRef mangled);

    char peek();

    bool nextIf(char c);

    char next();

    bool isEmpty();

    explicit operator bool();

    std::string slice(size_t size);

    std::string getString();

    size_t getOffset();

    size_t getSize();

    bool hasAtLeast(size_t n);

    void advanceOffset(size_t by);

  private:
    StringRef Mangled;
    size_t Offset;
  };

  Substitution demangleGlobal();

  Substitution demangleEntity();

  Substitution demangleValueWitnessKind();

  Substitution demangleContext(SwiftContextType* context = NULL);

  Substitution demangleModule();

  Substitution demangleIdentifier();

  bool demangleNatural(size_t &num);

  bool demangleBuiltinSize(size_t &num);

  Substitution demangleOperator();

  Substitution demangleSubstitution();

  Substitution demangleSubstitutionIndex();

  SubstitutionWithProtocol demangleSubstitutionIndexWithProtocol();

  Substitution demangleIndex();

  Substitution demangleNominalType();

  Substitution demangleProtocolName();

  Substitution demangleDirectness();

  Substitution demangleDeclWithContext(std::string context);

  Substitution demangleDeclTypeWithContextAndName(std::string context,
                                                  std::string name);

  Substitution demangleArchetypes();

  Substitution demangleArchetypeRef(size_t depth, size_t i);

  Substitution demangleDeclarationName(IsProtocol protocol);

  Substitution demangleProtocolList();

  Substitution demangleList(std::function<Substitution(void)> element);

  Substitution demangleTuple(IsVariadic variadic);

  Substitution failure();

  Substitution success(std::string S);

  Substitution success(DemanglerPrinter &SS);

  Substitution success(Substitution &SN);

  std::vector<Substitution> Substitutions;
  std::vector<int> ArchetypeCounts;
  int ArchetypeCount;
  MangledNameSource Mangled;
};

Demangler::Substitution::Substitution(std::string X, bool Y) : Impl({ X, Y }) {}

Demangler::Substitution::Substitution(DemanglerPrinter &X, bool Y)
    : Impl({ X.str(), Y }) {}

const std::string &Demangler::Substitution::first() const { return Impl.first; }

const bool &Demangler::Substitution::second() const { return Impl.second; }

Demangler::Substitution::operator bool() { return second(); }

Demangler::Substitution Demangler::Substitution::success(std::string s) {
  return Substitution(s, true);
}

Demangler::Substitution Demangler::Substitution::failure(std::string s) {
  return Substitution(s, false);
}

Demangler::Substitution Demangler::Substitution::monad(
    Demangler::Substitution::SubstitutionMonadOperator f) {
  if (second())
    return f(first());
  return *this;
}

Demangler::MangledNameSource::MangledNameSource(StringRef Mangled)
    : Mangled(Mangled), Offset(0) {}

char Demangler::MangledNameSource::peek() { return Mangled.front(); }

bool Demangler::MangledNameSource::nextIf(char c) {
  if (isEmpty())
    return false;
  char real_c = peek();
  if (real_c == c) {
    advanceOffset(1);
    return true;
  }
  return false;
}

char Demangler::MangledNameSource::next() {
  char c = peek();
  advanceOffset(1);
  return c;
}

bool Demangler::MangledNameSource::isEmpty() { return Mangled.empty(); }

Demangler::MangledNameSource::operator bool() { return isEmpty() == false; }

std::string Demangler::MangledNameSource::slice(size_t size) {
  return Mangled.substr(0, size);
}

std::string Demangler::MangledNameSource::getString() { return Mangled.data(); }

size_t Demangler::MangledNameSource::getOffset() { return Offset; }

size_t Demangler::MangledNameSource::getSize() { return Mangled.size(); }

bool Demangler::MangledNameSource::hasAtLeast(size_t n) {
  if (n > getSize())
    return false;
  return true;
}

void Demangler::MangledNameSource::advanceOffset(size_t by) {
  Offset += by;
  Mangled = Mangled.substr(by);
}

Demangler::Demangler(StringRef Mangled)
    : Substitutions(), ArchetypeCounts(), ArchetypeCount(0), Mangled(Mangled) {}

Demangler::~Demangler() {}

Demangler::Substitution Demangler::demangleSymbol() {

  if (!Mangled.hasAtLeast(2))
    return failure();
  if (Mangled.slice(2) != "_T")
    return failure();
  if (Mangled.hasAtLeast(4)) {
    if (Mangled.slice(4) == "_TTo") {
      Mangled.advanceOffset(4);
      Substitution global = demangleGlobal();
      if (global) {
        DemanglerPrinter demangled;
        demangled << "[objc] " << global.first();
        return success(demangled.str());
      }
      return failure();
    }
  }
  Mangled.advanceOffset(2);
  Substitution global = demangleGlobal();
  if (global)
    return global;
  return failure();
}

Demangler::Substitution Demangler::demangleGlobal() {

  if (!Mangled)
    return failure();

  if (Mangled.nextIf('M')) {
    if (Mangled.nextIf('P')) {
      Substitution directness = demangleDirectness();
      if (!directness)
        return failure();
      Substitution type = demangleType();
      if (!type)
        return failure();
      return success(DemanglerPrinter() << directness.first()
                                        << " generic type metadata pattern for "
                                        << type.first());
    }
    if (Mangled.nextIf('m')) {
      Substitution type = demangleType();
      if (!type)
        return failure();
      return success(DemanglerPrinter() << "metaclass for " << type.first());
    }
    Substitution directness = demangleDirectness();
    if (!directness)
      return failure();
    Substitution type = demangleType();
    if (!type)
      return failure();
    return success(DemanglerPrinter() << directness.first()
                                      << " type metadata for "
                                      << type.first());
  }

  if (Mangled.nextIf('n')) {
    if (Mangled.nextIf('k') && Mangled.nextIf('_')) {
      Substitution entity = demangleEntity();
      if (!entity)
        return failure();
      return success(DemanglerPrinter() << "protocol witness for "
                                        << entity.first());
    }
    return failure();
  }

  if (Mangled.nextIf('t')) {
    Substitution type = demangleType();
    if (!type)
      return failure();
    return success(DemanglerPrinter() << type.first());
  }

  if (Mangled.nextIf('w')) {
    Substitution kind = demangleValueWitnessKind();
    if (!kind)
      return failure();
    Substitution type = demangleType();
    if (!type)
      return failure();
    return success(DemanglerPrinter() << kind.first() << " value witness for "
                                      << type.first());
  }

  if (Mangled.nextIf('W')) {
    if (Mangled.nextIf('V')) {
      Substitution type = demangleType();
      if (type)
        return success(DemanglerPrinter() << "value witness table for "
                                          << type.first());
      return failure();
    }
    if (Mangled.nextIf('o')) {
      Substitution entity = demangleEntity();
      if (entity)
        return success(DemanglerPrinter() << "witness table Offset for "
                                          << entity.first());
      return failure();
    }
    if (Mangled.nextIf('v')) {
      Substitution directness = demangleDirectness();
      if (!directness)
        return failure();
      Substitution entity = demangleEntity();
      if (entity)
        return success(DemanglerPrinter() << directness.first()
                                          << " field Offset for "
                                          << entity.first());
      return failure();
    }
    return failure();
  }
  if (Mangled.nextIf('T')) {
    if (Mangled.nextIf('b')) {
      Substitution type = demangleType();
      if (type)
        return success(
            DemanglerPrinter() << "bridge-to-block std::function for "
                               << type.first());
      return failure();
    }
    return failure();
  }
  if (Mangled.nextIf('L')) {
    Substitution entity = demangleEntity();
    if (entity)
      return success(DemanglerPrinter() << "local " << entity.first());
    return failure();
  }
  return demangleEntity();
}

Demangler::Substitution Demangler::demangleEntity() {
  SwiftContextType contextType;
  Substitution context = demangleContext(&contextType);
  if (context) {
    if (Mangled.nextIf('D')) {
      if (contextType == SwiftContextType::swiftClass)
        return success(DemanglerPrinter() << context.first()
                       << ".__deallocating_destructor");
      return success(DemanglerPrinter() << context.first() << ".destructor");
    }
    if (Mangled.nextIf('d'))
      return success(DemanglerPrinter() << context.first()
                                        << ".destructor");
    if (Mangled.nextIf('C')) {
      Substitution type = demangleType();
      if (!type)
        return failure();
      if (contextType == SwiftContextType::swiftClass)
        return success(DemanglerPrinter() << context.first()
                       << ".__allocating_constructor : " << type.first());
      return success(DemanglerPrinter() << context.first()
                                        << ".constructor : " << type.first());
    }
    if (Mangled.nextIf('c')) {
      Substitution type = demangleType();
      if (!type)
        return failure();
      return success(DemanglerPrinter() << context.first()
                                        << ".constructor : "
                                        << type.first());
    }

    Substitution decl_with_ctx = demangleDeclWithContext(context.first());
    if (decl_with_ctx) {
      if (Mangled.nextIf('a'))
        return success(DemanglerPrinter() << decl_with_ctx.first()
                                          << " addressor");
      if (Mangled.nextIf('g'))
        return success(DemanglerPrinter() << decl_with_ctx.first()
                                          << " getter");
      if (Mangled.nextIf('s'))
        return success(DemanglerPrinter() << decl_with_ctx.first()
                                          << " setter");
      return success(decl_with_ctx);
    }
  }
  return failure();
}

Demangler::Substitution Demangler::demangleValueWitnessKind() {
  if (!Mangled)
    return failure();
  char c1 = Mangled.next();
  if (!Mangled)
    return failure();
  char c2 = Mangled.next();
  if (c1 == 'a' && c2 == 'l')
    return success("allocateBuffer");
  if (c1 == 'c' && c2 == 'a')
    return success("assignWithCopy");
  if (c1 == 't' && c2 == 'a')
    return success("assignWithTake");
  if (c1 == 'd' && c2 == 'e')
    return success("deallocateBuffer");
  if (c1 == 'x' && c2 == 'x')
    return success("destroy");
  if (c1 == 'X' && c2 == 'X')
    return success("destroyBuffer");
  if (c1 == 'C' && c2 == 'P')
    return success("initializeBufferWithCopyOfBuffer");
  if (c1 == 'C' && c2 == 'p')
    return success("initializeBufferWithCopy");
  if (c1 == 'c' && c2 == 'p')
    return success("initializeWithCopy");
  if (c1 == 'T' && c2 == 'k')
    return success("initializeBufferWithTake");
  if (c1 == 't' && c2 == 'k')
    return success("initializeWithTake");
  if (c1 == 'p' && c2 == 'r')
    return success("projectBuffer");
  if (c1 == 't' && c2 == 'y')
    return success("typeof");
  return failure();
}

Demangler::Substitution Demangler::demangleContext(SwiftContextType* context) {
  if (!Mangled)
    return failure();
  char c = Mangled.peek();
  if (isStartOfIdentifier(c) || c == 'S')
  {
    if (context) {
      if (c == 'S')
        *context = SwiftContextType::swiftSubstitution;
      else
        *context = SwiftContextType::swiftModule;
    }
    return demangleModule();
  }
  if (isStartOfNominalType(c, context))
    return demangleNominalType();
  if (c == 'P') {
    Mangled.next();
    if (context)
      *context = SwiftContextType::swiftProtocol;
    return demangleProtocolName();
  }
  return failure();
}

Demangler::Substitution Demangler::demangleModule() {
  char c = Mangled.peek();
  if (isStartOfIdentifier(c)) {
    Substitution identifier = demangleIdentifier();
    if (identifier)
      Substitutions.push_back(Substitution(identifier.first(), false));
    return identifier;
  }
  if (c == 'S')
    return demangleSubstitution();
  return failure();
}

Demangler::Substitution Demangler::demangleIdentifier() {

  if (!Mangled)
    return failure();
  if (Mangled.nextIf('o')) {
    char op_mode = Mangled.next();
    if (op_mode != 'p' && op_mode != 'P' && op_mode != 'i')
      return failure();
    Substitution operatr = demangleOperator();
    if (operatr) {
      if (op_mode == 'p')
        return success(DemanglerPrinter() << operatr.first() << " [prefix]");
      if (op_mode == 'P')
        return success(DemanglerPrinter() << operatr.first() << " [postfix]");
      if (op_mode == 'i')
        return success(DemanglerPrinter() << operatr.first() << " [infix]");
      return failure();
    }
  }

  size_t length;
  if (demangleNatural(length)) {
    if (Mangled.hasAtLeast(length)) {
      Substitution identifier = success(Mangled.slice(length));
      Mangled.advanceOffset(length);
      return identifier;
    }
  }

  return failure();
}

bool Demangler::demangleNatural(size_t &num) {
  if (!Mangled)
    return false;
  DemanglerPrinter printer;
  char c = Mangled.next();
  if (c < '0' || c > '9')
    return false;
  printer << c;
  while (true) {
    if (!Mangled) {
      num = stringToNumber(printer.str());
      return true;
    }
    c = Mangled.peek();
    if (c < '0' || c > '9') {
      num = stringToNumber(printer.str());
      return true;
    } else
      printer << c;
    Mangled.next();
  }
}

Demangler::Substitution Demangler::demangleOperator() {
  static const char op_char_table[] = "& @/= >    <*!|+ %-~   ^ .";
  size_t length;
  if (demangleNatural(length)) {
    if (Mangled.hasAtLeast(length)) {
      std::string op_base = Mangled.slice(length);
      Mangled.advanceOffset(length);
      DemanglerPrinter op;
      size_t op_base_size = op_base.size();
      for (size_t idx = 0; idx < op_base_size; ++idx) {
        char c = op_base[idx];
        if (c < 'a' || c > 'z')
          return failure();
        char o = op_char_table[c - 'a'];
        if (o == ' ')
          return failure();
        op << o;
      }
      return success(op);
    } else
      return failure();
  }
  return failure();
}

Demangler::Substitution Demangler::demangleSubstitution() {
  if (Mangled.nextIf('S'))
    return demangleSubstitutionIndex();
  return failure();
}

Demangler::Substitution Demangler::demangleSubstitutionIndex() {
  SubstitutionWithProtocol sub_with_proto =
      demangleSubstitutionIndexWithProtocol();
  return Substitution(std::get<0>(sub_with_proto), std::get<1>(sub_with_proto));
}

Demangler::SubstitutionWithProtocol
Demangler::demangleSubstitutionIndexWithProtocol() {
  if (!Mangled)
    return { Mangled.getString(), false, IsProtocol::no };
  if (Mangled.nextIf('o'))
    return { "ObjectiveC", true, IsProtocol::no };
  if (Mangled.nextIf('C'))
    return { "C", true, IsProtocol::no };
  if (Mangled.nextIf('s'))
    return { "swift", true, IsProtocol::no };
  if (Mangled.nextIf('a'))
    return { "swift.Slice", true, IsProtocol::no };
  if (Mangled.nextIf('b'))
    return { "swift.Bool", true, IsProtocol::no };
  if (Mangled.nextIf('c'))
    return { "swift.Char", true, IsProtocol::no };
  if (Mangled.nextIf('d'))
    return { "swift.Float64", true, IsProtocol::no };
  if (Mangled.nextIf('f'))
    return { "swift.Float32", true, IsProtocol::no };
  if (Mangled.nextIf('i'))
    return { "swift.Int64", true, IsProtocol::no };
  if (Mangled.nextIf('S'))
    return { "swift.String", true, IsProtocol::no };
  if (Mangled.nextIf('u'))
    return { "swift.UInt64", true, IsProtocol::no };
  Substitution index_sub = demangleIndex();
  if (!index_sub)
    return { Mangled.getString(), false, IsProtocol::no };
  size_t index = stringToNumber(index_sub.first());
  if (index >= Substitutions.size())
    return { Mangled.getString(), false, IsProtocol::no };
  Substitution sub = Substitutions[index];
  return { sub.first(), true, sub.second() ? IsProtocol::yes : IsProtocol::no };
}

Demangler::Substitution Demangler::demangleIndex() {
  if (Mangled.nextIf('_'))
    return success("0");
  size_t natural;
  if (demangleNatural(natural)) {
    if (!Mangled.nextIf('_'))
      return failure();
    return success(DemanglerPrinter() << natural + 1);
  }
  return failure();
}

Demangler::Substitution Demangler::demangleNominalType() {
  if (!Mangled)
    return failure();
  char c = Mangled.next();
  if (c == 'S')
    return demangleSubstitutionIndex();
  if (isStartOfNominalType(c))
    return demangleDeclarationName(IsProtocol::no);
  return failure();
}

Demangler::Substitution Demangler::demangleProtocolName() {
  if (Mangled.nextIf('S')) {
    SubstitutionWithProtocol sub_with_proto =
        demangleSubstitutionIndexWithProtocol();
    if (std::get<1>(sub_with_proto) == false)
      return failure();
    if (std::get<2>(sub_with_proto) == IsProtocol::yes)
      return success(std::get<0>(sub_with_proto));
    Substitution identifier = demangleIdentifier();
    if (!identifier)
      return failure();
    Substitutions.push_back(success(identifier));
    return success(identifier);
  }
  return demangleDeclarationName(IsProtocol::yes);
}

Demangler::Substitution Demangler::demangleDirectness() {
  if (Mangled.nextIf('d'))
    return success("direct");
  if (Mangled.nextIf('i'))
    return success("indirect");
  return failure();
}

Demangler::Substitution
Demangler::demangleDeclWithContext(std::string context) {
  Substitution identifier = demangleIdentifier();
  if (!identifier)
    return failure();
  return demangleDeclTypeWithContextAndName(context, identifier.first());
}

Demangler::Substitution
Demangler::demangleDeclTypeWithContextAndName(std::string context,
                                              std::string name) {
  Substitution type = demangleType();
  if (!type)
    return failure();
  return success(DemanglerPrinter() << context << "." << name << " : "
                                    << type.first());
}

bool Demangler::demangleBuiltinSize(size_t &num) {
  if (!demangleNatural(num))
    return false;
  if (Mangled.nextIf('_'))
    return true;
  return false;
}

Demangler::Substitution Demangler::demangleType() {
  if (!Mangled)
    return failure();

  char c = Mangled.next();
  if (c == 'A') {
    size_t size;
    if (demangleNatural(size)) {
      Substitution type = demangleType();
      if (type)
        return success(DemanglerPrinter() << type.first() << "[" << size
                                          << "]");
    }
    return failure();
  }
  if (c == 'B') {
    if (!Mangled)
      return failure();
    c = Mangled.next();
    if (c == 'f') {
      size_t size;
      if (demangleBuiltinSize(size)) {
          return success(DemanglerPrinter() << "Builtin.Float" << size);
      }
    }
    if (c == 'i') {
      size_t size;
      if (demangleBuiltinSize(size)) {
          return success(DemanglerPrinter() << "Builtin.Int" << size);
      }
    }
    if (c == 'v') {
      size_t elts;
      if (demangleNatural(elts)) {
        if (!Mangled.nextIf('B'))
          return failure();
        if (Mangled.nextIf('i')) {
          size_t size;
          if (!demangleBuiltinSize(size))
            return failure();
          return success(DemanglerPrinter() << "Builtin.Vec" << elts << "xInt"
                                            << size);
        }
        if (Mangled.nextIf('f')) {
          size_t size;
          if (!demangleBuiltinSize(size))
            return failure();
          return success(DemanglerPrinter() << "Builtin.Vec" << elts << "xFloat"
                                            << size);
        }
        if (Mangled.nextIf('p'))
          return success(DemanglerPrinter() << "Builtin.Vec" << elts
                                            << "xRawPointer");
      }
    }
    if (c == 'O')
      return success("Builtin.ObjCPointer");
    if (c == 'o')
      return success("Builtin.ObjectPointer");
    if (c == 'p')
      return success("Builtin.RawPointer");
    if (c == 'u')
      return success("Builtin.OpaquePointer");
    if (c == 'p')
      return success("Builtin.RawPointer");
    return failure();
  }
  if (c == 'b') {
    Substitution in = demangleType();
    if (in) {
      Substitution out = demangleType();
      if (out)
        return success(DemanglerPrinter() << "[objc_block] "
                                          << parenthesize(in.first()) << " -> "
                                          << out.first());
    }
    return failure();
  }
  if (c == 'F') {
    Substitution in = demangleType();
    if (in) {
      Substitution out = demangleType();
      if (out)
        return success(DemanglerPrinter() << parenthesize(in.first()) << " -> "
                                          << out.first());
    }
    return failure();
  }
  if (c == 'f') {
    Substitution in = demangleType();
    if (in) {
      Substitution out = demangleType();
      if (out)
        return success(DemanglerPrinter() << parenthesize(in.first())
                                          << out.first());
    }
    return failure();
  }
  if (c == 'G') {
    Substitution type = demangleType();
    if (!type)
      return failure();
    Substitution list = demangleList([this]{
      return demangleType();
    });
    if (!list)
      return failure();
    return success(DemanglerPrinter() << type.first() << "<" << list.first()
                                      << ">");
  }
  if (c == 'M') {
    Substitution type = demangleType();
    if (type)
      return success(DemanglerPrinter() << type.first() << ".metatype");
    return failure();
  }
  if (c == 'P')
    return demangleProtocolList();
  if (c == 'Q') {
    if (Mangled.nextIf('d')) {
      Substitution depth_sub = demangleIndex();
      if (!depth_sub)
        return failure();
      Substitution index_sub = demangleIndex();
      if (!index_sub)
        return failure();
      return demangleArchetypeRef(stringToNumber(depth_sub.first()) + 1,
                                  stringToNumber(index_sub.first()));
    }
    Substitution index_sub = demangleIndex();
    if (!index_sub)
      return failure();
    return demangleArchetypeRef(0, stringToNumber(index_sub.first()));
  }
  if (c == 'R') {
    Substitution type = demangleType();
    if (type)
      return success(DemanglerPrinter() << "[byref] " << type.first());
    return failure();
  }
  if (c == 'T')
    return demangleTuple(IsVariadic::no);
  if (c == 't')
    return demangleTuple(IsVariadic::yes);
  if (isStartOfNominalType(c))
    return demangleDeclarationName(IsProtocol::no);
  if (c == 'S')
    return demangleSubstitutionIndex();
  if (c == 'U') {
    ArchetypeCounts.push_back(ArchetypeCount);
    Substitution archetypes = demangleArchetypes();
    if (!archetypes)
      return failure();
    Substitution base = demangleType();
    if (!base)
      return failure();
    ArchetypeCount = ArchetypeCounts.back();
    ArchetypeCounts.pop_back();
    return success(DemanglerPrinter() << "<" << archetypes.first() << ">"
                                      << base.first());
  }
  return failure();
}

Demangler::Substitution Demangler::demangleArchetypes() {
  DemanglerPrinter result;
  bool first = true;
  while (true) {
    if (Mangled.nextIf('_')) {
      if (!Mangled)
        return failure();
      char c = Mangled.peek();
      if (c != '_' && c != 'S' && !isStartOfIdentifier(c))
        break;
      if (!first)
        result << ", ";
      result << archetypeName(ArchetypeCount);
    } else {
      Substitution proto_list = demangleProtocolList();
      if (!proto_list)
        return failure();
      if (!first)
        result << ", ";
      result << archetypeName(ArchetypeCount) << " : " << proto_list.first();
    }

    ++ArchetypeCount;
    first = false;
  }

  return success(result);
}

Demangler::Substitution Demangler::demangleArchetypeRef(size_t depth,
                                                        size_t i) {
  if (depth == 0 && ArchetypeCount == 0)
    return success(DemanglerPrinter() << archetypeName(i));
  size_t length = ArchetypeCounts.size();
  if (depth >= length)
    return failure();
  size_t index = ArchetypeCounts[length - 1 - depth] + i;
  size_t max = (depth == 0) ? ArchetypeCount : ArchetypeCounts[length - depth];
  if (index >= max)
    return failure();
  return success(archetypeName(index));
}

Demangler::Substitution Demangler::demangleDeclarationName(IsProtocol isA) {
  Substitution context = demangleContext();
  if (context) {
    Substitution identifier = demangleIdentifier();
    if (identifier) {
      Substitutions.push_back(Substitution(
          DemanglerPrinter() << context.first() << "." << identifier.first(),
          isA == IsProtocol::yes));
      return success(DemanglerPrinter() << context.first() << "."
                                        << identifier.first());
    }
  }
  return failure();
}

Demangler::Substitution Demangler::demangleProtocolList() {
  if (Mangled.nextIf('_'))
    return success("protocol<>");
  Substitution first = demangleProtocolName();
  if (!first)
    return failure();
  if (Mangled.nextIf('_'))
    return first;
  Substitution list = demangleList([this]() {
    return demangleProtocolName();
  });
  if (list)
    return success(DemanglerPrinter() << "protocol<" << first.first() << ", "
                                      << list.first() << ">");
  return failure();
}

Demangler::Substitution
Demangler::demangleList(std::function<Substitution(void)> element) {
  DemanglerPrinter result;
  bool first = true;
  while (Mangled.nextIf('_') == false) {
    if (!first)
      result << ", ";
    Substitution next = element();
    if (!next)
      return failure();
    result << next.first();
    first = false;
  }
  return success(result);
}

Demangler::Substitution Demangler::demangleTuple(IsVariadic isA) {
  Substitution list = demangleList([this]() {
    if (!Mangled)
      return failure();
    if (isStartOfIdentifier(Mangled.peek())) {
      Substitution id = demangleIdentifier();
      if (id) {
        Substitution type = demangleType();
        if (type)
          return success(DemanglerPrinter() << id.first() << " : "
                                            << type.first());
      }
      return failure();
    }
    return demangleType();
  });
  if (!list)
    return failure();
  if (isA == IsVariadic::yes)
    return success(DemanglerPrinter() << "(" << list.first() << "...)");
  return success(DemanglerPrinter() << "(" << list.first() << ")");
}

Demangler::Substitution Demangler::failure() {
  return Substitution::failure(Mangled.getString());
}

Demangler::Substitution Demangler::success(std::string S) {
  return Substitution::success(S);
}

Demangler::Substitution Demangler::success(DemanglerPrinter &SS) {
  return success(SS.str());
}

Demangler::Substitution Demangler::success(Substitution &SN) {
  return success(SN.first());
}

std::string swift::Demangle::demangleSymbol(StringRef mangled) {
  Demangler::Substitution demangled = Demangler(mangled).demangleSymbol();
  if (demangled)
    return demangled.first();
  return mangled.data();
}

std::string swift::Demangle::demangleType(StringRef mangled) {
  Demangler::Substitution demangled = Demangler(mangled).demangleType();
  if (demangled)
    return demangled.first();
  return mangled.data();
}
