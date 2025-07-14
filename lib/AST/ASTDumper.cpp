//===--- ASTDumper.cpp - Swift Language AST Dumper ------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements dumping for the Swift ASTs.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTDumper.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Attr.h"
#include "swift/AST/AutoDiff.h"
#include "swift/AST/AvailabilitySpec.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/ForeignAsyncConvention.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/PackConformance.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeVisitor.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/QuotedString.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/StringExtras.h"
#include "clang/AST/Type.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/JSON.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/raw_ostream.h"
#include <optional>

//
// AST DUMPING TIPS
// ================
//
// * Pass values before names (because names are often optional and can be
//   omitted or empty).
//
// * Put all `printField*()` and `printFlag*()` calls before `printRec()` calls.
//   `printRec()` variants print a child node, and all fields of the current
//   node need to be printed before any child node is printed.
//
// * `printField()` expects a "simple" argument that will be converted to a
//   keyword string by passing it through `getDumpString()`. For values that are
//   at all complicated, use `printFieldQuoted()`, which will automatically
//   quote and escape the value.
//
// * Confine all direct formatting for the console (e.g. printing quotes or
//   parentheses) in `PrintBase`. Eventually we want to allow e.g. JSON dumping;
//   limiting the amount of direct I/O helps us with that.
//

using namespace swift;

struct TerminalColor {
  llvm::raw_ostream::Colors Color;
  bool Bold;
};

#define DEF_COLOR(NAME, COLOR, BOLD) \
static const TerminalColor NAME##Color = { llvm::raw_ostream::COLOR, BOLD };

DEF_COLOR(Func, YELLOW, false)
DEF_COLOR(Range, YELLOW, false)
DEF_COLOR(AccessLevel, YELLOW, false)
DEF_COLOR(ASTNode, YELLOW, true)
DEF_COLOR(Parameter, YELLOW, false)
DEF_COLOR(Extension, MAGENTA, false)
DEF_COLOR(Pattern, RED, true)
DEF_COLOR(Override, RED, false)
DEF_COLOR(Stmt, RED, true)
DEF_COLOR(Captures, RED, false)
DEF_COLOR(Arguments, RED, false)
DEF_COLOR(TypeRepr, GREEN, false)
DEF_COLOR(LiteralValue, GREEN, false)
DEF_COLOR(Decl, GREEN, true)
DEF_COLOR(Parenthesis, BLUE, false)
DEF_COLOR(Type, BLUE, false)
DEF_COLOR(Discriminator, BLUE, false)
DEF_COLOR(InterfaceType, GREEN, false)
DEF_COLOR(Identifier, GREEN, false)
DEF_COLOR(Expr, MAGENTA, true)
DEF_COLOR(ExprModifier, CYAN, false)
DEF_COLOR(DeclModifier, CYAN, false)
DEF_COLOR(ArgModifier, CYAN, false)
DEF_COLOR(ClosureModifier, CYAN, false)
DEF_COLOR(DeclAttribute, CYAN, false)
DEF_COLOR(FieldLabel, CYAN, false)
DEF_COLOR(Location, CYAN, false)

#undef DEF_COLOR

namespace {
  /// RAII object that prints with the given color, if color is supported on the
  /// given stream.
  class PrintWithColorRAII {
    raw_ostream &OS;
    bool ShowColors;

  public:
    PrintWithColorRAII(raw_ostream &os, TerminalColor color)
    : OS(os), ShowColors(false)
    {
      ShowColors = os.has_colors();

      if (ShowColors)
        OS.changeColor(color.Color, color.Bold);
    }

    ~PrintWithColorRAII() {
      if (ShowColors) {
        OS.resetColor();
      }
    }

    raw_ostream &getOS() const { return OS; }

    template<typename T>
    friend raw_ostream &operator<<(PrintWithColorRAII &&printer,
                                   const T &value){
      printer.OS << value;
      return printer.OS;
    }

  };

/// Wraps a \c raw_ostream so that anything printed through it is automatically
/// escaped appropriately for a double-quoted string.
class escaping_ostream : public raw_ostream {
  raw_ostream &base_os;

public:
  escaping_ostream(raw_ostream &base_os)
    : raw_ostream(/*unbuffered=*/true), base_os(base_os)
  {}

  virtual ~escaping_ostream() {}

  virtual void reserveExtraSpace(uint64_t ExtraSize) override {
    base_os.reserveExtraSpace(ExtraSize);
  }

  virtual raw_ostream &changeColor(enum Colors Color, bool Bold = false,
                                   bool BG = false) override {
    return base_os.changeColor(Color, Bold, BG);
  }

  virtual raw_ostream &resetColor() override {
    return base_os.resetColor();
  }

  virtual raw_ostream &reverseColor() override {
    return base_os.reverseColor();
  }

  virtual bool is_displayed() const override {
    return base_os.is_displayed();
  }

  virtual bool has_colors() const override {
    return base_os.has_colors();
  }

  virtual void enable_colors(bool enable) override {
    base_os.enable_colors(enable);
  }

private:
  virtual void write_impl(const char *Ptr, size_t Size) override {
    writeEscaped(StringRef(Ptr, Size), base_os);
  }

  virtual uint64_t current_pos() const override {
    return base_os.tell();
  }

  virtual void anchor() override {}
};

/// Replaces any local archetypes in the given type with their equivalent
/// existential upper bounds so that they can be passed to the AST mangler. This
/// loses information but is probably sufficient for most questions about these
/// types that consumers of the JSON AST would ask.
Type replaceLocalArchetypesWithExistentials(Type type) {
  return type.transformRec([&](TypeBase *t) -> std::optional<Type> {
    if (auto LAT = dyn_cast<LocalArchetypeType>(t)) {
      return LAT->getExistentialType();
    }
    return std::nullopt;
  });
}

/// Replaces any opaque type archetypes in the given type with their equivalent
/// existential upper bounds. This is used when dumping the mapping of all
/// opaque types in the source file so that their conformances can be more
/// easily reasoned about without having to find the declaring opaque result
/// type deeper in the AST.
Type replaceOpaqueArchetypesWithExistentials(Type type) {
  return type.transformRec([&](TypeBase *t) -> std::optional<Type> {
    if (auto OT = dyn_cast<OpaqueTypeArchetypeType>(t)) {
      return OT->getExistentialType();
    }
    return std::nullopt;
  });
}

/// Returns the USR of the given declaration. Gracefully returns an empty
/// string if D is null or invalid.
std::string declUSR(const Decl *D) {
  if (!D)
    return "";

  // Certain local synthesized declarations won't be assigned a local
  // discriminator, later causing an assertion if we try to generate a USR
  // for them. Avoid these.
  // FIXME: USR generation should probably do a better job of avoiding the
  // asserting code path?
  if (auto VD = dyn_cast<ValueDecl>(D);
      VD && VD->getDeclContext()->isLocalContext() &&
      (!VD->getLoc().isValid() ||
       (VD->getModuleContext()
            ->getSourceFileContainingLocation(VD->getLoc())
            ->getFulfilledMacroRole() == std::nullopt))) {
    return "";
  }

  std::string usr;
  llvm::raw_string_ostream os(usr);
  if (swift::ide::printDeclUSR(D, os))
    return "";
  return usr;
}

/// Returns the USR of the given type. Gracefully returns an empty string
/// if the type is invalid.
std::string typeUSR(Type type) {
  if (!type)
    return "";

  if (type->hasArchetype()) {
    type = type->mapTypeOutOfContext();
  }
  if (type->hasLocalArchetype()) {
    type = replaceLocalArchetypesWithExistentials(type);
  }

  std::string usr;
  llvm::raw_string_ostream os(usr);
  if (swift::ide::printTypeUSR(type, os))
    return "";
  return usr;
}

/// Returns the USR of the given value declaration's type. Gracefully returns
/// the empty string if D is null.
std::string declTypeUSR(const ValueDecl *D) {
  if (!D)
    return "";

  std::string usr;
  llvm::raw_string_ostream os(usr);
  if (swift::ide::printDeclTypeUSR(D, os))
    return "";
  return usr;
}

} // end anonymous namespace

static StringRef getDumpString(SILFunctionType::Representation value) {
  switch (value) {
  case SILFunctionType::Representation::Thick: return "thick";
  case SILFunctionType::Representation::Block: return "block";
  case SILFunctionType::Representation::CFunctionPointer: return "c";
  case SILFunctionType::Representation::CXXMethod:
    return "cxx_method";
  case SILFunctionType::Representation::Thin: return "thin";
  case SILFunctionType::Representation::Method: return "method";
  case SILFunctionType::Representation::ObjCMethod: return "objc_method";
  case SILFunctionType::Representation::WitnessMethod: return "witness_method";
  case SILFunctionType::Representation::Closure: return "closure";
  case SILFunctionType::Representation::KeyPathAccessorGetter:
    return "keypath_accessor_getter";
  case SILFunctionType::Representation::KeyPathAccessorSetter:
    return "keypath_accessor_setter";
  case SILFunctionType::Representation::KeyPathAccessorEquals:
    return "keypath_accessor_equals";
  case SILFunctionType::Representation::KeyPathAccessorHash:
    return "keypath_accessor_hash";
  }

  llvm_unreachable("Unhandled SILFunctionTypeRepresentation in switch.");
}

static StringRef getDumpString(ReadImplKind kind) {
  switch (kind) {
  case ReadImplKind::Stored:
    return "stored";
  case ReadImplKind::Inherited:
    return "inherited";
  case ReadImplKind::Get:
    return "getter";
  case ReadImplKind::Address:
    return "addressor";
  case ReadImplKind::Read:
    return "read_coroutine";
  case ReadImplKind::Read2:
    return "read2_coroutine";
  }
  llvm_unreachable("bad kind");
}

static StringRef getDumpString(WriteImplKind kind) {
  switch (kind) {
  case WriteImplKind::Immutable:
    return "immutable";
  case WriteImplKind::Stored:
    return "stored";
  case WriteImplKind::StoredWithObservers:
    return "stored_with_observers";
  case WriteImplKind::InheritedWithObservers:
    return "inherited_with_observers";
  case WriteImplKind::Set:
    return "setter";
  case WriteImplKind::MutableAddress:
    return "mutable_addressor";
  case WriteImplKind::Modify:
    return "modify_coroutine";
  case WriteImplKind::Modify2:
    return "modify2_coroutine";
  }
  llvm_unreachable("bad kind");
}

static StringRef getDumpString(ReadWriteImplKind kind) {
  switch (kind) {
  case ReadWriteImplKind::Immutable:
    return "immutable";
  case ReadWriteImplKind::Stored:
    return "stored";
  case ReadWriteImplKind::MutableAddress:
    return "mutable_addressor";
  case ReadWriteImplKind::MaterializeToTemporary:
    return "materialize_to_temporary";
  case ReadWriteImplKind::Modify:
    return "modify_coroutine";
  case ReadWriteImplKind::Modify2:
    return "modify2_coroutine";
  case ReadWriteImplKind::StoredWithDidSet:
    return "stored_with_didset";
  case ReadWriteImplKind::InheritedWithDidSet:
    return "inherited_with_didset";
  }
  llvm_unreachable("bad kind");
}

static StringRef getDumpString(ImportKind value) {
  switch (value) {
  case ImportKind::Module: return "module";
  case ImportKind::Type: return "type";
  case ImportKind::Struct: return "struct";
  case ImportKind::Class: return "class";
  case ImportKind::Enum: return "enum";
  case ImportKind::Protocol: return "protocol";
  case ImportKind::Var: return "var";
  case ImportKind::Func: return "func";
  }
  
  llvm_unreachable("Unhandled ImportKind in switch.");
}

static StringRef getDumpString(ForeignErrorConvention::Kind value) {
  switch (value) {
  case ForeignErrorConvention::ZeroResult: return "ZeroResult";
  case ForeignErrorConvention::NonZeroResult: return "NonZeroResult";
  case ForeignErrorConvention::ZeroPreservedResult: return "ZeroPreservedResult";
  case ForeignErrorConvention::NilResult: return "NilResult";
  case ForeignErrorConvention::NonNilError: return "NonNilError";
  }

  llvm_unreachable("Unhandled ForeignErrorConvention in switch.");
}
static StringRef getDumpString(DefaultArgumentKind value) {
  switch (value) {
    case DefaultArgumentKind::None: return "none";
#define MAGIC_IDENTIFIER(NAME, STRING)                                         \
    case DefaultArgumentKind::NAME: return STRING;
#include "swift/AST/MagicIdentifierKinds.def"
    case DefaultArgumentKind::Inherited: return "inherited";
    case DefaultArgumentKind::NilLiteral: return "nil";
    case DefaultArgumentKind::EmptyArray: return "[]";
    case DefaultArgumentKind::EmptyDictionary: return "[:]";
    case DefaultArgumentKind::Normal: return "normal";
    case DefaultArgumentKind::StoredProperty: return "stored property";
    case DefaultArgumentKind::ExpressionMacro:
    return "expression macro";
  }

  llvm_unreachable("Unhandled DefaultArgumentKind in switch.");
}
static StringRef getDumpString(ObjCSelectorExpr::ObjCSelectorKind value) {
  switch (value) {
    case ObjCSelectorExpr::Method: return "method";
    case ObjCSelectorExpr::Getter: return "getter";
    case ObjCSelectorExpr::Setter: return "setter";
  }

  llvm_unreachable("Unhandled ObjCSelectorExpr in switch.");
}
static StringRef getDumpString(AccessSemantics value) {
  switch (value) {
    case AccessSemantics::Ordinary: return "ordinary";
    case AccessSemantics::DirectToStorage: return "direct_to_storage";
    case AccessSemantics::DirectToImplementation: return "direct_to_impl";
    case AccessSemantics::DistributedThunk: return "distributed_thunk";
  }

  llvm_unreachable("Unhandled AccessSemantics in switch.");
}
static StringRef getDumpString(MetatypeRepresentation value) {
  switch (value) {
    case MetatypeRepresentation::Thin: return "thin";
    case MetatypeRepresentation::Thick: return "thick";
    case MetatypeRepresentation::ObjC: return "@objc";
  }

  llvm_unreachable("Unhandled MetatypeRepresentation in switch.");
}
static StringRef getDumpString(StringLiteralExpr::Encoding value) {
  switch (value) {
    case StringLiteralExpr::UTF8: return "utf8";
    case StringLiteralExpr::OneUnicodeScalar: return "unicodeScalar";
  }

  llvm_unreachable("Unhandled StringLiteral in switch.");
}
static StringRef getDumpString(CtorInitializerKind value) {
  switch (value) {
    case CtorInitializerKind::Designated: return "designated";
    case CtorInitializerKind::Convenience: return "convenience";
    case CtorInitializerKind::ConvenienceFactory: return "convenience_factory";
    case CtorInitializerKind::Factory: return "factory";
  }

  llvm_unreachable("Unhandled CtorInitializerKind in switch.");
}
static StringRef getDumpString(Associativity value) {
  switch (value) {
    case Associativity::None: return "none";
    case Associativity::Left: return "left";
    case Associativity::Right: return "right";
  }

  llvm_unreachable("Unhandled Associativity in switch.");
}
static StringRef getDumpString(CheckedCastKind kind) {
  return getCheckedCastKindName(kind);
}
static StringRef getDumpString(bool value) {
  return value ? "true" : "false";
}
static StringRef getDumpString(AccessLevel level) {
  return getAccessLevelSpelling(level);
}
static StringRef getDumpString(LifetimeAnnotation lifetime) {
  switch (lifetime) {
  case LifetimeAnnotation::EagerMove:
    return "_eagerMove";
  case LifetimeAnnotation::Lexical:
    return "_lexical";
  case LifetimeAnnotation::None:
    return "";
  }
  
  llvm_unreachable("Unhandled LifetimeAnnotation in switch.");
}
static StringRef getDumpString(AccessorKind kind) {
  return getAccessorKindString(kind);
}
static StringRef getDumpString(MagicIdentifierLiteralExpr::Kind kind) {
  return MagicIdentifierLiteralExpr::getKindString(kind);
}
static StringRef getDumpString(ObjectLiteralExpr::LiteralKind kind) {
  return ObjectLiteralExpr::getLiteralKindPlainName(kind);
}
static StringRef getDumpString(ParamSpecifier specifier) {
  return ParamDecl::getSpecifierSpelling(specifier);
}
static StringRef getDumpString(ValueOwnership ownership) {
  switch (ownership) {
  case ValueOwnership::Default:
      return "";
  case ValueOwnership::Owned:
      return "owned";
  case ValueOwnership::Shared:
      return "shared";
  case ValueOwnership::InOut:
      return "inout";
  }

  llvm_unreachable("Unhandled ValueOwnership in switch.");
}
static StringRef getDumpString(PlatformKind kind) {
  return platformString(kind);
}
static StringRef getDumpString(ForeignErrorConvention::IsOwned_t owned) {
  switch (owned) {
  case swift::ForeignErrorConvention::IsNotOwned:
    return "unowned";
  case swift::ForeignErrorConvention::IsOwned:
    return "owned";
  }

  llvm_unreachable("Unhandled ForeignErrorConvention::IsOwned_t in switch.");
}
static StringRef getDumpString(RequirementKind kind) {
  switch (kind) {
    case RequirementKind::SameShape: return "same_shape";
    case RequirementKind::Conformance: return "conforms_to";
    case RequirementKind::Layout: return "has_layout";
    case RequirementKind::Superclass: return "subclass_of";
    case RequirementKind::SameType: return "same_type";
  }

  llvm_unreachable("Unhandled RequirementKind in switch.");
}
static StringRef getDumpString(RequirementReprKind kind) {
  switch (kind) {
  case RequirementReprKind::TypeConstraint: return "type_constraint";
  case RequirementReprKind::SameType: return "same_type";
  case RequirementReprKind::LayoutConstraint: return "layout_constraint";
  }

  llvm_unreachable("Unhandled RequirementKind in switch.");
}
static StringRef getDumpString(ClangImporterSynthesizedTypeAttr::Kind kind) {
  switch (kind) {
  case ClangImporterSynthesizedTypeAttr::Kind::NSErrorWrapper:
    return "NSErrorWrapper";
  case ClangImporterSynthesizedTypeAttr::Kind::NSErrorWrapperAnon:
    return "NSErrorWrapperAnon";
  }
  llvm_unreachable("unhandled ClangImporterSynthesizedTypeAttr::Kind");
}
static StringRef getDumpString(ExternKind kind) {
  switch (kind) {
  case ExternKind::C:
    return "C";
  case ExternKind::Wasm:
    return "Wasm";
  }
  llvm_unreachable("unhandled ExternKind");
}
static StringRef getDumpString(InlineKind kind) {
  switch (kind) {
  case InlineKind::Always:
    return "always";
  case InlineKind::Never:
    return "never";
  }
  llvm_unreachable("unhandled InlineKind");
}
static StringRef getDumpString(MacroRole role) {
  return getMacroRoleString(role);
}
static StringRef getDumpString(EffectsKind kind) {
  switch (kind) {
  case EffectsKind::ReadNone:
    return "ReadNone";
  case EffectsKind::ReadOnly:
    return "ReadOnly";
  case EffectsKind::ReleaseNone:
    return "ReleaseNone";
  case EffectsKind::ReadWrite:
    return "ReadWrite";
  case EffectsKind::Unspecified:
    return "Unspecified";
  case EffectsKind::Custom:
    return "Custom";
  }
  llvm_unreachable("unhandled EffectsKind");
}
static StringRef getDumpString(ExclusivityAttr::Mode mode) {
  switch (mode) {
  case ExclusivityAttr::Mode::Checked:
    return "checked";
  case ExclusivityAttr::Mode::Unchecked:
    return "unchecked";
  }
  llvm_unreachable("unhandled ExclusivityAttr::Mode");
}
static StringRef getDumpString(OptimizationMode mode) {
  switch (mode) {
  case OptimizationMode::NotSet:
    return "<not set>";
  case OptimizationMode::NoOptimization:
    return "NoOptimization";
  case OptimizationMode::ForSpeed:
    return "ForSpeed";
  case OptimizationMode::ForSize:
    return "ForSize";
  }
}
static StringRef getDumpString(NonSendableKind kind) {
  switch (kind) {
  case NonSendableKind::Assumed:
    return "Assumed";
  case NonSendableKind::Specific:
    return "Specific";
  }
}
static StringRef getDumpString(FunctionRefInfo::ApplyLevel applyLevel) {
  switch (applyLevel) {
  case FunctionRefInfo::ApplyLevel::Unapplied:
    return "unapplied";
  case FunctionRefInfo::ApplyLevel::SingleApply:
    return "single_apply";
  case FunctionRefInfo::ApplyLevel::DoubleApply:
    return "double_apply";
  }
}
static StringRef getDumpString(ExplicitSafety safety) {
  switch (safety) {
  case ExplicitSafety::Unspecified:
    return "unspecified";
  case ExplicitSafety::Safe:
    return "safe";
  case ExplicitSafety::Unsafe:
    return "unsafe";
  }
}
static StringRef getDumpString(ConformanceEntryKind kind) {
  switch (kind) {
  case ConformanceEntryKind::Inherited:
    return "inherited";
  case ConformanceEntryKind::Explicit:
    return "explicit";
  case ConformanceEntryKind::PreMacroExpansion:
    return "pre_macro_expansion";
  case ConformanceEntryKind::Synthesized:
    return "synthesized";
  case ConformanceEntryKind::Implied:
    return "implied";
  }
  llvm_unreachable("unhandled ConformanceEntryKind");
}
static StringRef getDumpString(StringRef s) {
  return s;
}
static unsigned getDumpString(unsigned value) {
  return value;
}
static size_t getDumpString(size_t value) {
  return value;
}
static void *getDumpString(void *value) { return value; }

static StringRef getDumpString(Identifier ident) { return ident.str(); }

//===----------------------------------------------------------------------===//
//  Decl printing.
//===----------------------------------------------------------------------===//

// Print a name.
static void printName(raw_ostream &os, DeclName name) {
  if (!name)
    os << "<anonymous>";
  else
    os << name;
}

static Type defaultGetTypeOfExpr(Expr *E) { return E->getType(); }
static Type defaultGetTypeOfKeyPathComponent(KeyPathExpr *E, unsigned index) {
  return E->getComponents()[index].getComponentType();
}

using VisitedConformances = llvm::SmallPtrSetImpl<const ProtocolConformance *>;

namespace {
  /// Represents a label attached to some data in the AST being dumped.
  ///
  /// This type exists to balance the simplified S-expression output with
  /// the need for additional structure in more complex data formats like
  /// JSON. The S-expression output, in most cases, prints nested data as
  /// unlabeled children (indented one more step) and allows fields to be
  /// printed without explicit names, but this isn't compatible with JSON where
  /// each value needs its own unique label/key (or it needs to be an element
  /// of an array).
  class Label {
    StringRef Text;
    bool IsOptional;

    Label(StringRef text, bool isOptional)
        : Text(text), IsOptional(isOptional) {}

  public:
    /// Returns a new label that is always printed, even in the default
    /// (S-expression) output.
    static Label always(StringRef text) {
      return Label(text, /*isOptional=*/ false);
    }

    /// Returns a new label that is optional on output formats that allow
    /// labels to be omitted.
    static Label optional(StringRef text) {
      return Label(text, /*isOptional=*/ true);
    }

    /// Returns true if the label's text is empty.
    bool empty() const { return Text.empty(); }

    /// Returns true if the label can be omitted by output formats that support
    /// optional labels.
    bool isOptional() const { return IsOptional; }

    /// Returns the label's text.
    StringRef text() const { return Text; }
  };

  /// Defines the interface for low-level printing operations that take place
  /// when dumping a Swift AST.
  class PrintWriterBase {
  protected:
    /// Only used by the S-expression writer to change the dumper to use
    /// single quotes when printing substitution maps in full.
    char Quote = '\"';

    /// Tracks the source buffer ID of the main source file, which subclasses
    /// can use to distinguish ranges/locations in that file vs. ranges in other
    /// buffers, like macro expansions.
    unsigned MainBufferID;

  public:
    virtual ~PrintWriterBase() {}

    char quote() const { return Quote; }
    void setQuote(char quote) { Quote = quote; }

    void setMainBufferID(unsigned bufferID) { MainBufferID = bufferID; }

    /// Call `body` in a context where the printer is ready for a child to be
    /// printed.
    virtual void printRecArbitrary(std::function<void(Label)> body,
                                   Label label) = 0;

    /// Print a range of nodes as a single "array" child node.
    virtual void printRecRange(std::function<void()> body,
                               Label label) = 0;

    /// Call `body` in a context where the printer is ready for a list of
    /// children to be printed.
    virtual void printListArbitrary(std::function<void()> body, Label label) = 0;

    /// Print the beginning of a new node, including its type and an optional
    /// label for it.
    virtual void printHead(StringRef name, TerminalColor color, Label label) = 0;

    /// Print the end of a new node.
    virtual void printFoot() = 0;

    /// Print a field with a short keyword-style value, printing the value by
    /// passing a closure that takes a \c raw_ostream.
    virtual void printFieldRaw(std::function<void(llvm::raw_ostream &)> body,
                               Label label, TerminalColor color) = 0;

    /// Print a field with a long value that will be automatically quoted and
    /// escaped, printing the value by passing a closure that takes a
    /// \c raw_ostream.
    virtual void printFieldQuotedRaw(
        std::function<void(llvm::raw_ostream &)> body, Label name,
        TerminalColor color) = 0;

    /// Print a simple boolean value, printing the value by passing a closure
    /// that takes a \c raw_ostream.
    virtual void printFlagRaw(std::function<void(llvm::raw_ostream &)> body,
                              TerminalColor color) = 0;

    /// Print a field containing a node's source location.
    virtual void printSourceLoc(const SourceLoc L, const ASTContext *Ctx,
                                Label label) = 0;

    /// Print a field containing a node's source range.
    virtual void printSourceRange(const SourceRange R, const ASTContext *Ctx,
                                  Label label) = 0;

    /// Indicates whether the output format is meant to be parsable. Parsable
    /// output should use structure rather than stringification to convey
    /// detailed information, and generally provides more information than the
    /// non-parsable formats, which are usually meant for human debugging.
    virtual bool isParsable() const = 0;
  };

  /// Implements the default (pseudo-S-expression) output format for `-dump-ast`.
  class DefaultWriter : public PrintWriterBase {
    raw_ostream &OS;
    unsigned Indent;

  public:
    DefaultWriter(raw_ostream &os, unsigned indent) : OS(os), Indent(indent) {}

    void printRecArbitrary(std::function<void(Label)> body,
                           Label label) override {
      Indent += 2;
      OS << '\n';
      body(label);
      Indent -= 2;
    }

    void printRecRange(std::function<void()> body,
                       Label label) override {
      printRecArbitrary([&](Label label) {
        printHead("array", ASTNodeColor, label);
        body();
        printFoot();
      }, label);
    }

    void printListArbitrary(std::function<void()> body, Label label) override {
      // This writer ignores the label and simply prints the list directly
      // underneath its parent.
      body();
    }

    void printHead(StringRef name, TerminalColor color, Label label) override {
      OS.indent(Indent);
      PrintWithColorRAII(OS, ParenthesisColor) << '(';
      if (!label.isOptional() && !label.empty()) {
        PrintWithColorRAII(OS, FieldLabelColor) << label.text();
        OS << "=";
      }
      PrintWithColorRAII(OS, color) << name;
    }

    void printFoot() override {
      PrintWithColorRAII(OS, ParenthesisColor) << ')';
    }

    void printFieldRaw(std::function<void(llvm::raw_ostream &)> body,
                       Label label, TerminalColor color) override {
      OS << " ";
      if (!label.isOptional() && !label.empty())
        PrintWithColorRAII(OS, color) << label.text() << "=";
      std::string value;
      llvm::raw_string_ostream SOS(value);
      body(SOS);
      PrintWithColorRAII(OS, color) << value;
    }

    void printFieldQuotedRaw(std::function<void(llvm::raw_ostream &)> body,
                             Label name, TerminalColor color) override {
      printFieldRaw([&](raw_ostream &OS) {
        OS << Quote;
        { escaping_ostream escOS(OS); body(escOS); }
        OS << Quote;
      }, name, color);
    }

    void printFlagRaw(std::function<void(llvm::raw_ostream &)> body,
                      TerminalColor color) override {
      printFieldRaw(body, Label::always(""), color);
    }

    void printSourceLoc(const SourceLoc L, const ASTContext *Ctx,
                        Label label) override {
      printFieldRaw([&](raw_ostream &OS) {
        escaping_ostream escOS(OS);
        L.print(escOS, Ctx->SourceMgr);
      }, label, LocationColor);
    }

    void printSourceRange(const SourceRange R, const ASTContext *Ctx,
                          Label label) override {
      printFieldRaw([&](raw_ostream &OS) {
        escaping_ostream escOS(OS);
        R.print(escOS, Ctx->SourceMgr, /*PrintText=*/false);
      }, label, RangeColor);
    }

    bool isParsable() const override { return false; }
  };

  /// Implements JSON formatted output for `-ast-dump`.
  class JSONWriter : public PrintWriterBase {
    llvm::json::OStream OS;
    std::vector<bool> InObjectStack;

  public:
    JSONWriter(raw_ostream &os, unsigned indent = 0) : OS(os, indent) {}

    void printRecArbitrary(std::function<void(Label)> body,
                           Label label) override {
      // The label is ignored if we're not printing inside an object (meaning
      // we must be in an array).
      assert(!InObjectStack.empty() && "printHead or printListArbitrary not called before printRecArbitrary");
      if (InObjectStack.back()) {
        OS.attributeBegin(label.text());
        body(Label::optional(""));
        OS.attributeEnd();
      } else {
        body(Label::optional(""));
      }
    }

    void printRecRange(std::function<void()> body, Label label) override {
      printListArbitrary([&]{ body(); }, label);
    }

    void printListArbitrary(std::function<void()> body, Label label) override {
      OS.attributeBegin(label.text());
      OS.arrayBegin();
      InObjectStack.push_back(false);
      body();
      InObjectStack.pop_back();
      OS.arrayEnd();
      OS.attributeEnd();
    }

    void printHead(StringRef name, TerminalColor color, Label label) override {
      OS.objectBegin();
      InObjectStack.push_back(true);
      OS.attribute(label.empty() ? "_kind" : label.text(), name);
    }

    void printFoot() override {
      InObjectStack.pop_back();
      OS.objectEnd();
    }

    void printFieldRaw(std::function<void(llvm::raw_ostream &)> body,
                       Label label, TerminalColor color) override {
      std::string value;
      llvm::raw_string_ostream SOS(value);
      body(SOS);
      // The label is ignored if we're not printing inside an object (meaning
      // we must be in an array).
      if (InObjectStack.back()) {
        OS.attribute(label.text(), value);
      } else {
        OS.value(value);
      }
    }

    void printFieldQuotedRaw(std::function<void(llvm::raw_ostream &)> body,
                             Label label, TerminalColor color) override {
      // No need to do special quoting for complex values; the JSON output
      // stream will do this for us.
      printFieldRaw(body, label, color);
    }

    void printFlagRaw(std::function<void(llvm::raw_ostream &)> body,
                      TerminalColor color) override {
      std::string flag;
      llvm::raw_string_ostream SOS(flag);
      body(SOS);
      OS.attribute(flag, true);
    }

    void printSourceLoc(const SourceLoc L, const ASTContext *Ctx,
                        Label label) override {
      // For compactness, we only print source ranges in JSON, since they
      // provide a superset of this information.
    }

    void printSourceRange(const SourceRange R, const ASTContext *Ctx,
                          Label label) override {
      OS.attributeBegin(label.text());
      OS.objectBegin();

      SourceManager &srcMgr = Ctx->SourceMgr;
      unsigned startBufferID = srcMgr.findBufferContainingLoc(R.Start);
      unsigned startOffset = srcMgr.getLocOffsetInBuffer(R.Start,
                                                         startBufferID);
      OS.attribute("start", startOffset);

      unsigned endBufferID = srcMgr.findBufferContainingLoc(R.End);
      unsigned endOffset = srcMgr.getLocOffsetInBuffer(R.End, endBufferID);
      OS.attribute("end", endOffset);

      // Only print the buffer ID when it doesn't match the main ID, so that we
      // distinguish macro expansions but don't bloat the output with the main
      // file name repeated over and over.
      if (startBufferID != MainBufferID)
        OS.attribute("buffer_id", srcMgr.getIdentifierForBuffer(startBufferID));

      OS.objectEnd();
      OS.attributeEnd();
    }

    bool isParsable() const override { return true; }
  };

  /// PrintBase - Base type for recursive structured dumps of AST nodes.
  ///
  /// Please keep direct I/O, especially of structural elements like
  /// parentheses and quote marks, confined to this base class. This will help
  /// if we eventually support alternate output formats for AST dumps.
  class PrintBase {
  protected:
    PrintWriterBase &Writer;
  public:
    ASTDumpMemberLoading MemberLoading;
    llvm::function_ref<Type(Expr *)> GetTypeOfExpr;
    llvm::function_ref<Type(TypeRepr *)> GetTypeOfTypeRepr;
    llvm::function_ref<Type(KeyPathExpr *E, unsigned index)>
        GetTypeOfKeyPathComponent;
    char quote = '"';

    explicit PrintBase(
        PrintWriterBase &writer,
        ASTDumpMemberLoading memberLoading = ASTDumpMemberLoading::None,
        llvm::function_ref<Type(Expr *)> getTypeOfExpr = defaultGetTypeOfExpr,
        llvm::function_ref<Type(TypeRepr *)> getTypeOfTypeRepr = nullptr,
        llvm::function_ref<Type(KeyPathExpr *E, unsigned index)>
            getTypeOfKeyPathComponent = defaultGetTypeOfKeyPathComponent)
        : Writer(writer), MemberLoading(memberLoading),
          GetTypeOfExpr(getTypeOfExpr), GetTypeOfTypeRepr(getTypeOfTypeRepr),
          GetTypeOfKeyPathComponent(getTypeOfKeyPathComponent) {}

    bool isTypeChecked() const {
      return MemberLoading == ASTDumpMemberLoading::TypeChecked;
    }

    /// Call `Body` in a context where the printer is ready for a child to be
    /// printed.
    void printRecArbitrary(std::function<void(Label)> body, Label label) {
      Writer.printRecArbitrary(body, label);
    }

    /// Print a declaration as a child node.
    void printRec(Decl *D, Label label);

    /// Print an expression as a child node.
    void printRec(Expr *E, Label label);

    /// Print a statement as a child node.
    void printRec(Stmt *S, const ASTContext *Ctx, Label label);

    /// Print a type representation as a child node.
    void printRec(TypeRepr *T, Label label);

    /// Print a pattern as a child node.
    void printRec(const Pattern *P, Label label);

    /// Print a type as a child node.
    void printRec(Type ty, Label label);

    /// Print an attribute as a child node.
    void printRec(const DeclAttribute *Attr, const ASTContext *Ctx,
                  DeclContext *dc, Label label);

    /// Print an \c ASTNode as a child node.
    void printRec(const ASTNode &Elt, const ASTContext *Ctx,
                  Label label) {
      if (auto *SubExpr = Elt.dyn_cast<Expr*>())
        printRec(SubExpr, label);
      else if (auto *SubStmt = Elt.dyn_cast<Stmt*>())
        printRec(SubStmt, Ctx, label);
      else
        printRec(Elt.get<Decl*>(), label);
    }

    /// Print a statement condition element as a child node.
    void printRec(StmtConditionElement C, const ASTContext *Ctx,
                  Label label) {
      switch (C.getKind()) {
      case StmtConditionElement::CK_Boolean:
        return printRec(C.getBoolean(), label);
      case StmtConditionElement::CK_PatternBinding:
          printRecArbitrary([&](Label label) {
            printHead("pattern", PatternColor, label);
            printRec(C.getPattern(), Label::optional("pattern"));
            printRec(C.getInitializer(), Label::optional("initializer"));
            printFoot();
          }, label);
        break;
      case StmtConditionElement::CK_Availability:
        printRecArbitrary([&](Label label) {
          printHead("#available", PatternColor, label);
          printList(C.getAvailability()->getQueries(),
                    [&](auto *query, Label label) { printRec(query, label); },
                    Label::optional("queries"));
          printFoot();
        }, label);
        break;
      case StmtConditionElement::CK_HasSymbol:
        printRecArbitrary([&](Label label) {
          printHead("#_hasSymbol", PatternColor, label);
          printSourceRange(C.getSourceRange(), Ctx);
          printRec(C.getHasSymbolInfo()->getSymbolExpr(),
                   Label::optional("symbol_expr"));
          printFoot();
        }, label);
        break;
      }
    }

    /// Print an availability spec as a child node.
    void printRec(AvailabilitySpec *Spec, Label label) {
      printRecArbitrary(
          [&](Label label) {
            printHead("availability_spec", PatternColor, label);
            printFieldRaw(
                [&](llvm::raw_ostream &OS) {
                  Spec->getDomainOrIdentifier().print(OS);
                },
                Label::always("domain"));
            if (!Spec->getRawVersion().empty())
              printFieldRaw(
                  [&](llvm::raw_ostream &OS) { OS << Spec->getRawVersion(); },
                  Label::always("version"));
            printFoot();
          },
          label);
    }

    /// Print a range of nodes as a single "array" child node.
    template <typename NodeRange>
    void printRecRange(const NodeRange &range, Label topLabel) {
      Writer.printRecRange([&]() {
        for (auto node : range) {
          printRec(node, Label::optional(""));
        }
      }, topLabel);
    }

    /// Print a range of nodes as a single "array" child node.
    template <typename NodeRange>
    void printRecRange(const NodeRange &range, const ASTContext *Ctx,
                       Label topLabel) {
      Writer.printRecRange([&]() {
        for (auto node : range) {
          printRec(node, Ctx, Label::optional(""));
        }
      }, topLabel);
    }

    /// Print the beginning of a new node, including its type and an optional
    /// label for it.
    void printHead(StringRef Name, TerminalColor Color, Label label) {
      Writer.printHead(Name, Color, label);
    }

    /// Print the end of a new node.
    void printFoot() {
      Writer.printFoot();
    }

    /// Print a single argument as a child node.
    void printRec(const Argument &arg, Label label) {
      printRecArbitrary([&](Label L) {
        printHead("argument", ExprColor, L);

        auto label = arg.getLabel();
        if (!label.empty()) {
          printFieldQuoted(label.str(), Label::always("label"), ArgumentsColor);
        }
        printFlag(arg.isInOut(), "inout", ArgModifierColor);

        printRec(arg.getExpr(), Label::optional("expr"));
        printFoot();
      }, label);
    }

    /// Print an argument list as a child node.
    void printRec(const ArgumentList *argList, Label label) {
      printRecArbitrary([&](Label label) {
        visitArgumentList(argList, label);
      }, label);
    }

    /// Print an argument list node.
    void visitArgumentList(const ArgumentList *argList, Label label) {
      printHead("argument_list", ExprColor, label);

      printFlag(argList->isImplicit(), "implicit", ArgModifierColor);

      if (argList->hasAnyArgumentLabels()) {
        printFieldQuotedRaw([&](raw_ostream &OS) {
          for (auto arg : *argList) {
            auto label = arg.getLabel();
            OS << (label.empty() ? "_" : label.str()) << ":";
          }
        }, Label::always("labels"), ArgumentsColor);
      }

      printList(*argList, [&](auto arg, Label label) {
        printRec(arg, label);
      }, Label::optional("args"));

      printFoot();
    }

    /// Print a parameter list as a child node.
    void printRec(const ParameterList *params, Label label,
                  const ASTContext *ctx = nullptr) {
      printRecArbitrary([&](Label label) {
        visitParameterList(params, label, ctx);
      }, label);
    }

    /// Print a parameter list node.
    void visitParameterList(const ParameterList *params,
                            Label label, const ASTContext *ctx = nullptr) {
      printHead("parameter_list", ParameterColor, label);

      if (!ctx && params->size() != 0 && params->get(0))
        ctx = &params->get(0)->getASTContext();
      printSourceRange(params->getSourceRange(), ctx);

      printList(*params, [&](auto P, Label label) {
        printRec(const_cast<ParamDecl *>(P), label);
      }, Label::optional("params"));

      printFoot();
    }

    /// Print an \c IfConfigClause as a child node.
    void printRec(const IfConfigClause &Clause, Label label,
                  const ASTContext *Ctx = nullptr) {
      printRecArbitrary([&](Label label) {
        printHead((Clause.Cond ? "#if:" : "#else:"), StmtColor, label);

        printFlag(Clause.isActive, "active", DeclModifierColor);

        if (Clause.Cond) {
          printRec(Clause.Cond, Label::optional("cond"));
        }
        printRecRange(Clause.Elements, Ctx, Label::always("elements"));

        printFoot();
      }, label);
    }

    /// Print a substitution map as a child node.
    void printRec(SubstitutionMap map, Label label) {
      SmallPtrSet<const ProtocolConformance *, 4> Dumped;
      printRec(map, Dumped, label);
    }

    /// Print a substitution map as a child node.
    void printRec(SubstitutionMap map, VisitedConformances &visited,
                  Label label);

    /// Print a substitution map as a child node.
    void printRec(const ProtocolConformanceRef &conf,
                  VisitedConformances &visited, Label label);

    /// Print a conformance reference as a child node.
    void printRec(const ProtocolConformanceRef &conf, Label label) {
      SmallPtrSet<const ProtocolConformance *, 4> Dumped;
      printRec(conf, Dumped, label);
    }

    /// Print a conformance reference as a child node.
    void printRec(const ProtocolConformance *conformance,
                  VisitedConformances &visited, Label label);

    // Print a field that describes the actor isolation associated with an AST
    // node.
    void printIsolation(const ActorIsolation &isolation) {
      switch (isolation) {
      case ActorIsolation::Unspecified:
      case ActorIsolation::NonisolatedUnsafe:
        break;

      case ActorIsolation::Nonisolated:
        printFlag(true, "nonisolated", CapturesColor);
        break;

      case ActorIsolation::Erased:
        printFlag(true, "dynamically_isolated", CapturesColor);
        break;

      case ActorIsolation::CallerIsolationInheriting:
        printFlag(true, "isolated_to_caller_isolation", CapturesColor);
        break;

      case ActorIsolation::ActorInstance:
        printReferencedDeclWithContextField(isolation.getActorInstance(),
                                            Label::always("actor_isolated"),
                                            CapturesColor);
        break;

      case ActorIsolation::GlobalActor:
        printTypeField(isolation.getGlobalActor(),
                       Label::always("global_actor_isolated"), PrintOptions(),
                       CapturesColor);
        break;
      }
    }

    /// Print a requirement node.
    void visitRequirement(const Requirement &requirement, Label label) {
      printHead("requirement", ASTNodeColor, label);

      PrintOptions opts;
      opts.ProtocolQualifiedDependentMemberTypes = true;
      printTypeField(requirement.getFirstType(), Label::optional("first_type"),
                     opts);

      printField(requirement.getKind(), Label::optional("kind"));

      switch (requirement.getKind()) {
      case RequirementKind::Layout:
        printFieldQuoted(requirement.getLayoutConstraint(),
                         Label::optional("layout"));
        break;
      case RequirementKind::Conformance:
        printReferencedDeclField(requirement.getProtocolDecl(),
                                 Label::optional("protocol"));
        break;
      default:
        printTypeField(requirement.getSecondType(),
                       Label::optional("second_type"), opts);
      }

      printFoot();
    }

    /// Print a requirement as a child node.
    void printRec(const Requirement &requirement, Label label) {
      printRecArbitrary([&](Label label) {
        visitRequirement(requirement, label);
      }, label);
    }

    /// Print a requirement node.
    void visitRequirementRepr(const RequirementRepr &requirement, Label label) {
      printHead("requirement_repr", ASTNodeColor, label);

      printField(requirement.getKind(), Label::optional("kind"));
      switch (requirement.getKind()) {
      case RequirementReprKind::TypeConstraint:
        printRec(requirement.getSubjectRepr(), Label::optional("first_type"));
        printRec(requirement.getConstraintRepr(), Label::optional("second_type"));
        break;
      case RequirementReprKind::SameType:
        printRec(requirement.getFirstTypeRepr(), Label::optional("first_type"));
        printRec(requirement.getSecondTypeRepr(), Label::optional("second_type"));
        break;
      case RequirementReprKind::LayoutConstraint:
        printRec(requirement.getSubjectRepr(), Label::optional("first_type"));
        printFieldQuoted(requirement.getLayoutConstraint(),
                         Label::optional("layout"));
        break;
      }

      printFoot();
    }

    /// Print a requirement as a child node.
    void printRec(const RequirementRepr &requirement, Label label) {
      printRecArbitrary([&](Label label) {
        visitRequirementRepr(requirement, label);
      }, label);
    }

    /// Print a protocol typealias node.
    void visitProtocolTypeAlias(const ProtocolTypeAlias &TA, Label label) {
      printHead("protocol_typealias", ASTNodeColor, label);

      printNameRaw([&](raw_ostream &os) {
        os << TA.getName();
      }, Label::optional("name"));

      PrintOptions opts;
      opts.ProtocolQualifiedDependentMemberTypes = true;
      printTypeField(TA.getUnderlyingType(),
                     Label::always("underlying_type"), opts);

      printFoot();
    }

    /// Print a protocol typealias as a child node.
    void printRec(const ProtocolTypeAlias &TA, Label label) {
      printRecArbitrary([&](Label label) {
        visitProtocolTypeAlias(TA, label);
      }, label);
    }

    /// Print a captured value as a child node.
    void printRec(const CapturedValue &value, Label label) {
      printRecArbitrary([&](Label label) {
        printHead("captured_value", CapturesColor, label);
        printFlag(value.isDirect(), "is_direct");
        printFlag(value.isNoEscape(), "is_no_escape");
        printFlag(value.isLocalCapture(), "is_local_capture");
        printFlag(value.isDynamicSelfMetadata(), "is_dynamic_self_metadata");
        if (auto *D = value.getDecl()) {
          printRec(D, Label::always("decl"));
        }
        if (auto *E = value.getExpr()) {
          printRec(E, Label::always("expr"));
        }
        if (auto *OV = value.getOpaqueValue()) {
          printRec(OV, Label::always("opaque_value"));
        }
        if (value.isPackElement()) {
          if (auto *PE = value.getPackElement()) {
            printRec(PE, Label::always("pack_element"));
          }
          if (auto PET = value.getPackElementType()) {
            printRec(PET, Label::always("pack_element_type"));
          }
        }
        printFoot();
      }, label);
    }

    /// Print a field containing function capture info.
    void printCaptureInfoField(const std::optional<CaptureInfo> &captureInfo,
                               Label label) {
      if (!captureInfo.has_value())
        return;
      if (captureInfo->isTrivial())
        return;

      if (Writer.isParsable()) {
        printList(captureInfo->getCaptures(), [&](auto capture, Label label) {
          printRec(capture, label);
        }, label);
      } else {
        printFieldRaw([&](raw_ostream &OS) {
          captureInfo->print(OS);
        }, label, CapturesColor);
      }
    }

    /// Print the requirements of a trailing where clause.
    void printWhereClause(const TrailingWhereClause *Where, Label label) {
      if (!Where) {
        return;
      }
      if (Writer.isParsable()) {
        printList(Where->getRequirements(), [&](auto req, Label label) {
          printRec(req, label);
        }, label);
      } else {
        printFieldQuotedRaw([&](raw_ostream &OS) {
          Where->print(OS, /*printWhereKeyword*/ false);
        }, label);
      }
    };

    /// Print a field with a short keyword-style value, printing the value by
    /// passing a closure that takes a \c raw_ostream.
    template<typename Fn>
    void printFieldRaw(Fn body, Label label,
                       TerminalColor color = FieldLabelColor) {
      Writer.printFieldRaw([&](raw_ostream &OS) { body(OS); }, label, color);
    }

    /// Print a field with a short keyword-style value. The value will be
    /// formatted using a \c getDumpString() overload.
    template<typename T>
    void printField(const T &value, Label label,
                    TerminalColor color = FieldLabelColor) {
      printFieldRaw([&](raw_ostream &OS) { OS << getDumpString(value); },
                    label, color);
    }

    /// Print a field with a long value that will be automatically quoted and
    /// escaped, printing the value by passing a closure that takes a
    /// \c raw_ostream.
    template<typename Fn>
    void printFieldQuotedRaw(Fn body, Label label,
                             TerminalColor color = FieldLabelColor) {
      Writer.printFieldQuotedRaw(body, label, color);
    }

    /// Print a field with a long value that will be automatically quoted and
    /// escaped.
    template<typename T>
    void printFieldQuoted(const T &value, Label label,
                                  TerminalColor color = FieldLabelColor) {
      printFieldQuotedRaw([&](raw_ostream &OS) { OS << value; }, label, color);
    }

    /// Print a simple boolean value, printing the value by passing a closure
    /// that takes a \c raw_ostream.
    template<typename Fn>
    void printFlagRaw(Fn body, TerminalColor color = FieldLabelColor) {
      Writer.printFlagRaw(body, color);
    }

    /// Print a simple boolean value unconditionally.
    void printFlag(StringRef name, TerminalColor color = FieldLabelColor) {
      printFlagRaw([&](raw_ostream &OS) { OS << name; }, color);
    }

    /// Print a simple boolean value.
    void printFlag(bool isSet, StringRef name,
                   TerminalColor color = FieldLabelColor) {
      if (isSet)
        printFlag(name, color);
    }

    /// Prints any structure necessary to render a list of items, calling
    /// the given function to produce the contents of the list.
    void printListArbitrary(std::function<void()> body, Label label) {
      Writer.printListArbitrary(body, label);
    }

    /// Prints a list of values.
    template <typename T, typename F>
    void printList(T &&list, F fn, Label label) {
      if (list.begin() == list.end())
        return;
      Writer.printListArbitrary([&]{
        for (const auto &elem : list) {
          fn(elem, Label::optional(""));
        }
      }, label);
    }

    /// Prints a list of strings in a compact form depending on the output
    /// format, where the given function returns a string for each element in
    /// the sequence. The default format will interleave these strings with
    /// commas, whereas parsable outputs will render them as a structured list
    /// of strings.
    template <typename T, typename F>
    void printStringListField(T &&list, F fn, Label label,
                              StringRef delimiter = ",",
                              TerminalColor color = FieldLabelColor) {
      if (list.begin() == list.end())
        return;

      if (Writer.isParsable()) {
        Writer.printListArbitrary([&]{
          for (const auto &elem : list) {
            printField(fn(elem), Label::optional(""));
          }
        }, label);
        return;
      }

      printFieldQuotedRaw([&](raw_ostream &OS) {
        interleave(list, OS, [&](auto item) {
          auto str = fn(item);
          OS << (str.empty() ? "''" : str);
        }, delimiter);
      }, label, color);
    }

    /// Print a field containing a node's source location.
    void printSourceLoc(const SourceLoc L, const ASTContext *Ctx,
                        Label label = Label::always("location")) {
      if (!L.isValid() || !Ctx)
        return;
      Writer.printSourceLoc(L, Ctx, label);
    }

    /// Print a field containing a node's source range.
    void printSourceRange(const SourceRange R, const ASTContext *Ctx,
                          Label label = Label::always("range")) {
      if (!R.isValid() || !Ctx)
        return;
      Writer.printSourceRange(R, Ctx, label);
    }

    /// Print a field containing a node's name, printing the node's name by
    /// passing a closure that takes a \c raw_ostream.
    template <typename Fn>
    void printNameRaw(Fn body, Label label) {
      if (label.empty()) {
        // If we were given an empty name, make sure we have a suitable default
        // fallback for parsable output formats.
        label = Label::optional("name");
      }
      printFieldQuotedRaw([&](raw_ostream &OS) { body(OS); }, label,
                          IdentifierColor);
    }

    /// Print a field containing a node's name.
    void printName(DeclName name, Label label) {
      if (Writer.isParsable()) {
        if (label.empty()) {
          // If we were given an empty name, make sure we have a suitable
          // default fallback for parsable output formats.
          label = Label::optional("name");
        }
        printRecArbitrary([&](Label label) {
          printHead("decl_name", IdentifierColor, label);
          printDeclBaseName(name.getBaseName(), Label::optional("base_name"));
          printList(name.getArgumentNames(), [&](auto arg, Label label) {
            printField(arg.str(), label);
          }, Label::optional("args"));
          printFoot();
        }, label);
        return;
      }
      printNameRaw([&](raw_ostream &OS) {
        ::printName(OS, name);
      }, label);
    }

    /// Prints a structured representation of a `DeclBaseName`, accounting for
    /// special names.
    void printDeclBaseName(DeclBaseName Basename, Label label) {
      printRecArbitrary([&](Label label) {
        printHead("base_name", IdentifierColor, label);
        switch (Basename.getKind()) {
        case DeclBaseName::Kind::Constructor:
          printField((StringRef)"init", Label::optional("special"));
          break;
        case DeclBaseName::Kind::Destructor:
          printField((StringRef)"deinit", Label::optional("special"));
          break;
        case DeclBaseName::Kind::Subscript:
          printField((StringRef)"subscript", Label::optional("special"));
          break;
        case DeclBaseName::Kind::Normal:
          printField(Basename.getIdentifier(), Label::optional("name"));
          printFlag(Basename.isOperator(), "is_operator");
        }
        printFoot();
      }, label);
    }

    /// Print an unnamed field containing a node's name, read from a declaration.
    void printDeclName(const Decl *D, Label label) {
      auto VD = dyn_cast<ValueDecl>(D);
      if (VD && VD->getName()) {
        printName(VD->getName(), label);
      } else if (!Writer.isParsable()) {
        // We don't print a name field for anonymous decls in parsable outputs.
        printFieldRaw([&](raw_ostream &OS) {
          OS << "<anonymous @ " << (const void*)D << '>';
        }, label, IdentifierColor);
      }
    }

    template <typename T>
    void printDeclContext(const T *D) {
      printField(static_cast<void *>(D->getDeclContext()),
                 Label::always("decl_context"));
    }

    /// Prints a field containing the name or the USR (based on parsability of
    /// the output) of a decl that is being referenced elsewhere.
    template <typename T>
    void printReferencedDeclField(const T *D, Label label) {
      if (Writer.isParsable() && isTypeChecked()) {
        printFieldQuoted(declUSR(D), label);
      } else {
        printFieldQuoted(D->getName(), label);
      }
    }

    /// Prints a field containing the name or the USR (based on parsability of
    /// the output) of a decl that is being referenced elsewhere.
    template <typename T>
    void printReferencedDeclWithContextField(const T *D, Label label,
                                             TerminalColor Color = DeclColor) {
      if (Writer.isParsable() && isTypeChecked()) {
        printFieldQuoted(declUSR(D), label, Color);
      } else {
        printFieldQuoted(D->printRef(), label, Color);
      }
    }

    /// Print a field containing a concrete reference to a declaration.
    void printDeclRefField(ConcreteDeclRef declRef, Label label,
                           TerminalColor Color = DeclColor) {
      if (Writer.isParsable() && isTypeChecked()) {
        // Just omit the key/value for parsable formats if there's no decl.
        if (!declRef.getDecl())
          return;

        // For parsable outputs, print much more detailed structured information
        // about the declref instead of just a human-readable string.
        printRecArbitrary([&](Label label) {
          printHead("decl_ref", Color, label);
          if (auto D = declRef.getDecl()) {
            printFieldQuoted(D->getBaseName(), Label::always("base_name"),
                             IdentifierColor);
            printFieldQuoted(declUSR(D), Label::always("decl_usr"),
                             FieldLabelColor);
            printFieldQuoted(declTypeUSR(D), Label::always("type_usr"),
                             TypeColor);
          }
          if (!declRef.getSubstitutions().empty()) {
            printRec(declRef.getSubstitutions(), Label::always("substitutions"));
          }
          printFlag(!ABIRoleInfo(declRef.getDecl()).providesAPI(),
                    "abi_only_decl");
          printFoot();
        }, label);
      } else {
        printFieldQuotedRaw([&](raw_ostream &OS) { declRef.dump(OS); }, label,
                            Color);
        if (auto D = declRef.getDecl()) {
          printFlag(!ABIRoleInfo(D).providesAPI(), "abi_only_decl");
        }
      }
    }

    /// Prints a type as a field. If writing a parsable output format, the
    /// `PrintOptions` are ignored and the type is written as a USR; otherwise,
    /// the type is stringified using the `PrintOptions`.
    void printTypeField(Type Ty, Label label,
                        const PrintOptions &opts = PrintOptions(),
                        TerminalColor Color = TypeColor) {
      if (Writer.isParsable()) {
        printField(typeUSR(Ty), label, Color);
      } else {
        printFieldQuotedRaw([&](raw_ostream &out) { Ty.print(out, opts); },
                            label, Color);
      }
    }

    /// Prints a `Type` if it is present, falling back to the `TypeRepr`
    /// otherwise (and lastly, doing nothing if the `TypeRepr` was also null).
    void printTypeOrTypeRepr(std::optional<Type> Ty, TypeRepr *Repr,
                             Label label) {
      if (Ty.has_value()) {
        printTypeField(*Ty, label);
      } else if (Repr) {
        printRec(Repr, label);
      }
    }

    void printThrowDest(ThrownErrorDestination throws, bool wantNothrow) {
      if (!throws) {
        if (wantNothrow)
          printFlag("nothrow", ExprModifierColor);

        return;
      }

      auto thrownError = throws.getThrownErrorType();
      auto contextError = throws.getContextErrorType();
      if (Writer.isParsable()) {
        // For parsable outputs, just print the full thrown and contextual error
        // information as a nice structured object, even if they're the same.
        printRecArbitrary(
            [&](Label label) {
              printHead("thrown_error_destination", IdentifierColor, label);
              printTypeField(thrownError, Label::always("thrown_type"));
              printTypeField(contextError, Label::always("context_type"));
              printFoot();
            },
            Label::always("throws"));
        return;
      }

      if (thrownError->isEqual(contextError)) {
        // No translation of the thrown error type is required, so only print
        // the thrown error type.
        Type errorExistentialType =
            contextError->getASTContext().getErrorExistentialType();

        if (errorExistentialType && thrownError->isEqual(errorExistentialType))
          printFlag("throws", ExprModifierColor);
        else {
          printFlag("throws(" + thrownError.getString() + ")", ExprModifierColor);
        }
        return;
      }

      printFlag("throws(" + thrownError.getString() + " to " +
                contextError.getString() + ")", ExprModifierColor);
    }
  };

  class PrintPattern : public PatternVisitor<PrintPattern, void, Label>,
                       public PrintBase {
  public:
    using PrintBase::PrintBase;

    void printCommon(Pattern *P, const char *Name, Label label) {
      printHead(Name, PatternColor, label);

      printFlag(P->isImplicit(), "implicit", ExprModifierColor);

      if (P->hasType()) {
        printTypeField(P->getType(), Label::always("type"));
      }
    }

    void visitParenPattern(ParenPattern *P, Label label) {
      printCommon(P, "pattern_paren", label);
      printRec(P->getSubPattern(), Label::optional("sub_pattern"));
      printFoot();
    }
    void visitTuplePattern(TuplePattern *P, Label label) {
      printCommon(P, "pattern_tuple", label);

      printStringListField(P->getElements(), [&](const TuplePatternElt &elt) {
        return elt.getLabel().str();
      }, Label::always("names"));
      printList(P->getElements(), [&](auto &elt, Label label) {
        printRec(elt.getPattern(), label);
      }, Label::optional("elements"));
      printFoot();
    }
    void visitNamedPattern(NamedPattern *P, Label label) {
      printCommon(P, "pattern_named", label);
      printDeclName(P->getDecl(), Label::optional("name"));
      printFoot();
    }
    void visitAnyPattern(AnyPattern *P, Label label) {
      if (P->isAsyncLet()) {
        printCommon(P, "async_let ", label);
      }
      printCommon(P, "pattern_any", label);
      printFoot();
    }
    void visitTypedPattern(TypedPattern *P, Label label) {
      printCommon(P, "pattern_typed", label);
      printRec(P->getSubPattern(), Label::optional("sub_pattern"));
      if (!Writer.isParsable()) {
        // In general, we don't want the TypeRepr for parsable outputs. They
        // reflect syntactic rather than semantic information, the same
        // information can be obtained through a syntax parse, and we print the
        // type USR instead.
        if (auto *repr = P->getTypeRepr()) {
          printRec(repr, Label::optional("type_repr"));
        }
      }
      printFoot();
    }

    void visitIsPattern(IsPattern *P, Label label) {
      printCommon(P, "pattern_is", label);
      printField(P->getCastKind(), Label::always("cast_kind"));
      printTypeField(P->getCastType(), Label::always("cast_to"));
      if (auto sub = P->getSubPattern()) {
        printRec(sub, Label::optional("sub_pattern"));
      }
      printFoot();
    }
    void visitExprPattern(ExprPattern *P, Label label) {
      printCommon(P, "pattern_expr", label);
      printDeclContext(P);
      switch (P->getCachedMatchOperandOwnership()) {
      case ValueOwnership::Default:
        break;
      case ValueOwnership::Shared:
        printFieldRaw([](llvm::raw_ostream &os) { os << "borrowing"; },
                      Label::always("ownership"));
        break;
      case ValueOwnership::InOut:
        printFieldRaw([](llvm::raw_ostream &os) { os << "mutating"; },
                      Label::always("ownership"));
        break;
      case ValueOwnership::Owned:
        printFieldRaw([](llvm::raw_ostream &os) { os << "consuming"; },
                      Label::always("ownership"));
        break;
      }
      if (auto m = P->getCachedMatchExpr())
        printRec(m, Label::optional("match_expr"));
      else
        printRec(P->getSubExpr(), Label::optional("sub_expr"));
      printFoot();
    }
    void visitBindingPattern(BindingPattern *P, Label label) {
      printCommon(P, "pattern_binding", label);
      printField(P->getIntroducerStringRef(), Label::always("kind"));
      printRec(P->getSubPattern(), Label::optional("sub_pattern"));
      printFoot();
    }
    void visitEnumElementPattern(EnumElementPattern *P, Label label) {
      printCommon(P, "pattern_enum_element", label);
      printDeclContext(P);

      if (Writer.isParsable()) {
        printName(P->getName().getFullName(), Label::always("element"));
      } else {
        printFieldQuotedRaw([&](raw_ostream &OS) {
          P->getParentType().print(PrintWithColorRAII(OS, TypeColor).getOS());
          OS << '.';
          PrintWithColorRAII(OS, IdentifierColor) << P->getName();
        }, Label::always("element"));
      }

      if (P->hasSubPattern()) {
        printRec(P->getSubPattern(), Label::optional("sub_pattern"));
      }
      printFoot();
    }
    void visitOptionalSomePattern(OptionalSomePattern *P, Label label) {
      printCommon(P, "pattern_optional_some", label);
      printRec(P->getSubPattern(), Label::optional("sub_pattern"));
      printFoot();
    }
    void visitBoolPattern(BoolPattern *P, Label label) {
      printCommon(P, "pattern_bool", label);
      printField(P->getValue(), Label::always("value"));
      printFoot();
    }

  };

  /// PrintDecl - Visitor implementation of Decl::print.
  class PrintDecl : public DeclVisitor<PrintDecl, void, Label>,
                    public PrintBase {
  public:
    using PrintBase::PrintBase;

  private:
    void printWhereRequirements(
        PointerUnion<const AssociatedTypeDecl *, const GenericContext *> Owner
                                ) {
      if (const auto GC = Owner.dyn_cast<const GenericContext *>()) {
        printWhereClause(GC->getTrailingWhereClause(),
                         Label::always("where_requirements"));
      } else {
        const auto ATD = Owner.get<const AssociatedTypeDecl *>();
        printWhereClause(ATD->getTrailingWhereClause(),
                         Label::always("where_requirements"));
      }
    }

    void printCommon(Decl *D, const char *Name, Label label,
                     TerminalColor Color = DeclColor) {
      printHead(Name, Color, label);

      // Parsable outputs include the USR for each decl since they can be used
      // to cross-reference them (within the AST dump itself and with other data
      // sources like indexstore and SourceKit).
      if (Writer.isParsable() && isTypeChecked()) {
        if (auto usr = declUSR(D); !usr.empty()) {
          printFieldQuoted(usr, Label::always("usr"));
        }
      }

      printDeclContext(D);

      printFlag(D->isImplicit(), "implicit", DeclModifierColor);
      printFlag(D->isHoisted(), "hoisted", DeclModifierColor);

      if (auto implAttr = D->getAttrs().getAttribute<ObjCImplementationAttr>()) {
        StringRef label =
            implAttr->isEarlyAdopter() ? "objc_impl" : "clang_impl";
        if (implAttr->CategoryName.empty())
          printFlag(label);
        else
          printFieldQuoted(implAttr->CategoryName.str(), Label::always(label));
      }

      printFlag(!ABIRoleInfo(D).providesAPI(), "abi_only");

      printSourceRange(D->getSourceRange(), &D->getASTContext());
      printFlag(D->TrailingSemiLoc.isValid(), "trailing_semi",
                DeclModifierColor);

      if (Writer.isParsable() && isTypeChecked()) {
        // Print just the USRs of any auxiliary decls associated with this decl,
        // which lets us relate macro expansions back to their originating decl
        // if desired.
        std::vector<std::string> auxiliaryUSRs;
        D->visitAuxiliaryDecls([&auxiliaryUSRs](Decl *auxDecl) {
          if (auto usr = declUSR(auxDecl); !usr.empty()) {
            auxiliaryUSRs.push_back(usr);
          }
        });
        printStringListField(
            auxiliaryUSRs, [&](auto usr) { return usr; },
            Label::always("auxiliary_decl_usrs"));
      }
    }

    void printInheritance(const IterableDeclContext *DC) {
      if (!(Writer.isParsable() && isTypeChecked())) {
        // If the output is not parsable or we're not type-checked, just print
        // the inheritance list as written.
        switch (DC->getIterableContextKind()) {
        case IterableDeclContextKind::NominalTypeDecl:
          printInherited(cast<NominalTypeDecl>(DC)->getInherited());
          break;
        case IterableDeclContextKind::ExtensionDecl:
          printInherited(cast<ExtensionDecl>(DC)->getInherited());
          break;
        }
        return;
      }

      // For parsable, type-checked output, print a more structured
      // representation of the data.
      printRecArbitrary(
          [&](Label label) {
            printHead("inheritance", FieldLabelColor, label);

            SmallPtrSet<const ProtocolConformance *, 4> dumped;
            printList(
                DC->getLocalConformances(),
                [&](auto conformance, Label label) {
                  printRec(conformance, dumped, label);
                },
                Label::always("conformances"));

            if (auto CD = dyn_cast<ClassDecl>(DC); CD && CD->hasSuperclass()) {
              printTypeField(CD->getSuperclass(),
                             Label::always("superclass_type"));
            }

            if (auto ED = dyn_cast<EnumDecl>(DC); ED && ED->hasRawType()) {
              printTypeField(ED->getRawType(), Label::always("raw_type"));
            }

            if (auto PD = dyn_cast<ProtocolDecl>(DC)) {
              printList(
                  PD->getAllInheritedProtocols(),
                  [&](auto inherited, Label label) {
                    printReferencedDeclField(inherited, label);
                  },
                  Label::always("protocols"));
              if (PD->hasSuperclass()) {
                printReferencedDeclField(PD->getSuperclassDecl(),
                                         Label::always("superclass_decl_usr"));
              }
            }

            printFoot();
          },
          Label::always("inherits"));
    }

    void printInherited(InheritedTypes Inherited) {
      if (Writer.isParsable()) {
        printList(
            Inherited.getEntries(),
            [&](InheritedEntry Super, Label label) {
              printRecArbitrary(
                  [&](Label label) {
                    printHead("inherited_entry", FieldLabelColor, label);
                    printTypeField(Super.getType(), Label::always("type"));
                    printFlag(Super.isPreconcurrency(), "preconcurrency");
                    printFlag(Super.isRetroactive(), "retroactive");
                    printFlag(Super.isSuppressed(), "suppressed");
                    printFlag(Super.isUnchecked(), "unchecked");
                    if (Super.getExplicitSafety() !=
                        ExplicitSafety::Unspecified)
                      printField(Super.getExplicitSafety(),
                                 Label::always("safety"));
                    printFoot();
                  },
                  label);
            },
            Label::always("inherits"));
      } else {
        printStringListField(
            Inherited.getEntries(),
            [&](InheritedEntry Super) {
              std::string value;
              llvm::raw_string_ostream SOS(value);
              Super.dump(SOS);
              return value;
            },
            Label::always("inherits"), /*delimiter=*/", ");
      }
    }

    void printImportPath(ImportDecl *ID, Label label) {
      // We use getRealModulePath/getRealImportPath to handle module aliasing
      // for the given imported module; for example, if
      // '-module-alias Foo=Bar' was passed and this module has 'import Foo',
      // its corresponding real module name 'Bar' is printed.

      if (Writer.isParsable()) {
        // For parsable formats, write the module path and the access path as
        // distinct lists.
        ImportPath::Builder scratch;
        auto modulePath = ID->getRealModulePath(scratch);
        printList(modulePath, [&](auto component, Label label) {
          printField(component.Item.str(), label);
        }, Label::always("module_path"));

        auto accessPath = ID->getAccessPath();
        printList(accessPath, [&](auto component, Label label) {
          printField(component.Item.str(), label);
        }, Label::always("access_path"));
        return;
      }

      printFieldQuotedRaw([&](raw_ostream &OS) {
        ImportPath::Builder scratch;
        ID->getRealImportPath(scratch).print(OS);
      }, label, IdentifierColor);
    }

  public:
    void visitImportDecl(ImportDecl *ID, Label label) {
      printCommon(ID, "import_decl", label);

      printFlag(ID->isExported(), "exported");
      if (ID->getImportKind() != ImportKind::Module)
        printField(ID->getImportKind(), Label::always("kind"));

      printImportPath(ID, Label::always("module"));
      printFoot();
    }

    void visitUsingDecl(UsingDecl *UD, Label label) {
      printCommon(UD, "using_decl", label);
      printFieldQuoted(UD->getSpecifierName(), Label::always("specifier"));
    }

    void visitExtensionDecl(ExtensionDecl *ED, Label label) {
      printCommon(ED, "extension_decl", label, ExtensionColor);
      printFlag(!ED->hasBeenBound(), "unbound");
      if (ED->hasBeenBound()) {
        printTypeField(ED->getExtendedType(), Label::optional("extended_type"));
      } else {
        printNameRaw([&](raw_ostream &OS) {
          ED->getExtendedTypeRepr()->print(OS);
        }, Label::optional("extended_type"));
      }
      printCommonPost(ED);
    }

    void visitTypeAliasDecl(TypeAliasDecl *TAD, Label label) {
      printCommon(TAD, "typealias", label);

      if (auto underlying = TAD->getCachedUnderlyingType()) {
        printTypeField(underlying, Label::always("type"));
      } else {
        printRec(TAD->getUnderlyingTypeRepr(), Label::always("type_repr"));
      }

      printWhereRequirements(TAD);
      printAttributes(TAD);

      printFoot();
    }

    void visitOpaqueTypeDecl(OpaqueTypeDecl *OTD, Label label) {
      printCommon(OTD, "opaque_type", label);

      printDeclName(OTD->getNamingDecl(), Label::always("naming_decl"));

      if (Writer.isParsable()) {
        printTypeField(OTD->getDeclaredInterfaceType(),
                       Label::always("declared_interface_type"));

        auto genericSig = OTD->getOpaqueInterfaceGenericSignature();
        printList(genericSig.getGenericParams(), [&](auto GP, Label label) {
          printTypeField(GP, label);
        }, Label::always("generic_params"));
        printList(genericSig.getRequirements(), [&](const auto &req, Label label) {
          printRec(req, label);
        }, Label::optional("reqs"));
      } else {
        printFieldQuotedRaw([&](raw_ostream &OS) {
          OS << OTD->getDeclaredInterfaceType() << " in "
            << OTD->getOpaqueInterfaceGenericSignature()->getAsString();
        }, Label::always("opaque_interface"), TypeColor);
      }
      printAttributes(OTD);

      printFoot();
    }

    void visitGenericTypeParamDecl(GenericTypeParamDecl *decl, Label label) {
      printCommon(decl, "generic_type_param", label);
      printField(decl->getDepth(), Label::always("depth"));
      printField(decl->getIndex(), Label::always("index"));

      switch (decl->getParamKind()) {
      case GenericTypeParamKind::Type:
        printField((StringRef)"type", Label::always("param_kind"));
        break;
      case GenericTypeParamKind::Pack:
        printField((StringRef)"pack", Label::always("param_kind"));
        break;
      case GenericTypeParamKind::Value:
        printField((StringRef)"value", Label::always("param_kind"));
        printRec(decl->getValueType(), Label::always("value_type"));
        break;
      }
      printAttributes(decl);

      printFoot();
    }

    void visitAssociatedTypeDecl(AssociatedTypeDecl *decl, Label label) {
      printCommon(decl, "associated_type_decl", label);

      Label fieldName = Label::always("default");
      if (auto defaultDef = decl->getCachedDefaultDefinitionType()) {
        printTypeField(*defaultDef, fieldName);
      } else {
        printField("<not computed>", fieldName);
      }

      printWhereRequirements(decl);
      if (decl->overriddenDeclsComputed()) {
        printStringListField(
            decl->getOverriddenDecls(),
            [&](AssociatedTypeDecl *overridden) {
              if (Writer.isParsable() && isTypeChecked()) {
                return declUSR(overridden->getProtocol());
              }
              return std::string(overridden->getProtocol()->getName().str());
            },
            Label::always("overridden"), /*delimiter=*/", ");
      }

      printAttributes(decl);
      printFoot();
    }

    void visitProtocolDecl(ProtocolDecl *PD, Label label) {
      printCommon(PD, "protocol", label);

      if (PD->isRequirementSignatureComputed()) {
        auto reqSig = PD->getRequirementSignature();

        if (Writer.isParsable()) {
          printRecArbitrary([&](Label label) {
            printHead("requirement_signature", ASTNodeColor, label);
            printList(reqSig.getRequirements(), [&](auto req, Label label) {
              printRec(req, label);
            }, Label::always("requirements"));
            printList(reqSig.getTypeAliases(), [&](auto typeAlias, Label label) {
              printRec(typeAlias, label);
            }, Label::always("typealiases"));
            printFoot();
          }, Label::always("requirement_signature"));
        } else {
          std::string reqSigStr;
          llvm::raw_string_ostream out(reqSigStr);
          reqSig.print(PD, out);

          printFieldQuoted(out.str(), Label::always("requirement_signature"));
        }
      } else {
        printFlag("uncomputed_requirement_signature");
      }

      printCommonPost(PD);
    }

    void printGenericSignature(const GenericSignature &Sig, Label label) {
      if (!Sig)
        return;

      printRecArbitrary([&](Label label) {
        printHead("generic_signature", ASTNodeColor, label);
        printList(Sig.getGenericParams(), [&](auto GP, Label label) {
          printTypeField(GP, label);
        }, Label::always("generic_params"));
        printGenericRequirements(Sig.getRequirements());
        printFoot();
      }, label);
    }

    void printParsedGenericParams(GenericParamList *Params) {
      if (!Params)
        return;

      if (Writer.isParsable()) {
        printList(*Params, [&](auto GP, Label label) {
          printRec(const_cast<GenericTypeParamDecl *>(GP), label);
        }, Label::optional("generic_params"));
      } else {
        printFieldQuotedRaw([&](raw_ostream &OS) {
          Params->print(OS);
        }, Label::optional("generic_params"), TypeColor);
      }
    }

    void printGenericRequirements(ArrayRef<Requirement> Reqs) {
      if (Reqs.empty() || !Writer.isParsable())
        return;

      printList(Reqs, [&](auto Req, Label label) {
        printRec(Req, label);
      }, Label::optional("generic_reqs"));
    }

    void printAttributes(const Decl *D) {
      ASTContext *Ctx = &D->getASTContext();
      DeclContext *DC = D->getDeclContext();
      printList(D->getAttrs(), [&](auto *attr, Label label) {
        printRec(attr, Ctx, DC, label);
      }, Label::optional("attrs"));
    }

    void printCommon(ValueDecl *VD, const char *Name, Label label,
                     TerminalColor Color = DeclColor) {
      printCommon((Decl*)VD, Name, label, Color);

      printDeclName(VD, Label::optional("name"));
      if (auto *GC = VD->getAsGenericContext()) {
        if (Writer.isParsable() && GC->hasComputedGenericSignature()) {
          printGenericSignature(GC->getGenericSignature(),
                                Label::optional("generic_signature"));
        } else {
          printParsedGenericParams(GC->getParsedGenericParams());
          printWhereRequirements(GC);
        }
      }

      if (VD->hasInterfaceType()) {
        printTypeField(VD->getInterfaceType(), Label::always("interface_type"),
                       PrintOptions(), InterfaceTypeColor);
      }

      if (VD->hasAccess()) {
        printField(VD->getFormalAccess(), Label::always("access"),
                   AccessLevelColor);
      }

      if (VD->overriddenDeclsComputed()) {
        auto overridden = VD->getOverriddenDecls();
        printStringListField(
            overridden,
            [&](ValueDecl *overridden) {
              if (Writer.isParsable() && isTypeChecked()) {
                return declUSR(overridden);
              }
              std::string value;
              llvm::raw_string_ostream SOS(value);
              overridden->dumpRef(SOS);
              return value;
            },
            Label::always("override"), /*delimiter=*/", ", OverrideColor);
      }

      auto VarD = dyn_cast<VarDecl>(VD);
      const auto &attrs = VD->getAttrs();
      printFlag(attrs.hasAttribute<FinalAttr>() && !(VarD && VarD->isLet()),
                "final");
      printFlag(attrs.hasAttribute<ObjCAttr>(), "@objc");
      printFlag(attrs.hasAttribute<DynamicAttr>(), "dynamic");
      if (!Writer.isParsable()) {
        // This format isn't suitable for parsable formats, and we already print
        // the full attribute as its own record so we can omit this.
        if (auto *attr = attrs.getAttribute<DynamicReplacementAttr>()) {
          printFlagRaw([&](raw_ostream &OS) {
            OS << "@_dynamicReplacement(for: \"";
            OS << attr->getReplacedFunctionName();
            OS << "\")";
          });
        }
      }
      // In some cases, getLifetimeAnnotation() can fail before extension
      // binding. hasResolvedImports() approximates an extension binding check.
      if (VD->getModuleContext()->hasResolvedImports()) {
        auto lifetimeString = getDumpString(VD->getLifetimeAnnotation());
        if (!lifetimeString.empty())
          printFlag(lifetimeString);
      }
    }

    void printCommon(NominalTypeDecl *NTD, const char *Name, Label label,
                     TerminalColor Color = DeclColor) {
      printCommon((ValueDecl *)NTD, Name, label, Color);

      if (NTD->hasInterfaceType())
        printFlag(NTD->isResilient() ? "resilient" : "non_resilient");
    }

    void printCommonPost(const IterableDeclContext *IDC) {
      switch (IDC->getIterableContextKind()) {
      case IterableDeclContextKind::NominalTypeDecl: {
        const auto NTD = cast<NominalTypeDecl>(IDC);
        printInheritance(NTD);
        printWhereRequirements(NTD);
        break;
      }
      case IterableDeclContextKind::ExtensionDecl:
        const auto ED = cast<ExtensionDecl>(IDC);
        printInheritance(ED);
        printWhereRequirements(ED);
        break;
      }

      printAttributes(IDC->getDecl());

      switch (MemberLoading) {
      case ASTDumpMemberLoading::None:
      case ASTDumpMemberLoading::Parsed: {
        auto members = (MemberLoading == ASTDumpMemberLoading::Parsed)
                           ? IDC->getMembers()
                           : IDC->getCurrentMembersWithoutLoading();
        printList(members, [&](Decl *D, Label label) {
          printRec(D, label);
        }, Label::optional("members"));
        break;
      }

      case ASTDumpMemberLoading::TypeChecked:
        // This mode is used for semantic analysis, so we want the full list of
        // members, including macro-generated ones.
        printList(
            IDC->getAllMembers(),
            [&](Decl *D, Label label) { printRec(D, label); },
            Label::optional("members"));
        break;
      }
      printFoot();
    }

    // Prints a mapping from the declared interface types of the opaque types to
    // their equivalent existential type. This loses some information, but it is
    // meant to make it easier to determine which protocols an opaque type
    // conforms to when such a type appears elsewhere in an expression dump,
    // farther away from where the opaque type is declared.
    void printOpaqueTypeMapping(ArrayRef<OpaqueTypeDecl *> opaqueDecls) {
      printRecArbitrary(
          [&](Label label) {
            printHead("opaque_to_existential_mapping", FieldLabelColor, label);
            for (const auto OTD : opaqueDecls) {
              Type interfaceType = OTD->getDeclaredInterfaceType();
              Type existentialType =
                  replaceOpaqueArchetypesWithExistentials(interfaceType);
              printTypeField(existentialType,
                             Label::always(typeUSR(interfaceType)));
            }
            printFoot();
          },
          Label::always("opaque_to_existential_mapping"));
    }

    void visitSourceFile(const SourceFile &SF) {
      Writer.setMainBufferID(SF.getBufferID());

      printHead("source_file", ASTNodeColor, Label::optional(""));
      printNameRaw([&](raw_ostream &OS) {
        OS << SF.getFilename();
      }, Label::optional("filename"));

      if (Writer.isParsable()) {
        // Print additional information about the compiler for parsable formats,
        // so that they can use it to determine their compatibility with the
        // output.
        printRecArbitrary([&](Label label) {
          auto version = version::getSwiftNumericVersion();
          printHead("compiler_version", FieldLabelColor, label);
          printField(version.first, Label::always("major"));
          printField(version.second, Label::always("minor"));
          printFieldQuoted(version::getSwiftFullVersion(
                               version::Version::getCurrentLanguageVersion()),
                           Label::always("full"));
          printFoot();
        }, Label::always("compiler_version"));
      }

      std::vector<ASTNode> items;
      bool shouldPrintImplicit;

      switch (MemberLoading) {
      case ASTDumpMemberLoading::None:
        shouldPrintImplicit = false;
        if (auto cached = SF.getCachedTopLevelItems()) {
          items = *cached;
        }
        break;
      case ASTDumpMemberLoading::Parsed:
        shouldPrintImplicit = false;
        items = SF.getTopLevelItems();
        break;
      case ASTDumpMemberLoading::TypeChecked:
        shouldPrintImplicit = true;
        for (ASTNode item : SF.getTopLevelItems()) {
          items.push_back(item);

          // If the item is a decl, also collect any auxiliary decls associated
          // with it so that we get macro expansions.
          if (auto decl = item.dyn_cast<Decl *>()) {
            decl->visitAuxiliaryDecls(
                [&items](Decl *auxDecl) { items.push_back(auxDecl); });
          }
        }
        break;
      }
      printList(
          items,
          [&](ASTNode item, Label label) {
            if (!shouldPrintImplicit && item.isImplicit())
              return;

            if (auto decl = item.dyn_cast<Decl *>()) {
              printRec(decl, label);
            } else if (auto stmt = item.dyn_cast<Stmt *>()) {
              printRec(stmt, &SF.getASTContext(), label);
            } else {
              auto expr = item.get<Expr *>();
              printRec(expr, label);
            }
          },
          Label::optional("items"));

      if (Writer.isParsable() && isTypeChecked()) {
        SmallVector<OpaqueTypeDecl *, 4> opaqueDecls;
        SF.getOpaqueReturnTypeDecls(opaqueDecls);
        if (!opaqueDecls.empty()) {
          printOpaqueTypeMapping(opaqueDecls);
        }
      }

      printFoot();
    }

    void visitVarDecl(VarDecl *VD, Label label) {
      printCommon(VD, "var_decl", label);

      printFlag(VD->isDistributed(), "distributed", DeclModifierColor);
      printFlag(VD->isLet(), "let", DeclModifierColor);
      printFlag(VD->getAttrs().hasAttribute<LazyAttr>(), "lazy",
                DeclModifierColor);
      printStorageImpl(VD);
      printFlag(VD->isSelfParamCapture(), "self_param_capture",
                DeclModifierColor);
      printFlag(VD->isDebuggerVar(), "debugger_var", DeclModifierColor);
      printFlag(VD->isLazyStorageProperty(), "lazy_storage_property",
                DeclModifierColor);
      printFlag(VD->isTopLevelGlobal(), "top_level_global", DeclModifierColor);
      printFlag(VD->isLazyStorageProperty(), "lazy_storage_property",
                DeclModifierColor);

      printFlag(VD->getAttrs().hasAttribute<KnownToBeLocalAttr>(),
                "known_to_be_local", DeclModifierColor);
      if (auto *nonisolatedAttr =
              VD->getAttrs().getAttribute<NonisolatedAttr>()) {
        if (nonisolatedAttr->isUnsafe()) {
          printFlag(true, "nonisolated(unsafe)", DeclModifierColor);
        } else if (nonisolatedAttr->isNonSending()) {
          printFlag(true, "nonisolated(nonsending)", DeclModifierColor);
        } else {
          printFlag(true, "nonisolated", DeclModifierColor);
        }
      }

      printAttributes(VD);
      printAccessors(VD);

      printFoot();
    }

    void printStorageImpl(AbstractStorageDecl *D) {
      printFlag(D->isStatic(), "static", DeclModifierColor);

      if (D->hasInterfaceType()) {
        auto impl = D->getImplInfo();
        printField(impl.getReadImpl(), Label::always("readImpl"),
                   DeclModifierColor);
        if (!impl.supportsMutation()) {
          printFlag("immutable", DeclModifierColor);
        } else {
          printField(impl.getWriteImpl(), Label::always("writeImpl"),
                     DeclModifierColor);
          printField(impl.getReadWriteImpl(), Label::always("readWriteImpl"),
                     DeclModifierColor);
        }
      }
    }

    void printAccessors(AbstractStorageDecl *D) {
      printList(D->getAllAccessors(), [&](auto accessor, Label label) {
        printRec(accessor, label);
      }, Label::optional("accessors"));
    }

    void visitParamDecl(ParamDecl *PD, Label label) {
      printHead("parameter", ParameterColor, label);

      printDeclName(PD, Label::optional("name"));

      printDeclContext(PD);
      if (!PD->getArgumentName().empty())
        printFieldQuoted(PD->getArgumentName(), Label::always("apiName"),
                         IdentifierColor);
      if (PD->hasInterfaceType()) {
        printTypeField(PD->getInterfaceType(),
                       Label::always("interface_type"), PrintOptions(),
                       InterfaceTypeColor);
      }

      if (auto specifier = PD->getCachedSpecifier()) {
        if (*specifier != ParamDecl::Specifier::Default) {
          printFlag(ParamDecl::getSpecifierSpelling(*specifier));
        }
      }

      if (PD->hasInterfaceType())
        printFlag(PD->isVariadic(), "variadic");
      printFlag(PD->isAutoClosure(), "autoclosure");
      printFlag(PD->getAttrs().hasAttribute<NonEphemeralAttr>(),"nonEphemeral");

      auto lifetimeString =
          getDumpString(PD->getLifetimeAnnotationFromAttributes());
      if (!lifetimeString.empty())
        printFlag(lifetimeString);

      printFlag(PD->isNoImplicitCopy(), "noImplicitCopy");

      if (PD->getDefaultArgumentKind() != DefaultArgumentKind::None) {
        printField(PD->getDefaultArgumentKind(), Label::always("default_arg"));
      }
      if (PD->hasDefaultExpr()) {
        printCaptureInfoField(PD->getCachedDefaultArgumentCaptureInfo(),
                              Label::optional("default_arg_capture_info"));
      }
      
      printFlag(PD->getAttrs().hasAttribute<KnownToBeLocalAttr>(),
                "known_to_be_local", DeclModifierColor);

      printAttributes(PD);

      if (auto init = PD->getStructuralDefaultExpr()) {
        printRec(init, Label::always("expression"));
      }

      printFoot();
    }

    void visitParameterList(ParameterList *PL, Label label) {
      PrintBase::visitParameterList(PL, label, /*ctx=*/nullptr);
    }

    void visitEnumCaseDecl(EnumCaseDecl *ECD, Label label) {
      printCommon(ECD, "enum_case_decl", label);
      printList(ECD->getElements(), [&](EnumElementDecl *D, Label label) {
        printRec(D, label);
      }, Label::optional("elements"));
      printFoot();
    }

    void visitEnumDecl(EnumDecl *ED, Label label) {
      printCommon(ED, "enum_decl", label);
      printCommonPost(ED);
    }

    void visitEnumElementDecl(EnumElementDecl *EED, Label label) {
      printCommon(EED, "enum_element_decl", label);
      if (auto *paramList = EED->getParameterList()) {
        printRec(paramList, Label::optional("params"));
      }
      printAttributes(EED);
      printFoot();
    }

    void visitStructDecl(StructDecl *SD, Label label) {
      printCommon(SD, "struct_decl", label);
      printCommonPost(SD);
    }

    void visitClassDecl(ClassDecl *CD, Label label) {
      printCommon(CD, "class_decl", label);

      printFlag(CD->isExplicitDistributedActor(), "distributed");
      printFlag(CD->isExplicitActor(), "actor");
      printFlag(CD->getAttrs().hasAttribute<StaticInitializeObjCMetadataAttr>(),
                "@_staticInitializeObjCMetadata");

      printCommonPost(CD);
    }

    void visitBuiltinTupleDecl(BuiltinTupleDecl *BTD, Label label) {
      printCommon(BTD, "builtin_tuple_decl", label);
      printCommonPost(BTD);
    }

    void visitPatternBindingDecl(PatternBindingDecl *PBD, Label label) {
      printCommon(PBD, "pattern_binding_decl", label);
      printAttributes(PBD);

      printList(
          range(PBD->getNumPatternEntries()),
          [&](auto idx, Label label) {
            printRecArbitrary(
                [&](Label label) {
                  printHead("pattern_entry", FieldLabelColor, label);

                  if (PBD->getInitContext(idx))
                    printField(PBD->getInitContext(idx),
                               Label::always("init_context"));

                  printRec(PBD->getPattern(idx), Label::optional("pattern"));
                  if (PBD->getOriginalInit(idx)) {
                    printRec(PBD->getOriginalInit(idx),
                             Label::always("original_init"));
                  }
                  if (PBD->getInit(idx)) {
                    printRec(PBD->getInit(idx),
                             Label::always("processed_init"));
                  }

                  printFoot();
                },
                Label::optional("pattern_entry"));
          },
          Label::optional("pattern_entries"));
      printFoot();
    }

    void visitSubscriptDecl(SubscriptDecl *SD, Label label) {
      printCommon(SD, "subscript_decl", label);
      printStorageImpl(SD);
      printAttributes(SD);
      printTypeOrTypeRepr(SD->getCachedElementInterfaceType(),
                          SD->getElementTypeRepr(), Label::always("element"));
      printAccessors(SD);
      printFoot();
    }

    void printCommonAFD(AbstractFunctionDecl *D, const char *Type, Label label) {
      printCommon(D, Type, label, FuncColor);
      if (auto captureInfo = D->getCachedCaptureInfo()) {
        printCaptureInfoField(captureInfo, Label::optional("captures"));
      }

      if (auto *attr = D->getAttrs().getAttribute<NonisolatedAttr>()) {
        printFlag(attr->isUnsafe() ? "nonisolated(unsafe)" : "nonisolated",
                  ExprModifierColor);
      }
      printFlag(D->isDistributed(), "distributed", ExprModifierColor);
      printFlag(D->isDistributedThunk(), "distributed_thunk",ExprModifierColor);
    }

    void printAbstractFunctionDecl(AbstractFunctionDecl *D) {
      printAttributes(D);
      if (auto *P = D->getImplicitSelfDecl()) {
        printRec(P, Label::optional("implicit_self_decl"));
      }
      if (auto FD = dyn_cast<FuncDecl>(D)) {
        printTypeOrTypeRepr(FD->getCachedResultInterfaceType(),
                            FD->getResultTypeRepr(), Label::always("result"));
        if (auto opaque = FD->getCachedOpaqueResultTypeDecl();
            opaque && *opaque != nullptr) {
          printRec(*opaque, Label::always("opaque_result_decl"));
        }
      }

      printTypeOrTypeRepr(D->getCachedThrownInterfaceType(),
                          D->getThrownTypeRepr(), Label::always("thrown_type"));

      printRec(D->getParameters(), Label::optional("params"),
               &D->getASTContext());

      if (auto fac = D->getForeignAsyncConvention()) {
        printRecArbitrary([&](Label label) {
          printHead("foreign_async_convention", ASTNodeColor, label);
          if (auto type = fac->completionHandlerType())
            printFieldQuoted(type, Label::always("completion_handler_type"),
                             TypeColor);
          printField(fac->completionHandlerParamIndex(),
                     Label::always("completion_handler_param"));
          if (auto errorParamIndex = fac->completionHandlerErrorParamIndex())
            printField(*errorParamIndex, Label::always("error_param"));
          printFoot();
        }, Label::optional("foreign_async_convention"));
      }

      if (auto fec = D->getForeignErrorConvention()) {
        printRecArbitrary([&](Label label) {
          printHead("foreign_error_convention", ASTNodeColor, label);
          printField(fec->getKind(), Label::always("kind"));

          bool wantResultType = (
            fec->getKind() == ForeignErrorConvention::ZeroResult ||
            fec->getKind() == ForeignErrorConvention::NonZeroResult);

          printFlag(getDumpString(fec->isErrorOwned()));

          printField(fec->getErrorParameterIndex(), Label::always("param"));
          printFieldQuoted(fec->getErrorParameterType(),
                           Label::always("paramtype"));
          if (wantResultType)
            printFieldQuoted(fec->getResultType(), Label::always("resulttype"));
          printFoot();
        }, Label::optional("foreign_error_convention"));
      }

      auto canParse =
          (MemberLoading != ASTDumpMemberLoading::None) && !D->isBodySkipped();
      if (auto Body = D->getBody(canParse)) {
        printRec(Body, &D->getASTContext(), Label::optional("body"));
      }
    }

    void printCommonFD(FuncDecl *FD, const char *type, Label label) {
      printCommonAFD(FD, type, label);
      printFlag(FD->isStatic(), "static", DeclModifierColor);
    }

    void visitFuncDecl(FuncDecl *FD, Label label) {
      printCommonFD(FD, "func_decl", label);
      printAbstractFunctionDecl(FD);
      printFoot();
    }

    void visitAccessorDecl(AccessorDecl *AD, Label label) {
      printCommonFD(AD, "accessor_decl", label);
      printFlag(getDumpString(AD->getAccessorKind()));
      printDeclName(AD->getStorage(), Label::always("for"));
      printAbstractFunctionDecl(AD);
      printFoot();
    }

    void visitConstructorDecl(ConstructorDecl *CD, Label label) {
      printCommonAFD(CD, "constructor_decl", label);
      printFlag(CD->isRequired(), "required", DeclModifierColor);
      printFlag(getDumpString(CD->getInitKind()), DeclModifierColor);
      if (CD->isFailable())
        printField((CD->isImplicitlyUnwrappedOptional()
                         ? "ImplicitlyUnwrappedOptional"
                         : "Optional"), Label::always("failable"),
                   DeclModifierColor);
      printAbstractFunctionDecl(CD);
      printFoot();
    }

    void visitDestructorDecl(DestructorDecl *DD, Label label) {
      printCommonAFD(DD, "destructor_decl", label);
      printAbstractFunctionDecl(DD);
      printFoot();
    }

    void visitTopLevelCodeDecl(TopLevelCodeDecl *TLCD, Label label) {
      printCommon(TLCD, "top_level_code_decl", label);
      if (TLCD->getBody()) {
        printRec(TLCD->getBody(), &static_cast<Decl *>(TLCD)->getASTContext(),
                 Label::optional("body"));
      }
      printFoot();
    }

    void visitPrecedenceGroupDecl(PrecedenceGroupDecl *PGD, Label label) {
      printCommon(PGD, "precedence_group_decl", label);
      printName(PGD->getName(), Label::optional("name"));
      printField(PGD->getAssociativity(), Label::always("associativity"));
      printField(PGD->isAssignment(), Label::always("assignment"));

      auto printRelationsRec =
          [&](ArrayRef<PrecedenceGroupDecl::Relation> rels, StringRef name) {
        if (rels.empty()) return;
        // The S-expression representation doesn't render cleanly in JSON
        // because it uses "flags" to just write out the names of the groups
        // in order. Impose a little more structure for JSON.
        if (Writer.isParsable()) {
          printList(rels, [&](auto &rel, Label label) {
            printRecArbitrary([&](Label label) {
              printHead(rel.Name.str(), FieldLabelColor, label);
              printFoot();
            }, label);
          }, Label::always(name));
          return;
        }

        printRecArbitrary([&](Label label) {
          printHead(name, FieldLabelColor, label);
          for (auto &rel : rels)
            printFlag(rel.Name.str());
          printFoot();
        }, Label::optional("relations"));
      };
      printRelationsRec(PGD->getHigherThan(), "higherThan");
      printRelationsRec(PGD->getLowerThan(), "lowerThan");

      printFoot();
    }

    void visitInfixOperatorDecl(InfixOperatorDecl *IOD, Label label) {
      printCommon(IOD, "infix_operator_decl", label);
      printName(IOD->getName(), Label::optional("name"));
      if (!IOD->getPrecedenceGroupName().empty())
        printFieldQuoted(IOD->getPrecedenceGroupName(),
                         Label::always("precedence_group_name"));
      printFoot();
    }

    void visitPrefixOperatorDecl(PrefixOperatorDecl *POD, Label label) {
      printCommon(POD, "prefix_operator_decl", label);
      printName(POD->getName(), Label::optional("name"));
      printFoot();
    }

    void visitPostfixOperatorDecl(PostfixOperatorDecl *POD, Label label) {
      printCommon(POD, "postfix_operator_decl", label);
      printName(POD->getName(), Label::optional("name"));
      printFoot();
    }

    void visitModuleDecl(ModuleDecl *MD, Label label) {
      printCommon(MD, "module", label);
      printFlag(MD->isNonSwiftModule(), "non_swift");
      printAttributes(MD);
      printFoot();
    }

    void visitMissingDecl(MissingDecl *missing, Label label) {
      printCommon(missing, "missing_decl", label);
      printFoot();
    }

    void visitMissingMemberDecl(MissingMemberDecl *MMD, Label label) {
      printCommon(MMD, "missing_member_decl ", label);
      printName(MMD->getName(), Label::optional("name"));
      printFoot();
    }

    void visitMacroDecl(MacroDecl *MD, Label label) {
      printCommon(MD, "macro_decl", label);
      printAttributes(MD);
      printRec(MD->getParameterList(), Label::optional("params"),
               &MD->getASTContext());
      printTypeOrTypeRepr(MD->getCachedResultInterfaceType(),
                          MD->getResultTypeRepr(), Label::always("result"));
      printRec(MD->definition, Label::always("definition"));
      printFoot();
    }

    void visitMacroExpansionDecl(MacroExpansionDecl *MED, Label label) {
      printCommon(MED, "macro_expansion_decl", label);
      if (MemberLoading == ASTDumpMemberLoading::TypeChecked) {
        printDeclRefField(MED->getMacroRef(), Label::always("macro"));
      } else {
        printName(MED->getMacroName().getFullName(), Label::optional("name"));
      }
      printRec(MED->getArgs(), Label::optional("args"));
      printFoot();
    }
  };
} // end anonymous namespace

void ParameterList::dump() const {
  dump(llvm::errs(), 0);
  llvm::errs() << '\n';
}

void ParameterList::dump(raw_ostream &OS, unsigned Indent) const {
  DefaultWriter writer(OS, Indent);
  PrintDecl(writer).visitParameterList(const_cast<ParameterList *>(this),
                                       Label::optional(""));
}

void Decl::dump() const {
  dump(llvm::errs(), 0);
}

void Decl::dump(const char *filename) const {
  std::error_code ec;
  llvm::raw_fd_ostream stream(filename, ec, llvm::sys::fs::FA_Read |
                              llvm::sys::fs::FA_Write);
  // In assert builds, we blow up. Otherwise, we just return.
  assert(!ec && "Failed to open file for dumping?!");
  if (ec)
    return;
  dump(stream, 0);
}

void Decl::dump(raw_ostream &OS, unsigned Indent) const {
  DefaultWriter writer(OS, Indent);
  PrintDecl(writer).visit(const_cast<Decl *>(this), Label::optional(""));
  OS << '\n';
}

/// Print the given declaration context (with its parents).
void swift::printContext(raw_ostream &os, DeclContext *dc) {
  if (auto parent = dc->getParent()) {
    printContext(os, parent);
    os << '.';
  }

  switch (dc->getContextKind()) {
  case DeclContextKind::Package:
    printName(os, cast<PackageUnit>(dc)->getName());
    break;

  case DeclContextKind::Module:
    printName(os, cast<ModuleDecl>(dc)->getRealName());
    break;

  case DeclContextKind::FileUnit:
    // FIXME: print the file's basename?
    os << "(file)";
    break;

  case DeclContextKind::SerializedAbstractClosure:
    os << "serialized abstract closure";
    break;

  case DeclContextKind::AbstractClosureExpr: {
    auto *ACE = cast<AbstractClosureExpr>(dc);
    if (isa<ClosureExpr>(ACE)) {
      PrintWithColorRAII(os, DiscriminatorColor)
        << "explicit closure discriminator=";
    }
    if (isa<AutoClosureExpr>(ACE)) {
      PrintWithColorRAII(os, DiscriminatorColor)
        << "autoclosure discriminator=";
    }

    PrintWithColorRAII(os, DiscriminatorColor) << ACE->getRawDiscriminator();
    break;
  }

  case DeclContextKind::GenericTypeDecl:
    printName(os, cast<GenericTypeDecl>(dc)->getName());
    break;

  case DeclContextKind::ExtensionDecl:
    if (auto repr = cast<ExtensionDecl>(dc)->getExtendedTypeRepr()) {
      repr->print(os);
    } else if (cast<ExtensionDecl>(dc)->hasBeenBound()) {
      auto extendedNominal = cast<ExtensionDecl>(dc)->getExtendedNominal();
      printName(os, extendedNominal->getName());
    } else {
      os << "<unbound>";
    }
    os << " extension";
    break;

  case DeclContextKind::Initializer:
    simple_display(os, cast<Initializer>(dc));
    break;

  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::SerializedTopLevelCodeDecl:
    os << "top-level code";
    break;

  case DeclContextKind::AbstractFunctionDecl:
    printName(os, cast<AbstractFunctionDecl>(dc)->getName());
    break;

  case DeclContextKind::SubscriptDecl:
    printName(os, cast<SubscriptDecl>(dc)->getName());
    break;

  case DeclContextKind::EnumElementDecl:
    printName(os, cast<EnumElementDecl>(dc)->getName());
    break;

  case DeclContextKind::MacroDecl:
    printName(os, cast<MacroDecl>(dc)->getName());
    break;
  }
}

std::string ValueDecl::printRef() const {
  std::string result;
  llvm::raw_string_ostream os(result);
  dumpRef(os);
  return os.str();
}

void ValueDecl::dumpRef(raw_ostream &os) const {
  if (!isa<ModuleDecl>(this)) {
    // Print the context.
    printContext(os, getDeclContext());
    os << ".";
    // Print name.
    getName().printPretty(os);
  } else {
    auto moduleName = cast<ModuleDecl>(this)->getRealName();
    os << moduleName;
  }

  if (getAttrs().hasAttribute<KnownToBeLocalAttr>()) {
    os << " known-to-be-local";
  }

  // Print location.
  auto &srcMgr = getASTContext().SourceMgr;
  if (getLoc().isValid()) {
    os << '@';
    getLoc().print(os, srcMgr);
  }
}

void LLVM_ATTRIBUTE_USED ValueDecl::dumpRef() const {
  dumpRef(llvm::errs());
  llvm::errs() << "\n";
}

void SourceFile::dump() const {
  dump(llvm::errs());
}

void SourceFile::dump(llvm::raw_ostream &OS,
                      ASTDumpMemberLoading memberLoading) const {
  DefaultWriter writer(OS, /*indent*/ 0);
  PrintDecl(writer, memberLoading).visitSourceFile(*this);
  llvm::errs() << '\n';
}

void SourceFile::dumpJSON(llvm::raw_ostream &OS,
                          ASTDumpMemberLoading memberLoading) const {
  JSONWriter writer(OS, /*indent*/ 0);
  PrintDecl(writer, memberLoading).visitSourceFile(*this);
}

void Pattern::dump() const {
  dump(llvm::errs());
}

void Pattern::dump(raw_ostream &OS, unsigned Indent) const {
  DefaultWriter writer(OS, Indent);
  PrintPattern(writer).visit(const_cast<Pattern*>(this), Label::optional(""));
  OS << '\n';
}

//===----------------------------------------------------------------------===//
// Printing for Stmt and all subclasses.
//===----------------------------------------------------------------------===//

namespace {
/// PrintStmt - Visitor implementation of Stmt::dump.
class PrintStmt : public StmtVisitor<PrintStmt, void, Label>,
                  public PrintBase {
public:
  using PrintBase::PrintBase;
  const ASTContext *Ctx;

  PrintStmt(
      PrintWriterBase &writer, const ASTContext *ctx,
      ASTDumpMemberLoading memberLoading = ASTDumpMemberLoading::None,
      llvm::function_ref<Type(Expr *)> getTypeOfExpr = defaultGetTypeOfExpr,
      llvm::function_ref<Type(TypeRepr *)> getTypeOfTypeRepr = nullptr,
      llvm::function_ref<Type(KeyPathExpr *E, unsigned index)>
          getTypeOfKeyPathComponent = defaultGetTypeOfKeyPathComponent)
      : PrintBase(writer, memberLoading, getTypeOfExpr, getTypeOfTypeRepr,
                  getTypeOfKeyPathComponent),
        Ctx(ctx) {}

  using PrintBase::printRec;

  void printRec(Stmt *S, Label label) {
    PrintBase::printRec(S, Ctx, label);
  }

  void printCommon(Stmt *S, const char *Name, Label label) {
    printHead(Name, StmtColor, label);

    printFlag(S->isImplicit(), "implicit");
    printSourceRange(S->getSourceRange(), Ctx);
    printFlag(S->TrailingSemiLoc.isValid(), "trailing_semi");
  }

  void visitBraceStmt(BraceStmt *S, Label label) {
    printCommon(S, "brace_stmt", label);
    printList(S->getElements(), [&](auto &Elt, Label label) {
      printRec(Elt, Ctx, label);
    }, Label::optional("elements"));
    printFoot();
  }

  void visitReturnStmt(ReturnStmt *S, Label label) {
    printCommon(S, "return_stmt", label);
    if (S->hasResult()) {
      printRec(S->getResult(), Label::optional("result"));
    }
    printFoot();
  }

  void visitYieldStmt(YieldStmt *S, Label label) {
    printCommon(S, "yield_stmt", label);
    printList(S->getYields(), [&](auto yield, Label label) {
      printRec(yield, label);
    }, Label::optional("yields"));
    printFoot();
  }

  void visitThenStmt(ThenStmt *S, Label label) {
    printCommon(S, "then_stmt", label);
    printRec(S->getResult(), Label::optional("result"));
    printFoot();
  }

  void visitDeferStmt(DeferStmt *S, Label label) {
    printCommon(S, "defer_stmt", label);
    printRec(S->getTempDecl(), Label::optional("temp_decl"));
    printRec(S->getCallExpr(), Label::optional("call_expr"));
    printFoot();
  }

  void visitIfStmt(IfStmt *S, Label label) {
    printCommon(S, "if_stmt", label);
    printRecRange(S->getCond(), Ctx, Label::always("conditions"));
    printRec(S->getThenStmt(), Label::optional("then_stmt"));
    if (S->getElseStmt()) {
      printRec(S->getElseStmt(), Label::optional("else_stmt"));
    }
    printFoot();
  }

  void visitGuardStmt(GuardStmt *S, Label label) {
    printCommon(S, "guard_stmt", label);
    printRecRange(S->getCond(), Ctx, Label::always("conditions"));
    printRec(S->getBody(), Label::optional("body"));
    printFoot();
  }

  void visitDoStmt(DoStmt *S, Label label) {
    printCommon(S, "do_stmt", label);
    printRec(S->getBody(), Label::optional("body"));
    printFoot();
  }

  void visitWhileStmt(WhileStmt *S, Label label) {
    printCommon(S, "while_stmt", label);
    printRecRange(S->getCond(), Ctx, Label::always("conditions"));
    printRec(S->getBody(), Label::optional("body"));
    printFoot();
  }

  void visitRepeatWhileStmt(RepeatWhileStmt *S, Label label) {
    printCommon(S, "repeat_while_stmt", label);
    printRec(S->getBody(), Label::optional("body"));
    printRec(S->getCond(), Label::optional("cond"));
    printFoot();
  }
  void visitForEachStmt(ForEachStmt *S, Label label) {
    printCommon(S, "for_each_stmt", label);
    printRec(S->getPattern(), Label::optional("pattern"));
    if (S->getWhere()) {
      printRec(S->getWhere(), Label::always("where"));
    }
    printRec(S->getParsedSequence(), Label::optional("parsed_sequence"));
    if (S->getIteratorVar()) {
      printRec(S->getIteratorVar(), Label::optional("iterator_var"));
    }
    if (S->getNextCall()) {
      printRec(S->getNextCall(), Label::optional("next_call"));
    }
    if (S->getConvertElementExpr()) {
      printRec(S->getConvertElementExpr(),
               Label::optional("convert_element_expr"));
    }
    if (S->getElementExpr()) {
      printRec(S->getElementExpr(), Label::optional("element_expr"));
    }
    printRec(S->getBody(), Label::optional("body"));
    printFoot();
  }
  void visitBreakStmt(BreakStmt *S, Label label) {
    printCommon(S, "break_stmt", label);
    printDeclContext(S);
    printFoot();
  }
  void visitContinueStmt(ContinueStmt *S, Label label) {
    printCommon(S, "continue_stmt", label);
    printDeclContext(S);
    printFoot();
  }
  void visitFallthroughStmt(FallthroughStmt *S, Label label) {
    printCommon(S, "fallthrough_stmt", label);
    printDeclContext(S);
    printFoot();
  }
  void visitSwitchStmt(SwitchStmt *S, Label label) {
    printCommon(S, "switch_stmt", label);
    printRec(S->getSubjectExpr(), Label::optional("subject_expr"));
    printList(
        S->getCases(), [&](CaseStmt *CS, Label label) { printRec(CS, label); },
        Label::optional("cases"));
    printFoot();
  }
  void visitCaseStmt(CaseStmt *S, Label label) {
    printCommon(S, "case_stmt", label);
    printFlag(S->hasUnknownAttr(), "@unknown");

    if (S->hasCaseBodyVariables()) {
      printRecRange(S->getCaseBodyVariables(),
                    Label::always("case_body_variables"));
    }

    printList(S->getCaseLabelItems(), [&](const auto &LabelItem, Label label) {
      printRecArbitrary([&](Label label) {
        printHead("case_label_item", StmtColor, label);
        printFlag(LabelItem.isDefault(), "default");
        
        if (auto *CasePattern = LabelItem.getPattern()) {
          switch (CasePattern->getOwnership()) {
          case ValueOwnership::Default:
            break;
          case ValueOwnership::Shared:
            printFieldRaw([](llvm::raw_ostream &os) { os << "borrowing"; },
                          Label::always("ownership"));
            break;
          case ValueOwnership::InOut:
            printFieldRaw([](llvm::raw_ostream &os) { os << "mutating"; },
                          Label::always("ownership"));
            break;
          case ValueOwnership::Owned:
            printFieldRaw([](llvm::raw_ostream &os) { os << "consuming"; },
                          Label::always("ownership"));
            break;
          }
          printRec(CasePattern, Label::optional("pattern"));
        }
        if (auto *Guard = LabelItem.getGuardExpr()) {
          printRec(const_cast<Expr *>(Guard), Label::optional("where_expr"));
        }

        printFoot();
      }, label);
    }, Label::optional("case_label_items"));

    printRec(S->getBody(), Label::optional("body"));
    printFoot();
  }
  void visitFailStmt(FailStmt *S, Label label) {
    printCommon(S, "fail_stmt", label);
    printFoot();
  }

  void visitThrowStmt(ThrowStmt *S, Label label) {
    printCommon(S, "throw_stmt", label);
    printRec(S->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }

  void visitDiscardStmt(DiscardStmt *S, Label label) {
    printCommon(S, "discard_stmt", label);
    printRec(S->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }

  void visitPoundAssertStmt(PoundAssertStmt *S, Label label) {
    printCommon(S, "pound_assert", label);
    printFieldQuoted(S->getMessage(), Label::always("message"));
    printRec(S->getCondition(), Label::optional("condition"));
    printFoot();
  }

  void visitDoCatchStmt(DoCatchStmt *S, Label label) {
    printCommon(S, "do_catch_stmt", label);
    printDeclContext(S);
    printThrowDest(S->rethrows(), /*wantNothrow=*/true);
    printRec(S->getBody(), Label::always("body"));
    printRecRange(S->getCatches(), Ctx, Label::always("catch_stmts"));
    printFoot();
  }
};

} // end anonymous namespace

void Stmt::dump() const {
  dump(llvm::errs());
  llvm::errs() << '\n';
}

void Stmt::dump(raw_ostream &OS, const ASTContext *Ctx, unsigned Indent) const {
  DefaultWriter writer(OS, Indent);
  PrintStmt(writer, Ctx).visit(const_cast<Stmt*>(this), Label::optional(""));
}

//===----------------------------------------------------------------------===//
// Printing for Expr and all subclasses.
//===----------------------------------------------------------------------===//

namespace {
/// PrintExpr - Visitor implementation of Expr::dump.
class PrintExpr : public ExprVisitor<PrintExpr, void, Label>,
                  public PrintBase {
public:
  using PrintBase::PrintBase;

  /// FIXME: This should use ExprWalker to print children.

  void printCommon(Expr *E, const char *C, Label label) {
    PrintOptions PO;
    PO.PrintTypesForDebugging = true;

    printHead(C, ExprColor, label);

    printFlag(E->isImplicit(), "implicit", ExprModifierColor);
    printTypeField(GetTypeOfExpr(E), Label::always("type"), PO, TypeColor);

    // If we have a source range and an ASTContext, print the source range.
    if (auto Ty = GetTypeOfExpr(E)) {
      auto &Ctx = Ty->getASTContext();
      printSourceLoc(E->getLoc(), &Ctx);
      printSourceRange(E->getSourceRange(), &Ctx);
    }

    printFlag(E->TrailingSemiLoc.isValid(), "trailing_semi");
  }
  
  void printFunctionRefInfo(const FunctionRefInfo &info, Label label) {
    if (Writer.isParsable()) {
      printRecArbitrary([&](Label label) {
        printHead("function_ref_info", ExprModifierColor, label);
        printFlag(info.isCompoundName(), "is_compound_name");
        printField(info.getApplyLevel(), Label::always("apply_level"));
        printFoot();
      }, label);
    } else {
      printFieldRaw([&](auto &os) { info.dump(os); }, label, ExprModifierColor);
    }
  }

  void visitErrorExpr(ErrorExpr *E, Label label) {
    printCommon(E, "error_expr", label);
    printFoot();
  }

  void visitCodeCompletionExpr(CodeCompletionExpr *E, Label label) {
    printCommon(E, "code_completion_expr", label);
    if (E->getBase()) {
      printRec(E->getBase(), Label::optional("base"));
    }
    printFoot();
  }

  void printInitializerField(ConcreteDeclRef declRef, Label label) {
    printDeclRefField(declRef, label, ExprModifierColor);
  }

  void visitNilLiteralExpr(NilLiteralExpr *E, Label label) {
    printCommon(E, "nil_literal_expr", label);
    printInitializerField(E->getInitializer(), Label::always("initializer"));
    printFoot();
  }

  void visitIntegerLiteralExpr(IntegerLiteralExpr *E, Label label) {
    printCommon(E, "integer_literal_expr", label);
    
    printFlag(E->isNegative(), "negative", LiteralValueColor);
    Type T = GetTypeOfExpr(E);
    if (T.isNull() || !T->is<BuiltinIntegerType>())
      printFieldQuoted(E->getDigitsText(), Label::always("value"),
                       LiteralValueColor);
    else
      printFieldQuoted(E->getValue(), Label::always("value"), LiteralValueColor);
    printInitializerField(E->getBuiltinInitializer(),
                          Label::always("builtin_initializer"));
    printInitializerField(E->getInitializer(), Label::always("initializer"));

    printFoot();
  }
  void visitFloatLiteralExpr(FloatLiteralExpr *E, Label label) {
    printCommon(E, "float_literal_expr", label);
    
    printFlag(E->isNegative(), "negative", LiteralValueColor);
    printFieldQuoted(E->getDigitsText(), Label::always("value"),
                     LiteralValueColor);
    printInitializerField(E->getBuiltinInitializer(),
                          Label::always("builtin_initializer"));
    printInitializerField(E->getInitializer(), Label::always("initializer"));
    if (!E->getBuiltinType().isNull()) {
      printFieldQuoted(E->getBuiltinType(), Label::always("builtin_type"),
                       ExprModifierColor);
    }
    
    printFoot();
  }

  void visitBooleanLiteralExpr(BooleanLiteralExpr *E, Label label) {
    printCommon(E, "boolean_literal_expr", label);
    
    printField(E->getValue(), Label::always("value"), LiteralValueColor);
    printInitializerField(E->getBuiltinInitializer(),
                          Label::always("builtin_initializer"));
    printInitializerField(E->getInitializer(), Label::always("initializer"));

    printFoot();
  }

  void visitStringLiteralExpr(StringLiteralExpr *E, Label label) {
    printCommon(E, "string_literal_expr", label);
    
    printField(E->getEncoding(), Label::always("encoding"), ExprModifierColor);
    printFieldQuoted(E->getValue(), Label::always("value"), LiteralValueColor);
    printInitializerField(E->getBuiltinInitializer(),
                          Label::always("builtin_initializer"));
    printInitializerField(E->getInitializer(), Label::always("initializer"));

    printFoot();
  }
  void visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *E, Label label) {
    printCommon(E, "interpolated_string_literal_expr", label);

    printField(E->getLiteralCapacity(), Label::always("literal_capacity"),
               ExprModifierColor);
    printField(E->getInterpolationCount(), Label::always("interpolation_count"),
               ExprModifierColor);
    printInitializerField(E->getBuilderInit(), Label::always("builder_init"));
    printInitializerField(E->getInitializer(), Label::always("result_init"));

    printRec(E->getAppendingExpr(), Label::optional("appending_expr"));

    printFoot();
  }
  void visitMagicIdentifierLiteralExpr(MagicIdentifierLiteralExpr *E, Label label) {
    printCommon(E, "magic_identifier_literal_expr", label);
    
    printField(E->getKind(), Label::always("kind"), ExprModifierColor);

    if (E->isString()) {
      printField(E->getStringEncoding(), Label::always("encoding"),
                 ExprModifierColor);
    }
    printInitializerField(E->getBuiltinInitializer(),
                          Label::always("builtin_initializer"));
    printInitializerField(E->getInitializer(), Label::always("initializer"));

    printFoot();
  }
  void visitRegexLiteralExpr(RegexLiteralExpr *E, Label label) {
    printCommon(E, "regex_literal_expr", label);

    printFieldQuoted(E->getParsedRegexText(), Label::always("text"),
                     LiteralValueColor);
    printInitializerField(E->getInitializer(), Label::always("initializer"));

    printFoot();
  }

  void visitObjectLiteralExpr(ObjectLiteralExpr *E, Label label) {
    printCommon(E, "object_literal", label);

    printField(E->getLiteralKind(), Label::always("kind"));
    printInitializerField(E->getInitializer(), Label::always("initializer"));

    printRec(E->getArgs(), Label::optional("args"));
    
    printFoot();
  }

  void visitDiscardAssignmentExpr(DiscardAssignmentExpr *E, Label label) {
    printCommon(E, "discard_assignment_expr", label);
    printFoot();
  }

  void visitDeclRefExpr(DeclRefExpr *E, Label label) {
    printCommon(E, "declref_expr", label);
    printThrowDest(E->throws(), /*wantNothrow=*/false);

    printDeclRefField(E->getDeclRef(), Label::always("decl"));
    if (E->getAccessSemantics() != AccessSemantics::Ordinary)
      printFlag(getDumpString(E->getAccessSemantics()), AccessLevelColor);
    printFunctionRefInfo(E->getFunctionRefInfo(), Label::always("function_ref"));

    printFoot();
  }
  void visitSuperRefExpr(SuperRefExpr *E, Label label) {
    printCommon(E, "super_ref_expr", label);
    printFoot();
  }

  void visitTypeExpr(TypeExpr *E, Label label) {
    printCommon(E, "type_expr", label);

    // For parsable outputs, the type is already printed by `printCommon`.
    if (!Writer.isParsable()) {
      if (E->getTypeRepr())
        printFieldQuotedRaw([&](raw_ostream &OS) { E->getTypeRepr()->print(OS); },
                            Label::always("typerepr"), TypeReprColor);
      else
        printFlag("null_typerepr");
    }

    printFoot();
  }

  void visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *E, Label label) {
    printCommon(E, "other_constructor_ref_expr", label);
    printDeclRefField(E->getDeclRef(), Label::always("decl"));
    printFoot();
  }
  void visitOverloadedDeclRefExpr(OverloadedDeclRefExpr *E, Label label) {
    printCommon(E, "overloaded_decl_ref_expr", label);

    printFieldQuoted(E->getDecls()[0]->getBaseName(), Label::always("name"),
                     IdentifierColor);
    printField(E->getDecls().size(), Label::always("number_of_decls"),
               ExprModifierColor);
    printFunctionRefInfo(E->getFunctionRefInfo(), Label::always("function_ref"));

    if (!E->isForOperator()) {
      printList(E->getDecls(), [&](auto D, Label label) {
        printRecArbitrary([&](Label label) {
          printHead("candidate_decl", DeclModifierColor, label);
          printNameRaw([&](raw_ostream &OS) { D->dumpRef(OS); },
                       Label::optional("name"));
          printFoot();
        }, Label::optional("decl"));
      }, Label::optional("candidates"));
    }

    printFoot();
  }
  void visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *E, Label label) {
    printCommon(E, "unresolved_decl_ref_expr", label);

    printFieldQuoted(E->getName(), Label::always("name"), IdentifierColor);
    printFunctionRefInfo(E->getFunctionRefInfo(), Label::always("function_ref"));

    printFoot();
  }
  void visitUnresolvedSpecializeExpr(UnresolvedSpecializeExpr *E, Label label) {
    printCommon(E, "unresolved_specialize_expr", label);

    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printList(E->getUnresolvedParams(), [&](TypeLoc T, Label label) {
      printRec(T.getTypeRepr(), label);
    }, Label::optional("unresolved_params"));

    printFoot();
  }

  void visitMemberRefExpr(MemberRefExpr *E, Label label) {
    printCommon(E, "member_ref_expr", label);
    printThrowDest(E->throws(), /*wantNothrow=*/false);

    printDeclRefField(E->getMember(), Label::always("decl"));
    if (E->getAccessSemantics() != AccessSemantics::Ordinary)
      printFlag(getDumpString(E->getAccessSemantics()), AccessLevelColor);
    printFlag(E->isSuper(), "super");

    printRec(E->getBase(), Label::optional("base"));
    printFoot();
  }
  void visitDynamicMemberRefExpr(DynamicMemberRefExpr *E, Label label) {
    printCommon(E, "dynamic_member_ref_expr", label);
    printThrowDest(E->throws(), /*wantNothrow=*/false);

    printDeclRefField(E->getMember(), Label::always("decl"));

    printRec(E->getBase(), Label::optional("base"));

    printFoot();
  }
  void visitUnresolvedMemberExpr(UnresolvedMemberExpr *E, Label label) {
    printCommon(E, "unresolved_member_expr", label);

    printFieldQuoted(E->getName(), Label::always("name"), ExprModifierColor);
    printFunctionRefInfo(E->getFunctionRefInfo(), Label::always("function_ref"));
    printFoot();
  }
  void visitDotSelfExpr(DotSelfExpr *E, Label label) {
    printCommon(E, "dot_self_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitParenExpr(ParenExpr *E, Label label) {
    printCommon(E, "paren_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitAwaitExpr(AwaitExpr *E, Label label) {
    printCommon(E, "await_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitUnsafeExpr(UnsafeExpr *E, Label label) {
    printCommon(E, "unsafe_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitConsumeExpr(ConsumeExpr *E, Label label) {
    printCommon(E, "consume_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitCopyExpr(CopyExpr *E, Label label) {
    printCommon(E, "copy_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitBorrowExpr(BorrowExpr *E, Label label) {
    printCommon(E, "borrow_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitUnresolvedMemberChainResultExpr(UnresolvedMemberChainResultExpr *E, Label label){
    printCommon(E, "unresolved_member_chain_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitTupleExpr(TupleExpr *E, Label label) {
    printCommon(E, "tuple_expr", label);

    if (E->hasElementNames()) {
      printStringListField(E->getElementNames(), [&](Identifier name) {
        return name.str();
      }, Label::optional("names"));
    }

    printList(range(E->getNumElements()), [&](unsigned i, Label label) {
      if (E->getElement(i))
        printRec(E->getElement(i), label);
      else {
        printRecArbitrary([&](Label label) {
          printHead("<tuple element default value>", ExprColor, label);
          printFoot();
        }, label);
      }
    }, Label::optional("elements"));

    printFoot();
  }
  void visitArrayExpr(ArrayExpr *E, Label label) {
    printCommon(E, "array_expr", label);

    printInitializerField(E->getInitializer(), Label::always("initializer"));

    printList(E->getElements(), [&](auto elt, Label label) {
      printRec(elt, label);
    }, Label::optional("elements"));

    printFoot();
  }
  void visitDictionaryExpr(DictionaryExpr *E, Label label) {
    printCommon(E, "dictionary_expr", label);

    printInitializerField(E->getInitializer(), Label::always("initializer"));

    printList(E->getElements(), [&](auto elt, Label label) {
      printRec(elt, label);
    }, Label::optional("elements"));

    printFoot();
  }
  void visitSubscriptExpr(SubscriptExpr *E, Label label) {
    printCommon(E, "subscript_expr", label);
    printThrowDest(E->throws(), /*wantNothrow=*/false);

    if (E->getAccessSemantics() != AccessSemantics::Ordinary)
      printFlag(getDumpString(E->getAccessSemantics()), AccessLevelColor);
    printFlag(E->isSuper(), "super");
    if (E->hasDecl()) {
      printDeclRefField(E->getDecl(), Label::always("decl"));
    }

    printRec(E->getBase(), Label::optional("base"));
    printRec(E->getArgs(), Label::optional("args"));

    printFoot();
  }
  void visitKeyPathApplicationExpr(KeyPathApplicationExpr *E, Label label) {
    printCommon(E, "keypath_application_expr", label);
    printRec(E->getBase(), Label::optional("base"));
    printRec(E->getKeyPath(), Label::optional("keypath"));
    printFoot();
  }
  void visitDynamicSubscriptExpr(DynamicSubscriptExpr *E, Label label) {
    printCommon(E, "dynamic_subscript_expr", label);
    printThrowDest(E->throws(), /*wantNothrow=*/false);

    printDeclRefField(E->getMember(), Label::always("decl"));

    printRec(E->getBase(), Label::optional("base"));
    printRec(E->getArgs(), Label::optional("args"));

    printFoot();
  }
  void visitUnresolvedDotExpr(UnresolvedDotExpr *E, Label label) {
    printCommon(E, "unresolved_dot_expr", label);

    printFieldQuoted(E->getName(), Label::always("field"));
    printFunctionRefInfo(E->getFunctionRefInfo(), Label::always("function_ref"));

    if (E->getBase()) {
      printRec(E->getBase(), Label::optional("base"));
    }

    printFoot();
  }
  void visitTupleElementExpr(TupleElementExpr *E, Label label) {
    printCommon(E, "tuple_element_expr", label);

    printField(E->getFieldNumber(), Label::always("field #"));

    printRec(E->getBase(), Label::optional("base"));

    printFoot();
  }
  void visitDestructureTupleExpr(DestructureTupleExpr *E, Label label) {
    printCommon(E, "destructure_tuple_expr", label);

    printRecRange(E->getDestructuredElements(), Label::always("destructured"));
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printRec(E->getResultExpr(), Label::optional("result_expr"));

    printFoot();
  }
  void visitUnresolvedTypeConversionExpr(UnresolvedTypeConversionExpr *E,
                                         Label label) {
    printCommon(E, "unresolvedtype_conversion_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitFunctionConversionExpr(FunctionConversionExpr *E, Label label) {
    printCommon(E, "function_conversion_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitCovariantFunctionConversionExpr(CovariantFunctionConversionExpr *E,
                                            Label label){
    printCommon(E, "covariant_function_conversion_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitCovariantReturnConversionExpr(CovariantReturnConversionExpr *E,
                                          Label label){
    printCommon(E, "covariant_return_conversion_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitUnderlyingToOpaqueExpr(UnderlyingToOpaqueExpr *E, Label label){
    printCommon(E, "underlying_to_opaque_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitErasureExpr(ErasureExpr *E, Label label) {
    printCommon(E, "erasure_expr", label);
    printList(E->getConformances(), [&](auto conf, Label label) {
      printRec(conf, label);
    }, Label::optional("conformances"));
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitAnyHashableErasureExpr(AnyHashableErasureExpr *E, Label label) {
    printCommon(E, "any_hashable_erasure_expr", label);
    printRec(E->getConformance(), Label::optional("conformance"));
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitConditionalBridgeFromObjCExpr(ConditionalBridgeFromObjCExpr *E,
                                          Label label) {
    printCommon(E, "conditional_bridge_from_objc_expr", label);

    printDeclRefField(E->getConversion(), Label::always("conversion"));

    printRec(E->getSubExpr(), Label::optional("sub_expr"));

    printFoot();
  }
  void visitBridgeFromObjCExpr(BridgeFromObjCExpr *E, Label label) {
    printCommon(E, "bridge_from_objc_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitBridgeToObjCExpr(BridgeToObjCExpr *E, Label label) {
    printCommon(E, "bridge_to_objc_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitLoadExpr(LoadExpr *E, Label label) {
    printCommon(E, "load_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitABISafeConversionExpr(ABISafeConversionExpr *E, Label label) {
    printCommon(E, "abi_safe_conversion_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitMetatypeConversionExpr(MetatypeConversionExpr *E, Label label) {
    printCommon(E, "metatype_conversion_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitCollectionUpcastConversionExpr(CollectionUpcastConversionExpr *E,
                                           Label label) {
    printCommon(E, "collection_upcast_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    if (auto keyConversion = E->getKeyConversion()) {
      printRec(keyConversion.Conversion, Label::always("key_conversion"));
    }
    if (auto valueConversion = E->getValueConversion()) {
      printRec(valueConversion.Conversion, Label::always("value_conversion"));
    }
    printFoot();
  }
  void visitDerivedToBaseExpr(DerivedToBaseExpr *E, Label label) {
    printCommon(E, "derived_to_base_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitArchetypeToSuperExpr(ArchetypeToSuperExpr *E, Label label) {
    printCommon(E, "archetype_to_super_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitInjectIntoOptionalExpr(InjectIntoOptionalExpr *E, Label label) {
    printCommon(E, "inject_into_optional", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitClassMetatypeToObjectExpr(ClassMetatypeToObjectExpr *E,
                                      Label label) {
    printCommon(E, "class_metatype_to_object", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitExistentialMetatypeToObjectExpr(ExistentialMetatypeToObjectExpr *E,
                                            Label label) {
    printCommon(E, "existential_metatype_to_object", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitProtocolMetatypeToObjectExpr(ProtocolMetatypeToObjectExpr *E,
                                         Label label) {
    printCommon(E, "protocol_metatype_to_object", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitInOutToPointerExpr(InOutToPointerExpr *E, Label label) {
    printCommon(E, "inout_to_pointer", label);
    printFlag(E->isNonAccessing(), "nonaccessing");
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitArrayToPointerExpr(ArrayToPointerExpr *E, Label label) {
    printCommon(E, "array_to_pointer", label);
    printFlag(E->isNonAccessing(), "nonaccessing");
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitStringToPointerExpr(StringToPointerExpr *E, Label label) {
    printCommon(E, "string_to_pointer", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitPointerToPointerExpr(PointerToPointerExpr *E, Label label) {
    printCommon(E, "pointer_to_pointer", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitForeignObjectConversionExpr(ForeignObjectConversionExpr *E,
                                        Label label) {
    printCommon(E, "foreign_object_conversion", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitUnevaluatedInstanceExpr(UnevaluatedInstanceExpr *E, Label label) {
    printCommon(E, "unevaluated_instance", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitUnreachableExpr(UnreachableExpr *E, Label label) {
    printCommon(E, "unreachable", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitDifferentiableFunctionExpr(DifferentiableFunctionExpr *E,
                                       Label label) {
    printCommon(E, "differentiable_function", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitLinearFunctionExpr(LinearFunctionExpr *E, Label label) {
    printCommon(E, "linear_function", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitDifferentiableFunctionExtractOriginalExpr(
      DifferentiableFunctionExtractOriginalExpr *E, Label label) {
    printCommon(E, "differentiable_function_extract_original", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitLinearFunctionExtractOriginalExpr(
      LinearFunctionExtractOriginalExpr *E, Label label) {
    printCommon(E, "linear_function_extract_original", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitLinearToDifferentiableFunctionExpr(
      LinearToDifferentiableFunctionExpr *E, Label label) {
    printCommon(E, "linear_to_differentiable_function", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }

  void visitActorIsolationErasureExpr(ActorIsolationErasureExpr *E,
                                      Label label) {
    printCommon(E, "actor_isolation_erasure_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }

  void visitUnsafeCastExpr(UnsafeCastExpr *E, Label label) {
    printCommon(E, "unsafe_cast_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }

  void visitExtractFunctionIsolationExpr(ExtractFunctionIsolationExpr *E,
                                         Label label) {
    printCommon(E, "extract_function_isolation", label);
    printRec(E->getFunctionExpr(), Label::optional("function_expr"));
    printFoot();
  }

  void visitInOutExpr(InOutExpr *E, Label label) {
    printCommon(E, "inout_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }

  void visitVarargExpansionExpr(VarargExpansionExpr *E, Label label) {
    printCommon(E, "vararg_expansion_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }

  void visitPackExpansionExpr(PackExpansionExpr *E, Label label) {
    printCommon(E, "pack_expansion_expr", label);
    printRec(E->getPatternExpr(), Label::optional("pattern_expr"));
    printFoot();
  }

  void visitPackElementExpr(PackElementExpr *E, Label label) {
    printCommon(E, "pack_element_expr", label);
    printRec(E->getPackRefExpr(), Label::optional("pack_ref_expr"));
    printFoot();
  }

  void visitMaterializePackExpr(MaterializePackExpr *E, Label label) {
    printCommon(E, "materialize_pack_expr", label);
    printRec(E->getFromExpr(), Label::optional("from_expr"));
    printFoot();
  }

  void visitForceTryExpr(ForceTryExpr *E, Label label) {
    printCommon(E, "force_try_expr", label);

    PrintOptions PO;
    PO.PrintTypesForDebugging = true;
    printTypeField(E->getThrownError(), Label::always("thrown_error"), PO,
                   TypeColor);

    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }

  void visitOptionalTryExpr(OptionalTryExpr *E, Label label) {
    printCommon(E, "optional_try_expr", label);

    PrintOptions PO;
    PO.PrintTypesForDebugging = true;
    printTypeField(E->getThrownError(), Label::always("thrown_error"), PO,
                   TypeColor);

    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }

  void visitTryExpr(TryExpr *E, Label label) {
    printCommon(E, "try_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }

  void visitSequenceExpr(SequenceExpr *E, Label label) {
    printCommon(E, "sequence_expr", label);
    printList(range(E->getNumElements()), [&](unsigned i, Label label) {
      printRec(E->getElement(i), label);
    }, Label::optional("elements"));
    printFoot();
  }

  void visitCaptureListExpr(CaptureListExpr *E, Label label) {
    printCommon(E, "capture_list", label);
    printList(E->getCaptureList(), [&](auto capture, Label label) {
      printRec(capture.PBD, label);
    }, Label::optional("captures"));
    printRec(E->getClosureBody(), Label::optional("closure_body"));
    printFoot();
  }

  void printClosure(AbstractClosureExpr *E, char const *name, Label label) {
    printCommon(E, name, label);

    // If we're dumping the type-checked AST, compute the discriminator if
    // needed. Otherwise, print the cached discriminator.
    auto discriminator = isTypeChecked() ? E->getDiscriminator()
                                         : E->getRawDiscriminator();

    printField(discriminator, Label::always("discriminator"),
               DiscriminatorColor);
    printIsolation(E->getActorIsolation());

    if (auto captureInfo = E->getCachedCaptureInfo()) {
      printCaptureInfoField(captureInfo, Label::optional("captures"));
    }
    // Printing a function type doesn't indicate whether it's escaping because it doesn't 
    // matter in 99% of contexts. AbstractClosureExpr nodes are one of the only exceptions.
    if (auto Ty = GetTypeOfExpr(E)) {
      if (auto fType = Ty->getAs<AnyFunctionType>()) {
        printFlag(!fType->getExtInfo().isNoEscape(), "escaping",
                  ClosureModifierColor);
        printFlag(fType->getExtInfo().isSendable(), "sendable",
                  ClosureModifierColor);
      }
    }
  }

  void visitClosureExpr(ClosureExpr *E, Label label) {
    printClosure(E, "closure_expr", label);
    printFlag(E->hasSingleExpressionBody(), "single_expression",
              ClosureModifierColor);
    printFlag(E->allowsImplicitSelfCapture(), "implicit_self",
              ClosureModifierColor);
    printFlag(E->inheritsActorContext(), "inherits_actor_context",
              ClosureModifierColor);

    if (E->getParameters()) {
      printRec(E->getParameters(), Label::optional("params"),
               &E->getASTContext());
    }
    printRec(E->getBody(), &E->getASTContext(), Label::optional("body"));

    printFoot();
  }
  void visitAutoClosureExpr(AutoClosureExpr *E, Label label) {
    printClosure(E, "autoclosure_expr", label);

    if (E->getParameters()) {
      printRec(E->getParameters(), Label::optional("params"),
               &E->getASTContext());
    }

    printRec(E->getSingleExpressionBody(), Label::optional("single_expr_body"));
    printFoot();
  }

  void visitDynamicTypeExpr(DynamicTypeExpr *E, Label label) {
    printCommon(E, "metatype_expr", label);
    printRec(E->getBase(), Label::optional("base"));
    printFoot();
  }

  void visitOpaqueValueExpr(OpaqueValueExpr *E, Label label) {
    printCommon(E, "opaque_value_expr", label);
    printNameRaw([&](raw_ostream &OS) { OS << (void*)E; },
                 Label::optional("identity"));
    printFoot();
  }

  void visitPropertyWrapperValuePlaceholderExpr(
      PropertyWrapperValuePlaceholderExpr *E, Label label) {
    printCommon(E, "property_wrapper_value_placeholder_expr", label);
    printRec(E->getOpaqueValuePlaceholder(),
             Label::optional("opaque_value_placeholder"));
    if (auto *value = E->getOriginalWrappedValue()) {
      printRec(value, Label::optional("original_wrapped_value"));
    }
    printFoot();
  }

  void visitAppliedPropertyWrapperExpr(AppliedPropertyWrapperExpr *E,
                                       Label label) {
    printCommon(E, "applied_property_wrapper_expr", label);
    printRec(E->getValue(), Label::optional("value"));
    printFoot();
  }

  void visitDefaultArgumentExpr(DefaultArgumentExpr *E, Label label) {
    printCommon(E, "default_argument_expr", label);
    printDeclRefField(E->getDefaultArgsOwner(),
                      Label::always("default_args_owner"));
    printField(E->getParamIndex(), Label::always("param"));
    printFoot();
  }

  void printApplyExpr(ApplyExpr *E, const char *NodeName, Label label) {
    printCommon(E, NodeName, label);
    if (E->isThrowsSet()) {
      printThrowDest(E->throws(), /*wantNothrow=*/true);
    }
    printFieldQuotedRaw([&](raw_ostream &OS) {
      auto isolationCrossing = E->getIsolationCrossing();
      if (isolationCrossing.has_value()) {
        OS << "{caller='";
        simple_display(OS, isolationCrossing.value().getCallerIsolation());
        OS << "', callee='";
        simple_display(OS, isolationCrossing.value().getCalleeIsolation());
        OS << "'}";
      } else {
        OS << "none";
      }
    }, Label::always("isolation_crossing"), ExprModifierColor);

    printRec(E->getFn(), Label::optional("fn"));
    printRec(E->getArgs(), Label::optional("args"));

    printFoot();
  }

  void visitCallExpr(CallExpr *E, Label label) {
    printApplyExpr(E, "call_expr", label);
  }
  void visitPrefixUnaryExpr(PrefixUnaryExpr *E, Label label) {
    printApplyExpr(E, "prefix_unary_expr", label);
  }
  void visitPostfixUnaryExpr(PostfixUnaryExpr *E, Label label) {
    printApplyExpr(E, "postfix_unary_expr", label);
  }
  void visitBinaryExpr(BinaryExpr *E, Label label) {
    printApplyExpr(E, "binary_expr", label);
  }
  void visitDotSyntaxCallExpr(DotSyntaxCallExpr *E, Label label) {
    printApplyExpr(E, "dot_syntax_call_expr", label);
  }
  void visitConstructorRefCallExpr(ConstructorRefCallExpr *E, Label label) {
    printApplyExpr(E, "constructor_ref_call_expr", label);
  }
  void visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E, Label label) {
    printCommon(E, "dot_syntax_base_ignored", label);
    printRec(E->getLHS(), Label::optional("lhs"));
    printRec(E->getRHS(), Label::optional("rhs"));
    printFoot();
  }

  void printExplicitCastExpr(ExplicitCastExpr *E, const char *name,
                             Label label) {
    printCommon(E, name, label);

    if (auto checkedCast = dyn_cast<CheckedCastExpr>(E))
      printFlag(getDumpString(checkedCast->getCastKind()));
    if (GetTypeOfTypeRepr)
      printTypeField(GetTypeOfTypeRepr(E->getCastTypeRepr()),
                     Label::always("written_type"));
    else
      printTypeField(E->getCastType(), Label::always("written_type"));

    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitForcedCheckedCastExpr(ForcedCheckedCastExpr *E, Label label) {
    printExplicitCastExpr(E, "forced_checked_cast_expr", label);
  }
  void visitConditionalCheckedCastExpr(ConditionalCheckedCastExpr *E,
                                       Label label) {
    printExplicitCastExpr(E, "conditional_checked_cast_expr", label);
  }
  void visitIsExpr(IsExpr *E, Label label) {
    printExplicitCastExpr(E, "is_subtype_expr", label);
  }
  void visitCoerceExpr(CoerceExpr *E, Label label) {
    printExplicitCastExpr(E, "coerce_expr", label);
  }
  void visitArrowExpr(ArrowExpr *E, Label label) {
    printCommon(E, "arrow", label);

    printFlag(E->getAsyncLoc().isValid(), "async");
    printFlag(E->getThrowsLoc().isValid(), "throws");

    printRec(E->getArgsExpr(), Label::optional("args"));
    printRec(E->getResultExpr(), Label::optional("result_expr"));

    printFoot();
  }
  void visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *E,
                                        Label label) {
    printCommon(E, "rebind_self_in_constructor_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitTernaryExpr(TernaryExpr *E, Label label) {
    printCommon(E, "ternary_expr", label);
    printRec(E->getCondExpr(), Label::optional("cond_expr"));
    printRec(E->getThenExpr(), Label::optional("then_expr"));
    printRec(E->getElseExpr(), Label::optional("else_expr"));
    printFoot();
  }
  void visitAssignExpr(AssignExpr *E, Label label) {
    printCommon(E, "assign_expr", label);
    printRec(E->getDest(), Label::optional("dest"));
    printRec(E->getSrc(), Label::optional("src"));
    printFoot();
  }
  void visitEnumIsCaseExpr(EnumIsCaseExpr *E, Label label) {
    printCommon(E, "enum_is_case_expr", label);
    printName(E->getEnumElement()->getBaseIdentifier(), Label::optional("name"));
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitUnresolvedPatternExpr(UnresolvedPatternExpr *E, Label label) {
    printCommon(E, "unresolved_pattern_expr", label);
    printRec(E->getSubPattern(), Label::optional("sub_pattern"));
    printFoot();
  }
  void visitBindOptionalExpr(BindOptionalExpr *E, Label label) {
    printCommon(E, "bind_optional_expr", label);
    printField(E->getDepth(), Label::always("depth"));
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitOptionalEvaluationExpr(OptionalEvaluationExpr *E, Label label) {
    printCommon(E, "optional_evaluation_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitForceValueExpr(ForceValueExpr *E, Label label) {
    printCommon(E, "force_value_expr", label);

    printFlag(E->isForceOfImplicitlyUnwrappedOptional(), "implicit_iuo_unwrap",
              ExprModifierColor);

    printRec(E->getSubExpr(), Label::optional("sub_expr"));

    printFoot();
  }
  void visitOpenExistentialExpr(OpenExistentialExpr *E, Label label) {
    printCommon(E, "open_existential_expr", label);
    printRec(E->getOpaqueValue(), Label::optional("opaque_value"));
    printRec(E->getExistentialValue(), Label::optional("existential_value"));
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitMakeTemporarilyEscapableExpr(MakeTemporarilyEscapableExpr *E,
                                         Label label) {
    printCommon(E, "make_temporarily_escapable_expr", label);
    printRec(E->getOpaqueValue(), Label::optional("opaque_value"));
    printRec(E->getNonescapingClosureValue(),
             Label::optional("nonescaping_closure_value"));
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitEditorPlaceholderExpr(EditorPlaceholderExpr *E, Label label) {
    printCommon(E, "editor_placeholder_expr", label);

    auto *TyR = E->getPlaceholderTypeRepr();
    auto *ExpTyR = E->getTypeForExpansion();
    if (TyR)
      printRec(TyR, Label::optional("placeholder_type_repr"));
    if (ExpTyR && ExpTyR != TyR) {
      printRec(ExpTyR, Label::optional("type_repr_for_expansion"));
    }
    if (auto *SE = E->getSemanticExpr()) {
      printRec(SE, Label::always("semantic_expr"));
    }
    printFoot();
  }
  void visitLazyInitializerExpr(LazyInitializerExpr *E, Label label) {
    printCommon(E, "lazy_initializer_expr", label);
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printFoot();
  }
  void visitObjCSelectorExpr(ObjCSelectorExpr *E, Label label) {
    printCommon(E, "objc_selector_expr", label);

    printField(E->getSelectorKind(), Label::always("kind"));
    printDeclRefField(E->getMethod(), Label::always("decl"));

    printRec(E->getSubExpr(), Label::optional("sub_expr"));

    printFoot();
  }

  void visitKeyPathExpr(KeyPathExpr *E, Label label) {
    printCommon(E, "keypath_expr", label);

    printFlag(E->isObjC(), "objc");

    auto printComponents = [&]{
      printList(indices(E->getComponents()), [&](unsigned i, Label label) {
        auto &component = E->getComponents()[i];
        printRecArbitrary([&](Label label) {
          switch (component.getKind()) {
          case KeyPathExpr::Component::Kind::Invalid:
            printHead("invalid", ASTNodeColor, label);
            break;

          case KeyPathExpr::Component::Kind::OptionalChain:
            printHead("optional_chain", ASTNodeColor, label);
            break;

          case KeyPathExpr::Component::Kind::OptionalForce:
            printHead("optional_force", ASTNodeColor, label);
            break;

          case KeyPathExpr::Component::Kind::OptionalWrap:
            printHead("optional_wrap", ASTNodeColor, label);
            break;

          case KeyPathExpr::Component::Kind::Member:
            printHead("member", ASTNodeColor, label);
            printDeclRefField(component.getDeclRef(), Label::always("decl"));
            break;

          case KeyPathExpr::Component::Kind::UnresolvedMember:
            printHead("unresolved_member", ASTNodeColor, label);
            printFieldQuoted(component.getUnresolvedDeclName(),
                             Label::always("decl_name"), IdentifierColor);
            break;

          case KeyPathExpr::Component::Kind::Subscript:
            printHead("subscript", ASTNodeColor, label);
            printDeclRefField(component.getDeclRef(), Label::always("decl"));
            break;

          case KeyPathExpr::Component::Kind::UnresolvedSubscript:
            printHead("unresolved_subscript", ASTNodeColor, label);
            break;
          case KeyPathExpr::Component::Kind::Identity:
            printHead("identity", ASTNodeColor, label);
            break;

          case KeyPathExpr::Component::Kind::TupleElement:
            printHead("tuple_element", ASTNodeColor, label);
            printField(component.getTupleIndex(), Label::always("index"),
                       DiscriminatorColor);
            break;
          case KeyPathExpr::Component::Kind::DictionaryKey:
            printHead("dict_key", ASTNodeColor, label);
            printFieldQuoted(component.getUnresolvedDeclName(),
                             Label::always("key"), IdentifierColor);
            break;
          case KeyPathExpr::Component::Kind::CodeCompletion:
            printHead("completion", ASTNodeColor, label);
            break;
          case KeyPathExpr::Component::Kind::UnresolvedApply:
            printHead("unresolved_apply", ASTNodeColor, label);
            break;
          case KeyPathExpr::Component::Kind::Apply:
            printHead("apply", ASTNodeColor, label);
            break;
          }
          printTypeField(GetTypeOfKeyPathComponent(E, i), Label::always("type"));
          if (auto *args = component.getArgs()) {
            printRec(args, Label::optional("args"));
          }
          printFoot();
        }, Label::optional("component"));
      }, Label::optional("components"));
    };

    if (Writer.isParsable()) {
      printComponents();
    } else {
      // `printList` in default mode simply indents, so we need to preserve the
      // additional level of `(components ...)` for that mode.
      printRecArbitrary([&](Label label) {
        printHead("components", ExprColor, label);
        printComponents();
        printFoot();
      }, Label::optional("components"));
    }

    if (auto stringLiteral = E->getObjCStringLiteralExpr()) {
      printRec(stringLiteral, Label::always("objc_string_literal"));
    }
    if (!E->isObjC()) {
      if (auto root = E->getParsedRoot()) {
        printRec(root, Label::always("parsed_root"));
      }
      if (auto path = E->getParsedPath()) {
        printRec(path, Label::always("parsed_path"));
      }
    }
    printFoot();
  }

  void visitCurrentContextIsolationExpr(
      CurrentContextIsolationExpr *E, Label label) {
    printCommon(E, "current_context_isolation_expr", label);
    if (auto actor = E->getActor())
      printRec(actor, Label::optional("actor"));

    printFoot();
  }

  void visitKeyPathDotExpr(KeyPathDotExpr *E, Label label) {
    printCommon(E, "key_path_dot_expr", label);
    printFoot();
  }

  void visitSingleValueStmtExpr(SingleValueStmtExpr *E, Label label) {
    printCommon(E, "single_value_stmt_expr", label);
    printDeclContext(E);
    printRec(E->getStmt(), &E->getDeclContext()->getASTContext(),
             Label::optional("stmt"));
    printFoot();
  }

  void visitTapExpr(TapExpr *E, Label label) {
    printCommon(E, "tap_expr", label);
    printDeclRefField(E->getVar(), Label::always("var"));
    printRec(E->getSubExpr(), Label::optional("sub_expr"));
    printRec(E->getBody(), &E->getVar()->getDeclContext()->getASTContext(),
             Label::optional("body"));
    printFoot();
  }

  void visitTypeJoinExpr(TypeJoinExpr *E, Label label) {
    printCommon(E, "type_join_expr", label);

    if (auto *var = E->getVar()) {
      printRec(var, Label::always("var"));
    }

    if (auto *SVE = E->getSingleValueStmtExpr()) {
      printRec(SVE, Label::always("single_value_stmt_expr"));
    }

    printList(E->getElements(), [&](auto *member, Label label) {
      printRec(member, label);
    }, Label::optional("elements"));

    printFoot();
  }

  void visitMacroExpansionExpr(MacroExpansionExpr *E, Label label) {
    printCommon(E, "macro_expansion_expr", label);
    printDeclContext(E);

    printFieldQuoted(E->getMacroName(), Label::always("name"), IdentifierColor);
    printField(E->getRawDiscriminator(), Label::always("discriminator"),
               DiscriminatorColor);

    if (E->getArgs()) {
      printRec(E->getArgs(), Label::optional("args"));
    }
    if (auto rewritten = E->getRewritten()) {
      printRec(rewritten, Label::always("rewritten"));
    }

    printFoot();
  }

  void visitTypeValueExpr(TypeValueExpr *E, Label label) {
    printCommon(E, "type_value_expr", label);

    PrintOptions PO;
    PO.PrintTypesForDebugging = true;
    printTypeField(E->getParamType(), Label::always("param_type"), PO,
                   TypeColor);

    printFoot();
  }
};

} // end anonymous namespace

void Expr::dump() const {
  dump(llvm::errs());
  llvm::errs() << "\n";
}

void Expr::dump(raw_ostream &OS, llvm::function_ref<Type(Expr *)> getTypeOfExpr,
                llvm::function_ref<Type(TypeRepr *)> getTypeOfTypeRepr,
                llvm::function_ref<Type(KeyPathExpr *E, unsigned index)>
                    getTypeOfKeyPathComponent,
                unsigned Indent) const {
  DefaultWriter writer(OS, Indent);
  PrintExpr(writer, ASTDumpMemberLoading::None, getTypeOfExpr,
            getTypeOfTypeRepr, getTypeOfKeyPathComponent)
      .visit(const_cast<Expr *>(this), Label::optional(""));
}

void Expr::dump(raw_ostream &OS, unsigned Indent) const {
  dump(OS, defaultGetTypeOfExpr, /*getTypeOfTypeRepr*/ nullptr,
       defaultGetTypeOfKeyPathComponent, Indent);
}

void Expr::print(ASTPrinter &Printer, const PrintOptions &Opts) const {
  // FIXME: Fully use the ASTPrinter.
  llvm::SmallString<128> Str;
  llvm::raw_svector_ostream OS(Str);
  dump(OS);
  Printer << OS.str();
}

void ArgumentList::dump() const {
  dump(llvm::errs(), 0);
  llvm::errs() << '\n';
}

void ArgumentList::dump(raw_ostream &OS, unsigned Indent) const {
  DefaultWriter writer(OS, Indent);
  PrintBase(writer).visitArgumentList(this, Label::optional(""));
}

//===----------------------------------------------------------------------===//
// Printing for TypeRepr and all subclasses.
//===----------------------------------------------------------------------===//

namespace {
class PrintTypeRepr : public TypeReprVisitor<PrintTypeRepr, void, Label>,
                      public PrintBase {
public:
  using PrintBase::PrintBase;

  void printCommon(const char *Name, Label label) {
    printHead(Name, TypeReprColor, label);
  }

  void visitErrorTypeRepr(ErrorTypeRepr *T, Label label) {
    printCommon("type_error", label);
  }

  void visitAttributedTypeRepr(AttributedTypeRepr *T, Label label) {
    printCommon("type_attributed", label);
    printFieldQuotedRaw([&](raw_ostream &OS) { T->printAttrs(OS); },
                        Label::always("attrs"));
    printRec(T->getTypeRepr(), Label::optional("type_repr"));
  }

  void visitDeclRefTypeRepr(DeclRefTypeRepr *T, Label label) {
    printCommon(isa<UnqualifiedIdentTypeRepr>(T) ? "type_unqualified_ident"
                                                 : "type_qualified_ident",
                label);

    printFieldQuoted(T->getNameRef(), Label::always("id"), IdentifierColor);
    if (T->isBound()) {
      printReferencedDeclWithContextField(T->getBoundDecl(), Label::always("bind"));
      printDeclContext(T);
    } else {
      printFlag("unbound");
    }

    if (auto *qualIdentTR = dyn_cast<QualifiedIdentTypeRepr>(T)) {
      printRec(qualIdentTR->getBase(), Label::optional("base"));
    }

    printList(T->getGenericArgs(), [&](auto *genArg, Label label) {
      printRec(genArg, label);
    }, Label::optional("generic_args"));

    printFoot();
  }

  void visitFunctionTypeRepr(FunctionTypeRepr *T, Label label) {
    printCommon("type_function", label);

    printFlag(T->isAsync(), "async");
    printFlag(T->isThrowing(), "throws");

    printRec(T->getArgsTypeRepr(), Label::optional("args_type_repr"));
    printRec(T->getResultTypeRepr(), Label::optional("result_type_repr"));

    printFoot();
  }

  void visitInverseTypeRepr(InverseTypeRepr *T, Label label) {
    printCommon("inverse", label);
    printRec(T->getConstraint(), Label::optional("constraint"));
    printFoot();
  }

  void visitArrayTypeRepr(ArrayTypeRepr *T, Label label) {
    printCommon("type_array", label);
    printRec(T->getBase(), Label::optional("element_type"));
    printFoot();
  }

  void visitInlineArrayTypeRepr(InlineArrayTypeRepr *T, Label label) {
    printCommon("type_inline_array", label);
    printRec(T->getCount(), Label::always("count"));
    printRec(T->getElement(), Label::always("element"));
    printFoot();
  }

  void visitDictionaryTypeRepr(DictionaryTypeRepr *T, Label label) {
    printCommon("type_dictionary", label);
    printRec(T->getKey(), Label::optional("key"));
    printRec(T->getValue(), Label::optional("value"));
    printFoot();
  }

  void visitVarargTypeRepr(VarargTypeRepr *T, Label label) {
    printCommon("vararg", label);
    printRec(T->getElementType(), Label::optional("element_type"));
    printFoot();
  }

  void visitPackTypeRepr(PackTypeRepr *T, Label label) {
    printCommon("pack", label);
    printList(T->getElements(), [&](auto elt, Label label) {
      printRec(elt, label);
    }, Label::optional("elements"));
    printFoot();
  }

  void visitPackExpansionTypeRepr(PackExpansionTypeRepr *T, Label label) {
    printCommon("pack_expansion", label);
    printRec(T->getPatternType(), Label::optional("pattern_type"));
    printFoot();
  }

  void visitPackElementTypeRepr(PackElementTypeRepr *T, Label label) {
    printCommon("pack_element", label);
    printRec(T->getPackType(), Label::optional("pack_type"));
    printFoot();
  }

  void visitTupleTypeRepr(TupleTypeRepr *T, Label label) {
    printCommon("type_tuple", label);

    if (T->hasElementNames()) {
      printFieldQuotedRaw([&](raw_ostream &OS) {
        llvm::interleave(T->getElements(), OS,
                         [&](const TupleTypeReprElement &Elt) {
          auto name = Elt.Name;
          if (Elt.UnderscoreLoc.isValid())
            OS << (name.empty() ? "_" : "_ " + name.str());
          else
            OS << (name.empty() ? "''" : name.str());
        }, ",");
      }, Label::always("names"));
    }

    printList(T->getElements(), [&](auto elem, Label label) {
      printRec(elem.Type, label);
    }, Label::optional("elements"));

    printFoot();
  }

  void visitCompositionTypeRepr(CompositionTypeRepr *T, Label label) {
    printCommon("type_composite", label);
    printList(T->getTypes(), [&](auto elem, Label label) {
      printRec(elem, label);
    }, Label::optional("types"));
    printFoot();
  }

  void visitMetatypeTypeRepr(MetatypeTypeRepr *T, Label label) {
    printCommon("type_metatype", label);
    printRec(T->getBase(), Label::optional("base"));
    printFoot();
  }

  void visitProtocolTypeRepr(ProtocolTypeRepr *T, Label label) {
    printCommon("type_protocol", label);
    printRec(T->getBase(), Label::optional("base"));
    printFoot();
  }

  void visitOwnershipTypeRepr(OwnershipTypeRepr *T, Label label) {
    printCommon("type_ownership", label);
    printFlag(getDumpString(T->getSpecifier()));
    printRec(T->getBase(), Label::optional("base"));
    printFoot();
  }
  
  void visitIsolatedTypeRepr(IsolatedTypeRepr *T, Label label) {
    printCommon("isolated", label);
    printRec(T->getBase(), Label::optional("base"));
    printFoot();
  }

  void visitSendingTypeRepr(SendingTypeRepr *T, Label label) {
    printCommon("sending", label);
    printRec(T->getBase(), Label::optional("base"));
    printFoot();
  }

  void visitCallerIsolatedTypeRepr(CallerIsolatedTypeRepr *T, Label label) {
    printCommon("caller_isolated", label);
    printRec(T->getBase(), Label::optional("base"));
    printFoot();
  }

  void visitCompileTimeLiteralTypeRepr(CompileTimeLiteralTypeRepr *T, Label label) {
    printCommon("_const", label);
    printRec(T->getBase(), Label::optional("base"));
    printFoot();
  }
                        
  void visitConstValueTypeRepr(ConstValueTypeRepr *T, Label label) {
    printCommon("@const", label);
    printRec(T->getBase(), Label::optional("base"));
    printFoot();
  }

  void visitOptionalTypeRepr(OptionalTypeRepr *T, Label label) {
    printCommon("type_optional", label);
    printRec(T->getBase(), Label::optional("base"));
    printFoot();
  }

  void visitImplicitlyUnwrappedOptionalTypeRepr(
      ImplicitlyUnwrappedOptionalTypeRepr *T, Label label) {
    printCommon("type_implicitly_unwrapped_optional", label);
    printRec(T->getBase(), Label::optional("base"));
    printFoot();
  }

  void visitOpaqueReturnTypeRepr(OpaqueReturnTypeRepr *T, Label label) {
    printCommon("type_opaque_return", label);
    printRec(T->getConstraint(), Label::optional("constraint"));
    printFoot();
  }

  void visitNamedOpaqueReturnTypeRepr(NamedOpaqueReturnTypeRepr *T,
                                      Label label) {
    printCommon("type_named_opaque_return", label);
    printRec(T->getBase(), Label::optional("base"));
    printFoot();
  }

  void visitExistentialTypeRepr(ExistentialTypeRepr *T, Label label) {
    printCommon("type_existential", label);
    printRec(T->getConstraint(), Label::optional("constraint"));
    printFoot();
  }

  void visitPlaceholderTypeRepr(PlaceholderTypeRepr *T, Label label) {
    printCommon("type_placeholder", label);
    printFoot();
  }

  void visitFixedTypeRepr(FixedTypeRepr *T, Label label) {
    printCommon("type_fixed", label);

    auto Ty = T->getType();
    if (Ty) {
      printSourceLoc(T->getLoc(), &Ty->getASTContext());
    }

    printRec(Ty, Label::always("type"));

    printFoot();
  }

  void visitSelfTypeRepr(SelfTypeRepr *T, Label label) {
    printCommon("type_self", label);

    auto Ty = T->getType();
    if (Ty) {
      printSourceLoc(T->getLoc(), &Ty->getASTContext());
    }

    printRec(Ty, Label::always("type"));

    printFoot();
  }

  void visitSILBoxTypeRepr(SILBoxTypeRepr *T, Label label) {
    printCommon("sil_box", label);

    printList(T->getFields(), [&](auto &Field, Label label) {
      printRecArbitrary([&](Label label) {
        printCommon("sil_box_field", label);
        printFlag(Field.isMutable(), "mutable");

        printRec(Field.getFieldType(), Label::optional("field_type"));
        printFoot();
      }, label);
    }, Label::optional("fields"));

    printList(T->getGenericArguments(), [&](auto arg, Label label) {
      printRec(arg, label);
    }, Label::optional("generic_args"));

    printFoot();
  }

  void visitLifetimeDependentTypeRepr(LifetimeDependentTypeRepr *T,
                                      Label label) {
    printCommon("type_lifetime_dependent_return", label);

    // FIXME: Improve lifetime entries in parsable output formats.
    printFieldRaw(
        [&](raw_ostream &out) {
          out << " " << T->getLifetimeEntry()->getString() << " ";
        },
        Label::optional("lifetime_entry"));
    printRec(T->getBase(), Label::optional("base"));
    printFoot();
  }

  void visitIntegerTypeRepr(IntegerTypeRepr *T, Label label) {
    printCommon("type_integer", label);

    if (T->getMinusLoc()) {
      printCommon("is_negative", label);
    }

    printFieldQuoted(T->getValue(), Label::always("value"), IdentifierColor);
    printFoot();
  }
};
} // end anonymous namespace

//===----------------------------------------------------------------------===//
// Dumping for DeclAttributes
//===----------------------------------------------------------------------===//

namespace {
class PrintAttribute : public AttributeVisitor<PrintAttribute, void, Label>,
                       public PrintBase {
  const ASTContext *Ctx;
  DeclContext *DC;

public:
  PrintAttribute(
      PrintWriterBase &writer, const ASTContext *ctx, DeclContext *dc,
      ASTDumpMemberLoading memberLoading = ASTDumpMemberLoading::None,
      llvm::function_ref<Type(Expr *)> getTypeOfExpr = defaultGetTypeOfExpr,
      llvm::function_ref<Type(TypeRepr *)> getTypeOfTypeRepr = nullptr,
      llvm::function_ref<Type(KeyPathExpr *E, unsigned index)>
          getTypeOfKeyPathComponent = defaultGetTypeOfKeyPathComponent)
      : PrintBase(writer, memberLoading, getTypeOfExpr, getTypeOfTypeRepr,
                  getTypeOfKeyPathComponent),
        Ctx(ctx), DC(dc) {}

  bool isTypeChecked() const {
    return PrintBase::isTypeChecked() && DC;
  }

  void printCommon(DeclAttribute *Attr, StringRef name, Label label) {
    printHead(name, DeclAttributeColor, label);
    printFlag(Attr->isImplicit(), "implicit");
    printFlag(Attr->isInvalid(), "invalid");
    printFlag(Attr->getAddedByAccessNote(), "added_by_access_note");
    printSourceRange(Attr->Range, Ctx);
  }

  /// Deleting this ensures that all attributes are covered by the
  /// visitor below.
  void visitDeclAttribute(DeclAttribute *A) = delete;

#define TRIVIAL_ATTR_PRINTER(Class, Name)                                      \
  void visit##Class##Attr(Class##Attr *Attr, Label label) {                \
    printCommon(Attr, #Name "_attr", label);                                   \
    printFoot();                                                               \
  }

  TRIVIAL_ATTR_PRINTER(Actor, actor)
  TRIVIAL_ATTR_PRINTER(AddressableSelf, _addressableSelf)
  TRIVIAL_ATTR_PRINTER(AddressableForDependencies, _addressableForDependencies)
  TRIVIAL_ATTR_PRINTER(AlwaysEmitConformanceMetadata,
                       always_emit_conformance_metadata)
  TRIVIAL_ATTR_PRINTER(AlwaysEmitIntoClient, always_emit_into_client)
  TRIVIAL_ATTR_PRINTER(Async, async)
  TRIVIAL_ATTR_PRINTER(AtReasync, at_reasync)
  TRIVIAL_ATTR_PRINTER(AtRethrows, at_rethrows)
  TRIVIAL_ATTR_PRINTER(Borrowed, borrowed)
  TRIVIAL_ATTR_PRINTER(Borrowing, borrowing)
  TRIVIAL_ATTR_PRINTER(CompileTimeLiteral, compile_time_literal)
  TRIVIAL_ATTR_PRINTER(ConstVal, compile_time_value)
  TRIVIAL_ATTR_PRINTER(ConstInitialized, const_initialized)
  TRIVIAL_ATTR_PRINTER(CompilerInitialized, compiler_initialized)
  TRIVIAL_ATTR_PRINTER(Consuming, consuming)
  TRIVIAL_ATTR_PRINTER(Convenience, convenience)
  TRIVIAL_ATTR_PRINTER(DiscardableResult, discardable_result)
  TRIVIAL_ATTR_PRINTER(DisfavoredOverload, disfavored_overload)
  TRIVIAL_ATTR_PRINTER(DistributedActor, distributed_actor)
  TRIVIAL_ATTR_PRINTER(Dynamic, dynamic)
  TRIVIAL_ATTR_PRINTER(DynamicCallable, dynamic_callable)
  TRIVIAL_ATTR_PRINTER(DynamicMemberLookup, dynamic_member_lookup)
  TRIVIAL_ATTR_PRINTER(EagerMove, eager_move)
  TRIVIAL_ATTR_PRINTER(EmitAssemblyVisionRemarks, emit_assembly_vision_remarks)
  TRIVIAL_ATTR_PRINTER(Exported, exported)
  TRIVIAL_ATTR_PRINTER(ExtractConstantsFromMembers,
                       extract_constants_from_members)
  TRIVIAL_ATTR_PRINTER(Final, final)
  TRIVIAL_ATTR_PRINTER(FixedLayout, fixed_layout)
  TRIVIAL_ATTR_PRINTER(ForbidSerializingReference, forbid_serializing_reference)
  TRIVIAL_ATTR_PRINTER(Frozen, frozen)
  TRIVIAL_ATTR_PRINTER(GKInspectable, gk_inspectable)
  TRIVIAL_ATTR_PRINTER(GlobalActor, global_actor)
  TRIVIAL_ATTR_PRINTER(HasInitialValue, has_initial_value)
  TRIVIAL_ATTR_PRINTER(HasMissingDesignatedInitializers,
                       has_missing_designated_initializers)
  TRIVIAL_ATTR_PRINTER(HasStorage, has_storage)
  TRIVIAL_ATTR_PRINTER(IBAction, ib_action)
  TRIVIAL_ATTR_PRINTER(IBDesignable, ib_designable)
  TRIVIAL_ATTR_PRINTER(IBInspectable, ib_inspectable)
  TRIVIAL_ATTR_PRINTER(IBOutlet, ib_outlet)
  TRIVIAL_ATTR_PRINTER(IBSegueAction, ib_segue_action)
  TRIVIAL_ATTR_PRINTER(ImplementationOnly, implementation_only)
  TRIVIAL_ATTR_PRINTER(ImplicitSelfCapture, implicit_self_capture)
  TRIVIAL_ATTR_PRINTER(Indirect, indirect)
  TRIVIAL_ATTR_PRINTER(Infix, infix)
  TRIVIAL_ATTR_PRINTER(InheritsConvenienceInitializers,
                       inherits_convenience_initializers)
  TRIVIAL_ATTR_PRINTER(Inlinable, inlinable)
  TRIVIAL_ATTR_PRINTER(Isolated, isolated)
  TRIVIAL_ATTR_PRINTER(KnownToBeLocal, known_to_be_local)
  TRIVIAL_ATTR_PRINTER(LLDBDebuggerFunction, lldb_debugger_function)
  TRIVIAL_ATTR_PRINTER(Lazy, lazy)
  TRIVIAL_ATTR_PRINTER(LegacyConsuming, legacy_consuming)
  TRIVIAL_ATTR_PRINTER(LexicalLifetimes, lexical_lifetimes)
  TRIVIAL_ATTR_PRINTER(MainType, main_type)
  TRIVIAL_ATTR_PRINTER(Marker, marker)
  TRIVIAL_ATTR_PRINTER(MoveOnly, move_only)
  TRIVIAL_ATTR_PRINTER(Mutating, mutating)
  TRIVIAL_ATTR_PRINTER(NSApplicationMain, ns_application_main)
  TRIVIAL_ATTR_PRINTER(NSCopying, ns_copying)
  TRIVIAL_ATTR_PRINTER(NSManaged, ns_managed)
  TRIVIAL_ATTR_PRINTER(NoAllocation, no_allocation)
  TRIVIAL_ATTR_PRINTER(NoDerivative, no_derivative)
  TRIVIAL_ATTR_PRINTER(NoEagerMove, no_eager_move)
  TRIVIAL_ATTR_PRINTER(NoExistentials, no_existentials)
  TRIVIAL_ATTR_PRINTER(NoImplicitCopy, no_implicit_copy)
  TRIVIAL_ATTR_PRINTER(NoLocks, no_locks)
  TRIVIAL_ATTR_PRINTER(NoMetadata, no_metadata)
  TRIVIAL_ATTR_PRINTER(NoObjCBridging, no_objc_bridging)
  TRIVIAL_ATTR_PRINTER(NoRuntime, no_runtime)
  TRIVIAL_ATTR_PRINTER(NonEphemeral, non_ephemeral)
  TRIVIAL_ATTR_PRINTER(NonEscapable, non_escapable)
  TRIVIAL_ATTR_PRINTER(NonMutating, non_mutating)
  TRIVIAL_ATTR_PRINTER(NonObjC, non_objc)
  TRIVIAL_ATTR_PRINTER(NonOverride, non_override)
  TRIVIAL_ATTR_PRINTER(ObjCMembers, objc_members)
  TRIVIAL_ATTR_PRINTER(ObjCNonLazyRealization, objc_non_lazy_realization)
  TRIVIAL_ATTR_PRINTER(Optional, optional)
  TRIVIAL_ATTR_PRINTER(Override, override)
  TRIVIAL_ATTR_PRINTER(Postfix, postfix)
  TRIVIAL_ATTR_PRINTER(PreInverseGenerics, pre_inverse_generics)
  TRIVIAL_ATTR_PRINTER(Preconcurrency, preconcurrency)
  TRIVIAL_ATTR_PRINTER(Prefix, prefix)
  TRIVIAL_ATTR_PRINTER(PropertyWrapper, property_wrapper)
  TRIVIAL_ATTR_PRINTER(Reasync, reasync)
  TRIVIAL_ATTR_PRINTER(Required, required)
  TRIVIAL_ATTR_PRINTER(RequiresStoredPropertyInits,
                       requires_stored_property_inits)
  TRIVIAL_ATTR_PRINTER(ResultBuilder, result_builder)
  TRIVIAL_ATTR_PRINTER(Rethrows, rethrows)
  TRIVIAL_ATTR_PRINTER(Safe, safe)
  TRIVIAL_ATTR_PRINTER(SPIOnly, spi_only)
  TRIVIAL_ATTR_PRINTER(Sendable, sendable)
  TRIVIAL_ATTR_PRINTER(Sensitive, sensitive)
  TRIVIAL_ATTR_PRINTER(ShowInInterface, show_in_interface)
  TRIVIAL_ATTR_PRINTER(SpecializeExtension, specialize_extension)
  TRIVIAL_ATTR_PRINTER(StaticExclusiveOnly, static_exclusive_only)
  TRIVIAL_ATTR_PRINTER(StaticInitializeObjCMetadata,
                       static_initialize_objc_metadata)
  TRIVIAL_ATTR_PRINTER(Testable, testable)
  TRIVIAL_ATTR_PRINTER(Transparent, transparent)
  TRIVIAL_ATTR_PRINTER(UIApplicationMain, ui_application_main)
  TRIVIAL_ATTR_PRINTER(Unsafe, unsafe)
  TRIVIAL_ATTR_PRINTER(UnsafeInheritExecutor, unsafe_inherit_executor)
  TRIVIAL_ATTR_PRINTER(UnsafeNoObjCTaggedPointer, unsafe_no_objc_tagged_pointer)
  TRIVIAL_ATTR_PRINTER(UnsafeNonEscapableResult, unsafe_non_escapable_result)
  TRIVIAL_ATTR_PRINTER(UsableFromInline, usable_from_inline)
  TRIVIAL_ATTR_PRINTER(Used, used)
  TRIVIAL_ATTR_PRINTER(WarnUnqualifiedAccess, warn_unqualified_access)
  TRIVIAL_ATTR_PRINTER(WeakLinked, weak_linked)
  TRIVIAL_ATTR_PRINTER(Nonexhaustive, nonexhaustive)
  TRIVIAL_ATTR_PRINTER(Concurrent, concurrent)

#undef TRIVIAL_ATTR_PRINTER

  void visitABIAttr(ABIAttr *Attr, Label label) {
    printCommon(Attr, "abi_attr", label);
    printRec(Attr->abiDecl, Label::always("decl"));
    printFoot();
  }
  void visitAccessControlAttr(AccessControlAttr *Attr, Label label) {
    printCommon(Attr, "access_control_attr", label);
    printField(Attr->getAccess(), Label::always("access_level"));
    printFoot();
  }
  void visitAlignmentAttr(AlignmentAttr *Attr, Label label) {
    printCommon(Attr, "alignment_attr", label);
    printField(Attr->getValue(), Label::always("value"));
    printFoot();
  }
  void visitAllowFeatureSuppressionAttr(AllowFeatureSuppressionAttr *Attr,
                                        Label label) {
    printCommon(Attr, "allow_feature_suppression_attr", label);
    printStringListField(Attr->getSuppressedFeatures(),
                         [&](auto name) { return name.str(); },
                         Label::always("features"));
    printFoot();
  }
  void visitAvailableAttr(AvailableAttr *Attr, Label label) {
    printCommon(Attr, "available_attr", label);

    printFlag(Attr->isGroupMember(), "group_member");
    printFlag(Attr->isGroupedWithWildcard(), "group_with_wildcard");
    printFlag(Attr->isGroupTerminator(), "group_terminator");

    auto domainOrIdentifier = Attr->getDomainOrIdentifier();
    if (domainOrIdentifier.isDomain()) {
      printFieldRaw([&](auto &out) { domainOrIdentifier.getAsDomain()->print(out); },
                    Label::always("domain"));
    } else {
      printFlag(domainOrIdentifier.isResolved(), "resolved");
      printField(*domainOrIdentifier.getAsIdentifier(), Label::always("domainIdentifier"));
    }

    switch (Attr->getKind()) {
    case swift::AvailableAttr::Kind::Default:
      break;
    case swift::AvailableAttr::Kind::Deprecated:
      printFlag("deprecated");
      break;
    case swift::AvailableAttr::Kind::Unavailable:
      printFlag("unavailable");
      break;
    case swift::AvailableAttr::Kind::NoAsync:
      printFlag("noasync");
      break;
    }
    if (auto introduced = Attr->getRawIntroduced())
      printFieldRaw([&](auto &out) { out << introduced.value().getAsString(); },
                    Label::always("introduced"));
    if (auto deprecated = Attr->getRawDeprecated())
      printFieldRaw([&](auto &out) { out << deprecated.value().getAsString(); },
                    Label::always("deprecated"));
    if (auto obsoleted = Attr->getRawObsoleted())
      printFieldRaw([&](auto &out) { out << obsoleted.value().getAsString(); },
                    Label::always("obsoleted"));
    if (!Attr->getMessage().empty())
      printFieldQuoted(Attr->getMessage(), Label::always("message"));
    if (!Attr->getRename().empty())
      printFieldQuoted(Attr->getRename(), Label::always("rename"));
    printFoot();
  }
  void visitBackDeployedAttr(BackDeployedAttr *Attr, Label label) {
    printCommon(Attr, "back_deployed_attr", label);
    printField(Attr->getPlatform(), Label::always("platform"));
    printFieldRaw(
        [&](auto &out) { out << Attr->getParsedVersion().getAsString(); },
        Label::always("version"));
    printFoot();
  }
  void visitCDeclAttr(CDeclAttr *Attr, Label label) {
    printCommon(Attr, "cdecl_attr", label);
    printFieldQuoted(Attr->Name, Label::always("name"));
    printFoot();
  }
  void
  visitClangImporterSynthesizedTypeAttr(ClangImporterSynthesizedTypeAttr *Attr,
                                        Label label) {
    printCommon(Attr, "clang_importer_synthesized_type_attr", label);
    printField(Attr->getKind(), Label::always("kind"));
    printField(Attr->originalTypeName, Label::always("original_type_name"));
    printFoot();
  }
  void visitCustomAttr(CustomAttr *Attr, Label label) {
    printCommon(Attr, "custom_attr", label);

    printField(
        static_cast<void *>(static_cast<DeclContext *>(Attr->getInitContext())),
        Label::always("init_context"));

    if (Attr->getType()) {
      printTypeField(Attr->getType(), Label::always("type"));
    } else if (isTypeChecked()) {
      // If the type is null, it might be a macro reference. Try that if we're
      // dumping the fully type-checked AST.
      auto macroRef =
          evaluateOrDefault(const_cast<ASTContext *>(Ctx)->evaluator,
                            ResolveMacroRequest{Attr, DC}, ConcreteDeclRef());
      if (macroRef) {
        printDeclRefField(macroRef, Label::always("macro"));
      }
    }
    if (!Writer.isParsable()) {
      // The type has the semantic information we want for parsable outputs, so
      // omit the `TypeRepr` there. This also works for macro references.
      printRec(Attr->getTypeRepr(), Label::optional("type_repr"));
    }
    if (Attr->getArgs())
      printRec(Attr->getArgs(), Label::optional("args"));
    printFoot();
  }
  void visitDerivativeAttr(DerivativeAttr *Attr, Label label) {
    printCommon(Attr, "derivative_attr", label);
    printRec(Attr->getBaseTypeRepr(), Label::optional("base_type_repr"));
    printName(Attr->getOriginalFunctionName().Name.getFullName(),
              Label::always("original_function_name"));
    // TODO: Print parameters.
    printFoot();
  }
  void visitDifferentiableAttr(DifferentiableAttr *Attr, Label label) {
    printCommon(Attr, "differentiable_attr", label);
    // TODO: Implement.
    printFoot();
  }
  void visitDocumentationAttr(DocumentationAttr *Attr, Label label) {
    printCommon(Attr, "documentation_attr", label);
    printFieldQuoted(Attr->Metadata, Label::always("metadata"));
    if (Attr->Visibility.has_value())
      printField(Attr->Visibility.value(), Label::always("visibility"));
    printFoot();
  }
  void visitDynamicReplacementAttr(DynamicReplacementAttr *Attr,
                                   Label label) {
    printCommon(Attr, "dynamic_replacement_attr", label);
    printName(Attr->getReplacedFunctionName().getFullName(),
              Label::always("replaced_function_name"));
    printFoot();
  }
  void visitEffectsAttr(EffectsAttr *Attr, Label label) {
    printCommon(Attr, "effects_attr", label);
    printField(Attr->getKind(), Label::always("kind"));
    if (Attr->getKind() == EffectsKind::Custom) {
      printFieldQuoted(Attr->getCustomString(), Label::always("custom"));
    }
    printFoot();
  }
  void visitExclusivityAttr(ExclusivityAttr *Attr, Label label) {
    printCommon(Attr, "exclusivity_attr", label);
    printField(Attr->getMode(), Label::always("mode"));
    printFoot();
  }
  void visitExposeAttr(ExposeAttr *Attr, Label label) {
    printCommon(Attr, "expose_attr", label);
    printFieldQuoted(Attr->Name, Label::always("name"));
    printFoot();
  }
  void visitExternAttr(ExternAttr *Attr, Label label) {
    printCommon(Attr, "extern_attr", label);
    printField(Attr->getExternKind(), Label::always("kind"));
    if (Attr->ModuleName.has_value())
      printField(Attr->ModuleName.value(), Label::always("module"));
    printFieldQuoted(Attr->Name, Label::always("name"));
    printFoot();
  }
  void visitImplementsAttr(ImplementsAttr *Attr, Label label) {
    printCommon(Attr, "implements_attr", label);
    if (Writer.isParsable() && isTypeChecked()) {
      // Print the resolved protocol's USR in parsable outputs, not the
      // TypeRepr.
      if (auto PD = Attr->getCachedProtocol(DC); PD && *PD != nullptr) {
        printFieldQuoted(declUSR(*PD), Label::always("protocol"));
      }
    } else {
      printRec(Attr->getProtocolTypeRepr(), Label::always("protocol"));
    }
    printName(Attr->getMemberName(), Label::always("member"));
    printFoot();
  }
  void visitInlineAttr(InlineAttr *Attr, Label label) {
    printCommon(Attr, "inline_attr", label);
    printField(Attr->getKind(), Label::always("kind"));
    printFoot();
  }
  void visitLifetimeAttr(LifetimeAttr *Attr, Label label) {
    printCommon(Attr, "lifetime_attr", label);
    // FIXME: Improve, more detailed info.
    printFieldRaw(
        [&](raw_ostream &out) {
          out << " " << Attr->getLifetimeEntry()->getString() << " ";
        },
        Label::optional("lifetime_entry"));
    printFoot();
  }
  void visitMacroRoleAttr(MacroRoleAttr *Attr, Label label) {
    printCommon(Attr, "macro_role_attr", label);
    switch (Attr->getMacroSyntax()) {
    case MacroSyntax::Attached:
      printFlag("attached");
      break;
    case MacroSyntax::Freestanding:
      printFlag("freestanding");
      break;
    }
    printField(Attr->getMacroRole(), Label::always("role"));
    if (Writer.isParsable()) {
      printList(Attr->getNames(),
                [&](const MacroIntroducedDeclName &name, Label label) {
        printRecArbitrary([&](Label label) {
          printHead("name", FieldLabelColor, label);
          printField(getMacroIntroducedDeclNameString(name.getKind()),
                     Label::always("kind"));
          if (macroIntroducedNameRequiresArgument(name.getKind())) {
            printName(name.getName(), Label::always("argument"));
          }
          printFoot();
        }, Label::always("macro_introduced_decl_name"));
      }, Label::always("names"));
    } else {
      printFieldQuotedRaw(
          [&](auto &out) {
            llvm::interleave(
                Attr->getNames(), out,
                [&](const MacroIntroducedDeclName &name) {
                  out << getMacroIntroducedDeclNameString(name.getKind());
                  if (macroIntroducedNameRequiresArgument(name.getKind())) {
                    out << "(";
                    name.getName().print(out);
                    out << ")";
                  }
                },
                ",");
          },
          Label::always("names"));
    }
    printRecRange(Attr->getConformances(), Label::always("conformances"));

    printFoot();
  }
  void visitNonSendableAttr(NonSendableAttr *Attr, Label label) {
    printCommon(Attr, "non_sendable_attr", label);
    printField(Attr->Specificity, Label::always("specificity"));
    printFoot();
  }
  void visitNonisolatedAttr(NonisolatedAttr *Attr, Label label) {
    printCommon(Attr, "nonisolated_attr", label);
    printFlag(Attr->isUnsafe(), "unsafe");
    printFlag(Attr->isNonSending(), "nonsending");
    printFoot();
  }
  void visitInheritActorContextAttr(InheritActorContextAttr *Attr,
                                    Label label) {
    printCommon(Attr, "inherit_actor_context_attr", label);
    printFlag(Attr->isAlways(), "always");
    printFoot();
  }
  void visitObjCAttr(ObjCAttr *Attr, Label label) {
    printCommon(Attr, "objc_attr", label);
    if (Attr->hasName())
      printFieldQuoted(Attr->getName(), Label::always("name"));
    printFlag(Attr->isNameImplicit(), "is_name_implicit");
    printFoot();
  }
  void visitObjCBridgedAttr(ObjCBridgedAttr *Attr, Label label) {
    printCommon(Attr, "objc_bridged_attr", label);
    printDeclRefField(Attr->getObjCClass(), Label::always("objc_class"));
    printFoot();
  }
  void visitObjCImplementationAttr(ObjCImplementationAttr *Attr,
                                   Label label) {
    printCommon(Attr, "objc_implementation_attr", label);
    if (!Attr->CategoryName.empty())
      printField(Attr->CategoryName, Label::always("category"));
    printFlag(Attr->isEarlyAdopter(), "is_early_adopter");
    printFlag(Attr->isCategoryNameInvalid(), "is_category_name_invalid");
    printFlag(Attr->hasInvalidImplicitLangAttrs(),
              "has_invalid_implicit_lang_attrs");
    printFoot();
  }
  void visitObjCRuntimeNameAttr(ObjCRuntimeNameAttr *Attr, Label label) {
    printCommon(Attr, "objc_runtime_name_attr", label);
    printField(Attr->Name, Label::always("name"));
    printFoot();
  }
  void visitOptimizeAttr(OptimizeAttr *Attr, Label label) {
    printCommon(Attr, "optimize_attr", label);
    printField(Attr->getMode(), Label::always("mode"));
    printFoot();
  }
  void visitOriginallyDefinedInAttr(OriginallyDefinedInAttr *Attr,
                                    Label label) {
    printCommon(Attr, "originally_defined_in_attr", label);
    printField(Attr->getManglingModuleName(), Label::always("mangling_module"));
    printField(Attr->getLinkerModuleName(), Label::always("linker_module"));
    printField(Attr->getPlatform(), Label::always("platform"));
    printFieldRaw(
        [&](auto &out) { out << Attr->getParsedMovedVersion().getAsString(); },
        Label::always("moved_version"));
    printFoot();
  }
  void visitPrivateImportAttr(PrivateImportAttr *Attr, Label label) {
    printCommon(Attr, "prinvate_import_attr", label);
    printFieldQuoted(Attr->getSourceFile(), Label::always("source_file"));
    printFoot();
  }
  void visitProjectedValuePropertyAttr(ProjectedValuePropertyAttr *Attr,
                                       Label label) {
    printCommon(Attr, "projected_value_property_attr", label);
    printField(Attr->ProjectionPropertyName, Label::always("name"));
    printFoot();
  }
  void visitRawDocCommentAttr(RawDocCommentAttr *Attr, Label label) {
    printCommon(Attr, "raw_doc_comment_attr", label);
    printFieldRaw(
        [&](auto &out) { Attr->getCommentRange().print(out, Ctx->SourceMgr); },
        Label::always("comment_range"));
    printFoot();
  }
  void visitRawLayoutAttr(RawLayoutAttr *Attr, Label label) {
    printCommon(Attr, "raw_layout_attr", label);
    if (auto *tyR = Attr->getScalarLikeType()) {
      printFlag("scalar_like");
      printRec(tyR, Label::optional("type_repr"));
    } else if (auto typeAndCount = Attr->getArrayLikeTypeAndCount()) {
      printFlag("array_like");
      printRec(typeAndCount->first, Label::optional("type_repr"));
      printRec(typeAndCount->second, Label::optional("count"));
    } else if (auto sizeAndAlignment = Attr->getSizeAndAlignment()) {
      printField(sizeAndAlignment->first, Label::always("size"));
      printField(sizeAndAlignment->second, Label::always("alignment"));
    }
    printFoot();
  }
  void visitReferenceOwnershipAttr(ReferenceOwnershipAttr *Attr,
                                   Label label) {
    printCommon(Attr, "reference_ownership_attr", label);
    printFlag(keywordOf(Attr->get()));
    printFoot();
  }
  void visitRestatedObjCConformanceAttr(RestatedObjCConformanceAttr *Attr,
                                        Label label) {
    printCommon(Attr, "restated_objc_conformance_attr", label);
    if (Attr->Proto) {
      if (Writer.isParsable() && isTypeChecked()) {
        printFieldQuoted(declUSR(Attr->Proto), Label::optional("proto"));
      } else {
        printFieldRaw([&](auto &out) { Attr->Proto->dumpRef(out); },
                      Label::optional("proto"));
      }
    }
    printFoot();
  }
  void visitSILGenNameAttr(SILGenNameAttr *Attr, Label label) {
    printCommon(Attr, "silgen_name_attr", label);
    printFlag(Attr->Raw, "raw");
    printFieldQuoted(Attr->Name, Label::optional("name"));
    printFoot();
  }
  void visitSPIAccessControlAttr(SPIAccessControlAttr *Attr, Label label) {
    printCommon(Attr, "spi_access_control_attr", label);
    printStringListField(Attr->getSPIGroups(),
                         [&](auto name) { return name.str(); },
                         Label::always("groups"));
    printFoot();
  }
  void visitSectionAttr(SectionAttr *Attr, Label label) {
    printCommon(Attr, "section_attr", label);
    printFieldQuoted(Attr->Name, Label::always("name"));
    printFoot();
  }
  void visitSemanticsAttr(SemanticsAttr *Attr, Label label) {
    printCommon(Attr, "semantics_attr", label);
    printFieldQuoted(Attr->Value, Label::always("value"));
    printFoot();
  }
  void visitSetterAccessAttr(SetterAccessAttr *Attr, Label label) {
    printCommon(Attr, "setter_access_attr", label);
    printField(Attr->getAccess(), Label::always("access"));
    printFoot();
  }
  void visitSpecializeAttr(SpecializeAttr *Attr, Label label) {
    visitAbstractSpecializeAttr(Attr, label);
  }

  void visitSpecializedAttr(SpecializedAttr *Attr, Label label) {
    visitAbstractSpecializeAttr(Attr, label);
  }

  void visitAbstractSpecializeAttr(AbstractSpecializeAttr *Attr, Label label) {
    printCommon(Attr, Attr->isPublic() ? "specialized_attr" :
                  "specialize_attr", label);
    printFlag(Attr->isExported(), "exported");
    printFlag(Attr->isFullSpecialization(), "full");
    printFlag(Attr->isPartialSpecialization(), "partial");
    if (Attr->getTargetFunctionName()) {
      printName(Attr->getTargetFunctionName().getFullName(),
                Label::always("target"));
    }
    if (!Attr->getSPIGroups().empty()) {
      printStringListField(Attr->getSPIGroups(),
                           [&](auto name) { return name.str(); },
                           Label::always("spi"));
    }
    if (Attr->getTrailingWhereClause()) {
      printWhereClause(Attr->getTrailingWhereClause(),
                       Label::always("requirements"));
    }
    printList(Attr->getAvailableAttrs(), [&](auto *availableAttr, Label label) {
      printRec(availableAttr, Ctx, DC, label);
    }, Label::optional("available_attrs"));
    printFoot();
  }
  void visitStorageRestrictionsAttr(StorageRestrictionsAttr *Attr,
                                    Label label) {
    printCommon(Attr, "storage_restrictions_attr", label);
    printStringListField(Attr->getInitializesNames(),
                         [&](auto name) { return name.str(); },
                         Label::always("initializes"));
    printStringListField(Attr->getAccessesNames(),
                         [&](auto name) { return name.str(); },
                         Label::always("accesses"));
    printFoot();
  }
  void visitSwiftNativeObjCRuntimeBaseAttr(SwiftNativeObjCRuntimeBaseAttr *Attr,
                                           Label label) {
    printCommon(Attr, "swift_native_objc_runtime_base", label);
    printFieldQuoted(Attr->BaseClassName, Label::always("base_class_name"));
    printFoot();
  }
  void visitSynthesizedProtocolAttr(SynthesizedProtocolAttr *Attr,
                                    Label label) {
    printCommon(Attr, "synthesized_protocol_attr", label);
    printFlag(Attr->isUnchecked(), "unchecked");
    if (Writer.isParsable() && isTypeChecked()) {
      printFieldQuoted(declUSR(Attr->getProtocol()),
                       Label::optional("protocol"));
    } else {
      printFieldQuotedRaw([&](auto &out) { Attr->getProtocol()->dumpRef(out); },
                          Label::always("protocol"));
    }
    printFoot();
  }
  void visitTransposeAttr(TransposeAttr *Attr, Label label) {
    printCommon(Attr, "transpose_attr", label);
    // TODO: Implement.
    printFoot();
  }
  void visitTypeEraserAttr(TypeEraserAttr *Attr, Label label) {
    printCommon(Attr, "type_eraser_attr", label);
    printTypeField(Attr->getTypeWithoutResolving(), Label::always("type"));
    printRec(Attr->getParsedTypeEraserTypeRepr(),
             Label::always("parsed_type_repr"));
    printFoot();
  }
  void visitUnavailableFromAsyncAttr(UnavailableFromAsyncAttr *Attr,
                                     Label label) {
    printCommon(Attr, "unavailable_from_async_attr", label);
    if (Attr->hasMessage()) {
      printFieldQuoted(Attr->Message, Label::always("message"));
    }
    printFoot();
  }
};

} // end anonymous namespace

void DeclAttribute::dump(const ASTContext &ctx) const {
  dump(llvm::errs(), ctx);
  llvm::errs() << '\n';
}

void DeclAttribute::dump(llvm::raw_ostream &os, const ASTContext &ctx) const {
  DefaultWriter writer(os, /*indent=*/0);
  PrintAttribute(writer, &ctx, nullptr)
    .visit(const_cast<DeclAttribute*>(this), Label::optional(""));
}

void DeclAttribute::dump(const DeclContext *dc) const {
  dump(llvm::errs(), dc);
  llvm::errs() << '\n';
}

void DeclAttribute::dump(llvm::raw_ostream &os, const DeclContext *dc) const {
  DefaultWriter writer(os, /*indent=*/0);
  PrintAttribute(writer, &dc->getASTContext(), const_cast<DeclContext*>(dc))
    .visit(const_cast<DeclAttribute*>(this), Label::optional(""));
}


void DeclAttributes::dump(const ASTContext &ctx) const {
  for (auto attr : *this) {
    attr->dump(llvm::errs(), ctx);
    llvm::errs() << '\n';
  }
}

void DeclAttributes::dump(const DeclContext *dc) const {
  for (auto attr : *this) {
    attr->dump(llvm::errs(), dc);
    llvm::errs() << '\n';
  }
}

void PrintBase::printRec(Decl *D, Label label) {
  printRecArbitrary([&](Label label) {
    if (!D) {
      printHead("<null decl>", DeclColor, label);
      printFoot();
    } else {
      PrintDecl(Writer, MemberLoading, GetTypeOfExpr, GetTypeOfTypeRepr,
                GetTypeOfKeyPathComponent)
          .visit(D, label);
    }
  }, label);
}
void PrintBase::printRec(Expr *E, Label label) {
  printRecArbitrary([&](Label label) {
    if (!E) {
      printHead("<null expr>", ExprColor, label);
      printFoot();
    } else {
      PrintExpr(Writer, MemberLoading, GetTypeOfExpr, GetTypeOfTypeRepr,
                GetTypeOfKeyPathComponent)
          .visit(E, label);
    }
  }, label);
}
void PrintBase::printRec(Stmt *S, const ASTContext *Ctx, Label label) {
  printRecArbitrary([&](Label label) {
    if (!S) {
      printHead("<null stmt>", ExprColor, label);
      printFoot();
    } else {
      PrintStmt(Writer, Ctx, MemberLoading, GetTypeOfExpr, GetTypeOfTypeRepr,
                GetTypeOfKeyPathComponent)
          .visit(S, label);
    }
  }, label);
}
void PrintBase::printRec(TypeRepr *T, Label label) {
  printRecArbitrary([&](Label label) {
    if (!T) {
      printHead("<null typerepr>", TypeReprColor, label);
      printFoot();
    } else {
      PrintTypeRepr(Writer, MemberLoading, GetTypeOfExpr, GetTypeOfTypeRepr,
                    GetTypeOfKeyPathComponent)
          .visit(T, label);
    }
  }, label);
}
void PrintBase::printRec(const Pattern *P, Label label) {
  printRecArbitrary([&](Label label) {
    if (!P) {
      printHead("<null pattern>", PatternColor, label);
      printFoot();
    } else {
      PrintPattern(Writer, MemberLoading, GetTypeOfExpr, GetTypeOfTypeRepr,
                   GetTypeOfKeyPathComponent)
          .visit(const_cast<Pattern *>(P), label);
    }
  }, label);
}
void PrintBase::printRec(const DeclAttribute *Attr, const ASTContext *Ctx,
                         DeclContext *DC, Label label) {
  printRecArbitrary(
      [&](Label label) {
        if (!Attr) {
          printHead("<null attribute>", DeclAttributeColor, label);
          printFoot();
        } else {
          PrintAttribute(Writer, Ctx, DC, MemberLoading, GetTypeOfExpr,
                         GetTypeOfTypeRepr, GetTypeOfKeyPathComponent)
              .visit(const_cast<DeclAttribute *>(Attr), label);
        }
      },
      label);
}

void TypeRepr::dump() const {
  dump(llvm::errs());
  llvm::errs() << '\n';
}
void TypeRepr::dump(raw_ostream &os, unsigned indent) const {
  DefaultWriter writer(os, indent);
  PrintTypeRepr(writer).visit(const_cast<TypeRepr*>(this), Label::optional(""));
}

namespace {

class PrintConformance : public PrintBase {
public:
  using PrintBase::PrintBase;

  void visitProtocolConformanceRef(const ProtocolConformanceRef conformance,
                                   VisitedConformances &visited,
                                   Label label) {
    if (conformance.isInvalid()) {
      printHead("invalid_conformance", ASTNodeColor, label);
      printFoot();
    } else if (conformance.isConcrete()) {
      visitProtocolConformance(conformance.getConcrete(), visited, label);
    } else if (conformance.isPack()) {
      visitPackConformance(conformance.getPack(), visited, label);
    } else {
      assert(conformance.isAbstract());

      printHead("abstract_conformance", ASTNodeColor, label);

      PrintOptions PO;
      PO.OpaqueReturnTypePrinting =
          PrintOptions::OpaqueReturnTypePrintingMode::StableReference;

      printTypeField(conformance.getType(), Label::always("type"), PO);
      printReferencedDeclField(conformance.getProtocol(),
                               Label::always("protocol"));
      printFoot();
    }
  }

  void visitProtocolConformance(const ProtocolConformance *conformance,
                                VisitedConformances &visited, Label label) {
    // A recursive conformance shouldn't have its contents printed, or there's
    // infinite recursion. (This also avoids printing things that occur multiple
    // times in a conformance hierarchy.) We also don't print the details in the
    // parsable (JSON) output because it is far too much information when it is
    // rendered as part of every declref that contains such a conformance.
    auto shouldPrintDetails =
        visited.insert(conformance).second && !Writer.isParsable();

    auto printCommon = [&](StringRef kind) {
      printHead(kind, ASTNodeColor, label);
      printTypeField(conformance->getType(), Label::always("type"));
      printReferencedDeclField(conformance->getProtocol(),
                               Label::always("protocol"));
      printField(conformance->getSourceKind(), Label::optional("source_kind"));
      printFlag(conformance->isRetroactive(), "retroactive");
      printIsolation(conformance->getIsolation());
      if (!Writer.isParsable())
        printFlag(!shouldPrintDetails, "<details printed above>");
    };

    switch (conformance->getKind()) {
      case ProtocolConformanceKind::Normal: {
        auto normal = cast<NormalProtocolConformance>(conformance);

        printCommon("normal_conformance");
        printFlag(normal->isPreconcurrency(), "preconcurrency");
        if (normal->isPreconcurrency() && normal->isComplete()) {
          printFlag(normal->isPreconcurrencyEffectful(),
                    "effectful_preconcurrency");
        }
        printFlag(normal->isRetroactive(), "retroactive");
        printFlag(normal->isUnchecked(), "unchecked");
        if (normal->getExplicitSafety() != ExplicitSafety::Unspecified)
          printField(normal->getExplicitSafety(), Label::always("safety"));

        if (!shouldPrintDetails)
          break;

        // Maybe print information about the conforming context?
        if (normal->isLazilyLoaded()) {
          printFlag("lazy");
        } else {
          printListArbitrary([&]{
            normal->forEachTypeWitness([&](const AssociatedTypeDecl *req,
                                           Type ty, const TypeDecl *) -> bool {
              printRecArbitrary([&](Label label) {
                printHead("assoc_type", ASTNodeColor, label);
                printReferencedDeclField(req, Label::always("req"));
                printTypeField(ty->getDesugaredType(), Label::always("type"));
                printFoot();
              }, Label::optional("type_witness"));
              return false;
            });
          }, Label::always("type_witnesses"));
          printListArbitrary([&]{
            normal->forEachValueWitness([&](const ValueDecl *req,
                                            Witness witness) {
              printRecArbitrary([&](Label label) {
                printHead("value", ASTNodeColor, label);
                printFieldQuoted(req->getName(), Label::always("req"));
                if (!witness)
                  printFlag("no_witness");
                else if (witness.getDecl() == req)
                  printFlag("dynamic_witness");
                else if (Writer.isParsable() && isTypeChecked()) {
                  printFieldQuoted(declUSR(witness.getDecl()),
                                   Label::always("witness"));
                } else {
                  printFieldQuotedRaw([&](raw_ostream &out) {
                    witness.getDecl()->dumpRef(out);
                  }, Label::always("witness"));
                }
                printFoot();
              }, Label::optional("value_witness"));
            });
          }, Label::always("value_witnesses"));

          printListArbitrary([&]{
            normal->forEachAssociatedConformance(
                [&](Type t, ProtocolDecl *proto, unsigned index) {
                  printRecArbitrary([&](Label label) {
                    printHead("assoc_conformance", ASTNodeColor, label);
                    printTypeField(t, Label::always("type"));
                    printReferencedDeclField(proto, Label::always("proto"));
                    printRec(normal->getAssociatedConformance(t, proto), visited,
                             Label::optional("conformance"));
                    printFoot();
                  }, Label::optional("associated_conformance"));
                  return false;
                });
          }, Label::always("associated_conformances"));
        }

        if (auto condReqs = normal->getConditionalRequirementsIfAvailable()) {
          printList(*condReqs, [&](auto requirement, Label label) {
            printRec(requirement, label);
          }, Label::optional("conditional_reqs"));
        } else {
          printRecArbitrary([&](Label label) {
            printHead("<conditional requirements unable to be computed>",
                      ASTNodeColor, label);
            printFoot();
          }, Label::optional("conditional_reqs"));
        }
        break;
      }

      case ProtocolConformanceKind::Self: {
        printCommon("self_conformance");
        break;
      }

      case ProtocolConformanceKind::Inherited: {
        auto conf = cast<InheritedProtocolConformance>(conformance);
        printCommon("inherited_conformance");
        if (!shouldPrintDetails)
          break;

        printRec(conf->getInheritedConformance(), visited,
                 Label::optional("inherited_conformance"));
        break;
      }

      case ProtocolConformanceKind::Specialized: {
        auto conf = cast<SpecializedProtocolConformance>(conformance);
        printCommon("specialized_conformance");
        if (!shouldPrintDetails)
          break;

        printRec(conf->getSubstitutionMap(), visited,
                 Label::optional("substitutions"));
        if (auto condReqs = conf->getConditionalRequirementsIfAvailableOrCached(/*computeIfPossible=*/false)) {
          printList(*condReqs, [&](auto subReq, Label label) {
            printRec(subReq, label);
          }, Label::optional("conditional_reqs"));
        } else {
          printRecArbitrary([&](Label label) {
            printHead("<conditional requirements unable to be computed>",
                      ASTNodeColor, label);
            printFoot();
          }, Label::optional("conditional_reqs"));
        }
        printRec(conf->getGenericConformance(), visited,
                 Label::optional("generic_conformance"));
        break;
      }

      case ProtocolConformanceKind::Builtin: {
        printCommon("builtin_conformance");
      }
    }

    printFoot();
  }

  void visitPackConformance(const PackConformance *conformance,
                            VisitedConformances &visited, Label label) {
    printHead("pack_conformance", ASTNodeColor, label);

    printTypeField(conformance->getType(), Label::always("type"));
    printReferencedDeclField(conformance->getProtocol(),
                             Label::always("protocol"));

    printList(conformance->getPatternConformances(), [&](auto conformanceRef, Label label) {
      printRec(conformanceRef, visited, label);
    }, Label::optional("pattern_conformances"));

    printFoot();
  }

  void visitSubstitutionMap(SubstitutionMap map,
                            SubstitutionMap::DumpStyle style,
                            VisitedConformances &visited, Label label) {
    // In Minimal style, use single quote so this dump can appear in
    // double-quoted fields without escaping.
    char quote = Writer.quote();
    if (style == SubstitutionMap::DumpStyle::Minimal)
      Writer.setQuote('\'');
    SWIFT_DEFER { Writer.setQuote(quote); };

    auto genericSig = map.getGenericSignature();
    printHead("substitution_map", ASTNodeColor, label);
    SWIFT_DEFER { printFoot(); };

    if (genericSig.isNull()) {
      printFlag("null_generic_signature");
      return;
    }

    // We don't need to print the human-readable signature for parsable outputs
    // because the parameters and requirements printed below contain all the
    // same information.
    if (!Writer.isParsable()) {
      printFieldRaw([&](raw_ostream &out) { genericSig->print(out); },
                    Label::always("generic_signature"));
    }

    auto genericParams = genericSig.getGenericParams();
    auto replacementTypes =
    static_cast<const SubstitutionMap &>(map).getReplacementTypes();
    printList(indices(genericParams), [&](unsigned i, Label label) {
      if (style == SubstitutionMap::DumpStyle::Minimal) {
        printFieldRaw([&](raw_ostream &out) {
          genericParams[i]->print(out);
          out << " -> ";
          out << replacementTypes[i];
        }, label);
      } else {
        printRecArbitrary([&](Label label) {
          printHead("substitution", ASTNodeColor, label);
          if (Writer.isParsable()) {
            printTypeField(genericParams[i], Label::always("generic_param"));
            printTypeField(replacementTypes[i],
                           Label::always("replacement_type"));
          } else {
            printFieldRaw([&](raw_ostream &out) {
              genericParams[i]->print(out);
              out << " -> ";
            }, Label::optional(""));
            printRec(replacementTypes[i], label);
          }
          printFoot();
        }, Label::optional("replacement"));
      }
    }, Label::always("substitutions"));

    // A minimal dump doesn't need the details about the conformances, a lot of
    // that info can be inferred from the signature.
    if (style == SubstitutionMap::DumpStyle::Minimal)
      return;

    auto conformances = map.getConformances();
    printList(genericSig.getRequirements(), [&](const auto &req, Label label) {
      if (req.getKind() != RequirementKind::Conformance)
        return;

      printRecArbitrary([&](Label label) {
        printHead("conformance", ASTNodeColor, label);
        printTypeField(req.getFirstType(), Label::always("type"));
        printRec(conformances.front(), visited, Label::optional("conformance"));
        printFoot();
      }, label);
      conformances = conformances.slice(1);
    }, Label::optional("reqs"));
  }
};

void PrintBase::printRec(SubstitutionMap map, VisitedConformances &visited,
                         Label label) {
  printRecArbitrary(
      [&](Label label) {
        PrintConformance(Writer, MemberLoading)
            .visitSubstitutionMap(map, SubstitutionMap::DumpStyle::Full,
                                  visited, label);
      },
      label);
}

void PrintBase::printRec(const ProtocolConformanceRef &ref,
                         VisitedConformances &visited, Label label) {
  printRecArbitrary(
      [&](Label label) {
        PrintConformance(Writer, MemberLoading)
            .visitProtocolConformanceRef(ref, visited, label);
      },
      label);
}

void PrintBase::printRec(const ProtocolConformance *conformance,
                         VisitedConformances &visited, Label label) {
  printRecArbitrary(
      [&](Label label) {
        PrintConformance(Writer, MemberLoading)
            .visitProtocolConformance(conformance, visited, label);
      },
      label);
}

} // end anonymous namespace

void ProtocolConformanceRef::dump() const {
  dump(llvm::errs());
  llvm::errs() << '\n';
}

void ProtocolConformanceRef::dump(llvm::raw_ostream &out, unsigned indent,
                                  bool details) const {
  llvm::SmallPtrSet<const ProtocolConformance *, 8> visited;
  if (!details && isConcrete())
    visited.insert(getConcrete());

  DefaultWriter writer(out, indent);
  PrintConformance(writer).visitProtocolConformanceRef(*this, visited,
                                                       Label::optional(""));
}

void ProtocolConformanceRef::print(llvm::raw_ostream &out) const {
  llvm::SmallPtrSet<const ProtocolConformance *, 8> visited;
  DefaultWriter writer(out, /*indent=*/ 0);
  PrintConformance(writer).visitProtocolConformanceRef(*this, visited,
                                                       Label::optional(""));
}

void ProtocolConformance::dump() const {
  auto &out = llvm::errs();
  dump(out);
  out << '\n';
}

void ProtocolConformance::dump(llvm::raw_ostream &out, unsigned indent) const {
  llvm::SmallPtrSet<const ProtocolConformance *, 8> visited;
  DefaultWriter writer(out, indent);
  PrintConformance(writer).visitProtocolConformance(this, visited,
                                                    Label::optional(""));
}

void PackConformance::dump(llvm::raw_ostream &out, unsigned indent) const {
  llvm::SmallPtrSet<const ProtocolConformance *, 8> visited;
  DefaultWriter writer(out, indent);
  PrintConformance(writer).visitPackConformance(this, visited,
                                                Label::optional(""));
}

void SubstitutionMap::dump(llvm::raw_ostream &out, DumpStyle style,
                           unsigned indent) const {
  llvm::SmallPtrSet<const ProtocolConformance *, 8> visited;
  DefaultWriter writer(out, indent);
  PrintConformance(writer).visitSubstitutionMap(*this, style, visited,
                                                Label::optional(""));
}

void SubstitutionMap::dump() const {
  dump(llvm::errs());
  llvm::errs() << "\n";
}

//===----------------------------------------------------------------------===//
// Dumping for Types.
//===----------------------------------------------------------------------===//

namespace {
  class PrintType : public TypeVisitor<PrintType, void, Label>,
                    public PrintBase {
    void printCommon(StringRef name, Label label) {
      printHead(name, TypeColor, label);
    }

    void dumpParameterFlags(ParameterTypeFlags paramFlags) {
      printFlag(paramFlags.isVariadic(), "vararg");
      printFlag(paramFlags.isAutoClosure(), "autoclosure");
      printFlag(paramFlags.isNonEphemeral(), "nonEphemeral");
      printFlag(paramFlags.isCompileTimeLiteral(), "compileTimeLiteral");
      printFlag(paramFlags.isConstValue(), "constValue");
      printFlag(getDumpString(paramFlags.getValueOwnership()));
    }

  public:
    using PrintBase::PrintBase;

#define TRIVIAL_TYPE_PRINTER(Class,Name)                        \
    void visit##Class##Type(Class##Type *T, Label label) {  \
      printCommon(#Name "_type", label); printFoot();           \
    }

    void visitErrorType(ErrorType *T, Label label) {
      printCommon("error_type", label);
      if (auto originalType = T->getOriginalType())
        printRec(originalType, Label::always("original_type"));
      printFoot();
    }

    TRIVIAL_TYPE_PRINTER(Unresolved, unresolved)

    void visitPlaceholderType(PlaceholderType *T, Label label) {
      printCommon("placeholder_type", label);
      auto originator = T->getOriginator();
      if (auto *typeVar = originator.dyn_cast<TypeVariableType *>()) {
        printRec(typeVar, Label::always("type_variable"));
      } else if (auto *VD = originator.dyn_cast<VarDecl *>()) {
        printFieldQuotedRaw([&](raw_ostream &OS) { VD->dumpRef(OS); },
                            Label::optional("originating_var"), DeclColor);
      } else if (originator.is<ErrorExpr *>()) {
        printFlag("error_expr");
      } else if (auto *DMT = originator.dyn_cast<DependentMemberType *>()) {
        printRec(DMT, Label::always("dependent_member_type"));
      } else if (originator.is<TypeRepr *>()) {
        printFlag("type_repr");
      } else {
        assert(false && "unknown originator");
      }
      printFoot();
    }

    void visitBuiltinIntegerType(BuiltinIntegerType *T, Label label) {
      printCommon("builtin_integer_type", label);
      if (T->isFixedWidth())
        printField(T->getFixedWidth(), Label::always("bit_width"));
      else
        printFlag("word_sized");
      printFoot();
    }

    void visitBuiltinFloatType(BuiltinFloatType *T, Label label) {
      printCommon("builtin_float_type", label);
      printField(T->getBitWidth(), Label::always("bit_width"));
      printFoot();
    }

    TRIVIAL_TYPE_PRINTER(BuiltinIntegerLiteral, builtin_integer_literal)
    TRIVIAL_TYPE_PRINTER(BuiltinJob, builtin_job)
    TRIVIAL_TYPE_PRINTER(BuiltinExecutor, builtin_executor_ref)
    TRIVIAL_TYPE_PRINTER(BuiltinDefaultActorStorage, builtin_default_actor_storage)
    TRIVIAL_TYPE_PRINTER(BuiltinNonDefaultDistributedActorStorage, builtin_non_default_distributed_actor_storage)
    TRIVIAL_TYPE_PRINTER(BuiltinPackIndex, builtin_pack_index)
    TRIVIAL_TYPE_PRINTER(BuiltinRawPointer, builtin_raw_pointer)
    TRIVIAL_TYPE_PRINTER(BuiltinRawUnsafeContinuation, builtin_raw_unsafe_continuation)
    TRIVIAL_TYPE_PRINTER(BuiltinNativeObject, builtin_native_object)
    TRIVIAL_TYPE_PRINTER(BuiltinBridgeObject, builtin_bridge_object)
    TRIVIAL_TYPE_PRINTER(BuiltinUnsafeValueBuffer, builtin_unsafe_value_buffer)
    TRIVIAL_TYPE_PRINTER(SILToken, sil_token)

    void visitBuiltinVectorType(BuiltinVectorType *T, Label label) {
      printCommon("builtin_vector_type", label);
      printField(T->getNumElements(), Label::always("num_elements"));
      printRec(T->getElementType(), Label::optional("element_type"));
      printFoot();
    }
    
    void visitBuiltinUnboundGenericType(BuiltinUnboundGenericType *T,
                                        Label label) {
      printCommon("builtin_unbound_generic_type", label);
      printField(T->getBuiltinTypeNameString(), Label::always("name"));
      printFoot();
    }
    
    void visitBuiltinFixedArrayType(BuiltinFixedArrayType *T,
                                    Label label) {
      printCommon("builtin_fixed_array_type", label);
      printRec(T->getSize(), Label::optional("size"));
      printRec(T->getElementType(), Label::optional("element_type"));
      printFoot();
    }

    void visitTypeAliasType(TypeAliasType *T, Label label) {
      printCommon("type_alias_type", label);

      printFieldQuoted(T->getDecl()->printRef(), Label::always("decl"));
      if (auto underlying = T->getSinglyDesugaredType()) {
        printRec(underlying, Label::always("underlying"));
      } else {
        // This can't actually happen
        printFlag("unresolved_underlying");
      }

      if (T->getParent())
        printRec(T->getParent(), Label::always("parent"));
      printList(T->getDirectGenericArgs(), [&](auto arg, Label label) {
        printRec(arg, label);
      }, Label::optional("direct_generic_args"));

      printFoot();
    }

    void visitLocatableType(LocatableType *T, Label label) {
      printCommon("locatable_type", label);
      printFieldQuotedRaw(
          [&](raw_ostream &OS) {
            auto &C = T->getASTContext();
            T->getLoc().print(OS, C.SourceMgr);
          },
          Label::always("loc"));
      printRec(T->getSinglyDesugaredType(), Label::always("underlying"));
      printFoot();
    }

    void visitPackType(PackType *T, Label label) {
      printCommon("pack_type", label);

      printField(T->getNumElements(), Label::always("num_elements"));

      printList(T->getElementTypes(), [&](Type elt, Label label) {
        printRec(elt, label);
      }, Label::optional("element_types"));

      printFoot();
    }

    void visitSILPackType(SILPackType *T, Label label) {
      printCommon("sil_pack_type", label);

      printField(T->isElementAddress(), Label::always("element_is_address"));
      printField(T->getNumElements(), Label::always("num_elements"));

      printList(T->getElementTypes(), [&](Type elt, Label label) {
        printRec(elt, label);
      }, Label::optional("element_types"));

      printFoot();
    }

    void visitPackExpansionType(PackExpansionType *T, Label label) {
      printCommon("pack_expansion_type", label);
      printRec(T->getPatternType(), Label::always("pattern"));
      printRec(T->getCountType(), Label::always("count"));
      printFoot();
    }

    void visitPackElementType(PackElementType *T, Label label) {
      printCommon("element_type", label);

      printField(T->getLevel(), Label::always("level"));

      printRec(T->getPackType(), Label::always("pack"));

      printFoot();
    }

    void visitTupleType(TupleType *T, Label label) {
      printCommon("tuple_type", label);

      printField(T->getNumElements(), Label::always("num_elements"));

      printList(T->getElements(), [&](const auto &elt, Label label) {
        printRecArbitrary([&](Label label) {
          printHead("tuple_type_elt", FieldLabelColor, label);
          if (elt.hasName())
            printFieldQuoted(elt.getName().str(), Label::always("name"));
          printRec(elt.getType(), Label::optional("type"));
          printFoot();
        }, label);
      }, Label::optional("elements"));

      printFoot();
    }

#define REF_STORAGE(Name, name, ...) \
    void visit##Name##StorageType(Name##StorageType *T, Label label) { \
      printCommon(#name "_storage_type", label); \
      printRec(T->getReferentType(), Label::optional("referent_type")); \
      printFoot(); \
    }
#include "swift/AST/ReferenceStorage.def"

#define VISIT_NOMINAL_TYPE(TypeClass, Name)                \
    void visit##TypeClass(TypeClass *T, Label label) { \
      printCommon(#Name, label);                           \
                                                           \
      printFieldQuoted(T->getDecl()->printRef(), Label::always("decl"));  \
      printFlag(T->getDecl()->hasClangNode(), "foreign");  \
                                                           \
      if (T->getParent())                                  \
        printRec(T->getParent(), Label::always("parent")); \
                                                           \
      printFoot();                                         \
    }

#define VISIT_BINDABLE_NOMINAL_TYPE(TypeClass, Name)       \
    VISIT_NOMINAL_TYPE(TypeClass, Name)                    \
    void visitBoundGeneric##TypeClass(                     \
        BoundGeneric##TypeClass *T, Label label) {         \
      printCommon("bound_generic_" #Name, label);          \
      printFieldQuoted(T->getDecl()->printRef(), Label::always("decl"));  \
      printFlag(T->getDecl()->hasClangNode(), "foreign");  \
      if (T->getParent())                                  \
        printRec(T->getParent(), Label::always("parent")); \
      printList(T->getGenericArgs(), [&](auto arg, Label label) {  \
        printRec(arg, label);                              \
      }, Label::optional("generic_args"));                 \
      printFoot();                                         \
    }

    VISIT_BINDABLE_NOMINAL_TYPE(EnumType, enum_type)
    VISIT_BINDABLE_NOMINAL_TYPE(StructType, struct_type)
    VISIT_BINDABLE_NOMINAL_TYPE(ClassType, class_type)
    VISIT_NOMINAL_TYPE(ProtocolType, protocol_type)

#undef VISIT_BINDABLE_NOMINAL_TYPE
#undef VISIT_NOMINAL_TYPE

    void visitBuiltinTupleType(BuiltinTupleType *T, Label label) {
      printCommon("builtin_tuple_type", label);
      printFieldQuoted(T->getDecl()->printRef(), Label::always("decl"));
      printFoot();
    }

    void visitMetatypeType(MetatypeType *T, Label label) {
      printCommon("metatype_type", label);

      if (T->hasRepresentation())
        printFlag(getDumpString(T->getRepresentation()));

      printRec(T->getInstanceType(), Label::optional("instance_type"));

      printFoot();
    }

    void visitExistentialMetatypeType(ExistentialMetatypeType *T,
                                      Label label) {
      printCommon("existential_metatype_type", label);

      if (T->hasRepresentation())
        printFlag(getDumpString(T->getRepresentation()));

      printRec(T->getInstanceType(), Label::optional("instance_type"));

      printFoot();
    }

    void visitModuleType(ModuleType *T, Label label) {
      printCommon("module_type", label);
      printDeclName(T->getModule(), Label::always("module"));
      printFlag(T->getModule()->isNonSwiftModule(), "foreign");
      printFoot();
    }

    void visitDynamicSelfType(DynamicSelfType *T, Label label) {
      printCommon("dynamic_self_type", label);
      printRec(T->getSelfType(), Label::optional("self_type"));
      printFoot();
    }
    
    void printArchetypeCommon(ArchetypeType *T,
                              StringRef className,
                              Label label) {
      printCommon(className, label);

      printField(static_cast<void *>(T), Label::always("address"));
      printFlag(T->requiresClass(), "class");
      if (auto layout = T->getLayoutConstraint()) {
        printFieldRaw([&](raw_ostream &OS) {
          layout->print(OS);
        }, Label::always("layout"));
      }
      for (auto proto : T->getConformsTo())
        printFieldQuoted(proto->printRef(), Label::always("conforms_to"));
    }

    void printArchetypeCommonRec(ArchetypeType *T) {
      printRec(T->getInterfaceType(), Label::always("interface_type"));
      if (auto superclass = T->getSuperclass())
        printRec(superclass, Label::always("superclass"));
    }

    void visitPrimaryArchetypeType(PrimaryArchetypeType *T, Label label) {
      printArchetypeCommon(T, "primary_archetype_type", label);

      printFieldQuoted(T->getFullName(), Label::always("name"));

      printArchetypeCommonRec(T);

      printFoot();
    }
    void visitExistentialArchetypeType(ExistentialArchetypeType *T, Label label) {
      printArchetypeCommon(T, "existential_archetype_type", label);

      auto *env = T->getGenericEnvironment();
      printFieldQuoted(env->getOpenedExistentialUUID(),
                       Label::always("opened_existential_id"));

      printArchetypeCommonRec(T);
      printRec(env->getOpenedExistentialType(),
               Label::always("opened_existential"));
      if (auto subMap = env->getOuterSubstitutions())
        printRec(subMap, Label::always("substitutions"));

      printFoot();
    }
    void visitOpaqueTypeArchetypeType(OpaqueTypeArchetypeType *T,
                                      Label label) {
      printArchetypeCommon(T, "opaque_type", label);

      printFieldQuoted(T->getDecl()->getNamingDecl()->printRef(),
                       Label::always("decl"));

      printArchetypeCommonRec(T);
      if (!T->getSubstitutions().empty()) {
        printRec(T->getSubstitutions(), Label::optional("substitutions"));
      }

      printFoot();
    }
    void visitPackArchetypeType(PackArchetypeType *T, Label label) {
      printArchetypeCommon(T, "pack_archetype_type", label);
      printFieldQuoted(T->getFullName(), Label::always("name"));
      printArchetypeCommonRec(T);
      printFoot();
    }
    void visitElementArchetypeType(ElementArchetypeType *T, Label label) {
      printArchetypeCommon(T, "element_archetype_type", label);
      printFieldQuoted(T->getOpenedElementID(),
                       Label::always("opened_element_id"));
      printFoot();
    }

    void visitGenericTypeParamType(GenericTypeParamType *T, Label label) {
      printCommon("generic_type_param_type", label);
      printField(T->getDepth(), Label::always("depth"));
      printField(T->getIndex(), Label::always("index"));
      if (!T->isCanonical())
        printFieldQuoted(T->getName(), Label::always("name"));

      switch (T->getParamKind()) {
      case GenericTypeParamKind::Type:
        printField((StringRef)"type", Label::always("param_kind"));
        break;
      case GenericTypeParamKind::Pack:
        printField((StringRef)"pack", Label::always("param_kind"));
        break;
      case GenericTypeParamKind::Value:
        printField((StringRef)"value", Label::always("param_kind"));
        printRec(T->getValueType(), Label::always("value_type"));
      }

      printFoot();
    }

    void visitDependentMemberType(DependentMemberType *T, Label label) {
      printCommon("dependent_member_type", label);

      if (auto assocType = T->getAssocType()) {
        printFieldQuoted(assocType->printRef(), Label::always("assoc_type"));
      } else {
        printFieldQuoted(T->getName(), Label::always("name"));
      }

      printRec(T->getBase(), Label::always("base"));

      printFoot();
    }

    void printAnyFunctionParamsRec(ArrayRef<AnyFunctionType::Param> params,
                                   Label label) {
      printRecArbitrary([&](Label label) {
        printHead("function_params", FieldLabelColor, label);

        printField(params.size(), Label::always("num_params"));
        printList(params, [&](const auto &param, Label label) {
          printRecArbitrary([&](Label label) {
            printHead("param", FieldLabelColor, label);

            if (param.hasLabel())
              printFieldQuoted(param.getLabel().str(), Label::always("name"));
            if (param.hasInternalLabel())
              printFieldQuoted(param.getInternalLabel().str(),
                               Label::always("internal_name"));
            dumpParameterFlags(param.getParameterFlags());

            printRec(param.getPlainType(), Label::optional("plain_type"));

            printFoot();
          }, label);
        }, Label::optional("params"));
        printFoot();
      }, label);
    }

    void printClangTypeRec(const ClangTypeInfo &info, const ASTContext &ctx,
                           Label label) {
      // [TODO: Improve-Clang-type-printing]
      if (!info.empty()) {
        printRecArbitrary([&](Label label) {
          printHead("clang_type", ASTNodeColor, label);
          printNameRaw([&](raw_ostream &OS) {
            auto &clangCtx = ctx.getClangModuleLoader()->getClangASTContext();
            info.dump(OS, clangCtx);
          }, Label::optional("clang_type_info"));
          printFoot();
        }, label);
      }
    }

    void printAnyFunctionTypeCommonRec(AnyFunctionType *T, Label label,
                                       StringRef name) {
      printCommon(name, label);

      if (T->hasExtInfo()) {
        SILFunctionType::Representation representation =
            T->getExtInfo().getSILRepresentation();

        if (representation != SILFunctionType::Representation::Thick) {
          printField(representation, Label::always("representation"));
        }
        printFlag(!T->isNoEscape(), "escaping");
        printFlag(T->isSendable(), "Sendable");
        printFlag(T->isAsync(), "async");
        printFlag(T->isThrowing(), "throws");
        printFlag(T->hasSendingResult(), "sending_result");
        if (T->isDifferentiable()) {
          switch (T->getDifferentiabilityKind()) {
          default:
            llvm_unreachable("unexpected differentiability kind");
          case DifferentiabilityKind::Reverse:
            printFlag("@differentiable(reverse)");
            break;
          case DifferentiabilityKind::Forward:
            printFlag("@differentiable(_forward)");
            break;
          case DifferentiabilityKind::Linear:
            printFlag("@differentiable(_linear)");
            break;
          case DifferentiabilityKind::Normal:
            printFlag("@differentiable");
            break;
          }
        }
        auto isolation = T->getIsolation();
        switch (isolation.getKind()) {
        case FunctionTypeIsolation::Kind::NonIsolated:
        case FunctionTypeIsolation::Kind::Parameter:
          break;
        case FunctionTypeIsolation::Kind::GlobalActor:
          printRec(isolation.getGlobalActorType(),
                   Label::always("global_actor"));
          break;
        case FunctionTypeIsolation::Kind::Erased:
          printFlag("@isolated(any)");
          break;
        case FunctionTypeIsolation::Kind::NonIsolatedCaller:
          printFlag("nonisolated(nonsending)");
          break;
        }
      }
      if (Type globalActor = T->getGlobalActor()) {
        printFieldQuoted(globalActor.getString(), Label::always("global_actor"));
      }

      printClangTypeRec(T->getClangTypeInfo(), T->getASTContext(),
                        Label::optional("clang_type_info"));
      printAnyFunctionParamsRec(T->getParams(), Label::always("input"));
      printRec(T->getResult(), Label::always("output"));
      if (Type thrownError = T->getThrownError()) {
        printRec(thrownError, Label::always("thrown_error"));
      }
    }

    void visitFunctionType(FunctionType *T, Label label) {
      printAnyFunctionTypeCommonRec(T, label, "function_type");
      printFoot();
    }

    void visitGenericFunctionType(GenericFunctionType *T, Label label) {
      printAnyFunctionTypeCommonRec(T, label, "generic_function_type");
      // FIXME: generic signature dumping needs improvement
      printRecArbitrary([&](Label label) {
        printHead("generic_sig", TypeColor, label);
        printFieldQuoted(T->getGenericSignature()->getAsString(),
                         Label::optional(""));
        printFoot();
      }, Label::optional("generic_sig"));
      printFoot();
    }

    void visitSILFunctionType(SILFunctionType *T, Label label) {
      printCommon("sil_function_type", label);
      printFieldQuoted(T->getString(), Label::always("type"));

      printList(T->getParameters(), [&](auto param, Label label) {
        printRec(param.getInterfaceType(), label);
      }, Label::always("input"));
      printList(T->getYields(), [&](auto yield, Label label) {
        printRec(yield.getInterfaceType(), label);
      }, Label::always("yield"));
      printList(T->getResults(), [&](auto result, Label label) {
        printRec(result.getInterfaceType(), label);
      }, Label::always("result"));
      if (auto error  = T->getOptionalErrorResult()) {
        printRec(error->getInterfaceType(), Label::always("error"));
      }
      printRec(T->getPatternSubstitutions(),
               Label::optional("pattern_substitutions"));
      printRec(T->getInvocationSubstitutions(),
               Label::optional("invocation_substitutions"));
      printClangTypeRec(T->getClangTypeInfo(), T->getASTContext(),
                        Label::optional("clang_type_info"));

      printFoot();
    }

    void visitSILBlockStorageType(SILBlockStorageType *T, Label label) {
      printCommon("sil_block_storage_type", label);
      printRec(T->getCaptureType(), Label::optional("capture_type"));
      printFoot();
    }

    void visitSILMoveOnlyWrappedType(SILMoveOnlyWrappedType *T,
                                     Label label) {
      printCommon("sil_move_only_type", label);
      printRec(T->getInnerType(), Label::optional("inner_type"));
      printFoot();
    }

    void visitSILBoxType(SILBoxType *T, Label label) {
      printCommon("sil_box_type", label);
      // FIXME: Print the structure of the type.
      printFieldQuoted(T->getString(), Label::always("type"));
      printFoot();
    }

    void visitArraySliceType(ArraySliceType *T, Label label) {
      printCommon("array_slice_type", label);
      printRec(T->getBaseType(), Label::optional("base_type"));
      printFoot();
    }

    void visitInlineArrayType(InlineArrayType *T, Label label) {
      printCommon("inline_array_type", label);
      printRec(T->getCountType(), Label::always("count"));
      printRec(T->getElementType(), Label::always("element"));
      printFoot();
    }

    void visitOptionalType(OptionalType *T, Label label) {
      printCommon("optional_type", label);
      printRec(T->getBaseType(), Label::optional("base_type"));
      printFoot();
    }

    void visitDictionaryType(DictionaryType *T, Label label) {
      printCommon("dictionary_type", label);
      printRec(T->getKeyType(), Label::always("key"));
      printRec(T->getValueType(), Label::always("value"));
      printFoot();
    }

    void visitVariadicSequenceType(VariadicSequenceType *T, Label label) {
      printCommon("variadic_sequence_type", label);
      printRec(T->getBaseType(), Label::optional("base_type"));
      printFoot();
    }

    void visitProtocolCompositionType(ProtocolCompositionType *T,
                                      Label label) {

      printCommon("protocol_composition_type", label);

      printFlag(T->hasExplicitAnyObject(), "any_object");

      for (auto ip : T->getInverses()) {
        switch (ip) {
        case InvertibleProtocolKind::Copyable:
          printFlag("inverse_copyable");
          break;
        case InvertibleProtocolKind::Escapable:
          printFlag("inverse_escapable");
          break;
        }
      }

      printList(T->getMembers(), [&](auto proto, Label label) {
        printRec(proto, label);
      }, Label::optional("members"));

      printFoot();
    }

    void visitParameterizedProtocolType(ParameterizedProtocolType *T,
                                        Label label) {
      printCommon("parameterized_protocol_type", label);
      printRec(T->getBaseType(), Label::always("base"));
      printList(T->getArgs(), [&](auto arg, Label label) {
        printRec(arg, label);
      }, Label::optional("args"));
      printFoot();
    }

    void visitExistentialType(ExistentialType *T,
                              Label label) {
      printCommon("existential_type", label);
      printRec(T->getConstraintType(), Label::optional("constraint_type"));
      printFoot();
    }

    void visitLValueType(LValueType *T, Label label) {
      printCommon("lvalue_type", label);
      printRec(T->getObjectType(), Label::optional("object_type"));
      printFoot();
    }

    void visitInOutType(InOutType *T, Label label) {
      printCommon("inout_type", label);
      printRec(T->getObjectType(), Label::optional("object_type"));
      printFoot();
    }

    void visitUnboundGenericType(UnboundGenericType *T, Label label) {
      printCommon("unbound_generic_type", label);
      printFieldQuoted(T->getDecl()->printRef(), Label::always("decl"));
      if (T->getParent())
        printRec(T->getParent(), Label::always("parent"));
      printFoot();
    }

    void visitTypeVariableType(TypeVariableType *T, Label label) {
      printCommon("type_variable_type", label);
      printField(T->getID(), Label::always("id"));
      printFoot();
    }

    void visitErrorUnionType(ErrorUnionType *T, Label label) {
      printCommon("error_union_type", label);
      printList(T->getTerms(), [&](auto term, Label label) {
        printRec(term, label);
      }, Label::optional("terms"));
      printFoot();
    }

    void visitIntegerType(IntegerType *T, Label label) {
      printCommon("integer_type", label);
      printFlag(T->isNegative(), "is_negative");
      printFieldQuoted(T->getValue(), Label::always("value"), LiteralValueColor);
      printFieldQuoted(T->getDigitsText(), Label::always("text"), IdentifierColor);
      printFoot();
    }

#undef TRIVIAL_TYPE_PRINTER
  };

  void PrintBase::printRec(Type type, Label label) {
    printRecArbitrary([&](Label label) {
      if (type.isNull()) {
        printHead("<null type>", DeclColor, label);
        printFoot();
      } else {
        PrintType(Writer, MemberLoading, GetTypeOfExpr, GetTypeOfTypeRepr,
                  GetTypeOfKeyPathComponent)
            .visit(type, label);
      }
    }, label);
  }
} // end anonymous namespace

void Type::dump() const {
  dump(llvm::errs());
}

void Type::dump(raw_ostream &os, unsigned indent) const {
  DefaultWriter writer(os, indent);
  PrintType(writer).visit(*this, Label::optional(""));
  os << "\n";
}

void TypeBase::dump() const {
  // Make sure to print type variables.
  Type(const_cast<TypeBase *>(this)).dump();
}

void TypeBase::dump(raw_ostream &os, unsigned indent) const {
  Type(const_cast<TypeBase *>(this)).dump(os, indent);
}

void GenericSignatureImpl::dump() const {
  GenericSignature(const_cast<GenericSignatureImpl *>(this)).dump();
}

void GenericEnvironment::dump(raw_ostream &os) const {
  os << "Generic environment:\n";
  for (auto gp : getGenericParams()) {
    gp->dump(os);
    mapTypeIntoContext(gp)->dump(os);
  }
  os << "Generic parameters:\n";
  for (auto paramTy : getGenericParams())
    paramTy->dump(os);
}

void GenericEnvironment::dump() const {
  dump(llvm::errs());
}

StringRef swift::getAccessorKindString(AccessorKind value) {
  switch (value) {
#define ACCESSOR(ID, KEYWORD)
#define SINGLETON_ACCESSOR(ID, KEYWORD) \
  case AccessorKind::ID: return #KEYWORD;
#include "swift/AST/AccessorKinds.def"
  }

  llvm_unreachable("Unhandled AccessorKind in switch.");
}

void StableSerializationPath::dump() const {
  dump(llvm::errs());
}

static StringRef getExternalPathComponentKindString(
                  StableSerializationPath::ExternalPath::ComponentKind kind) {
  switch (kind) {
#define CASE(ID, STRING) \
  case StableSerializationPath::ExternalPath::ID: return STRING;
  CASE(Record, "record")
  CASE(Enum, "enum")
  CASE(Namespace, "namespace")
  CASE(Typedef, "typedef")
  CASE(TypedefAnonDecl, "anonymous tag")
  CASE(ObjCInterface, "@interface")
  CASE(ObjCProtocol, "@protocol")
#undef CASE
  }
  llvm_unreachable("bad kind");
}

void StableSerializationPath::dump(llvm::raw_ostream &os) const {
  if (isSwiftDecl()) {
    os << "clang decl of:\n";
    getSwiftDecl()->dump(os, 2);
  } else {
    auto &path = getExternalPath();
    using ExternalPath = StableSerializationPath::ExternalPath;
    os << "external path: ";
    size_t index = 0;
    for (auto &entry : path.Path) {
      if (index++) os << " -> ";
      os << getExternalPathComponentKindString(entry.first);
      if (ExternalPath::requiresIdentifier(entry.first))  {
        os << "(" << entry.second << ")";
      }
    }
    os << "\n";
  }
}

void RequirementRepr::dump() const {
  print(llvm::errs());
  llvm::errs() << "\n";
}

void GenericParamList::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

void LayoutConstraint::dump() const {
  if (!*this) {
    llvm::errs() << "(null)\n";
    return;
  }
  getPointer()->print(llvm::errs());
}

void GenericSignature::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

void Requirement::dump() const {
  dump(llvm::errs());
  llvm::errs() << '\n';
}
void Requirement::dump(raw_ostream &out) const {
  DefaultWriter writer(out, /*indent=*/ 0);
  PrintBase(writer).visitRequirement(*this, Label::optional(""));
}

void SILParameterInfo::dump() const {
  // TODO: Fix LifetimeDependenceInfo printing here.
  print(llvm::errs());
  llvm::errs() << '\n';
}

void SILResultInfo::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

void InheritedEntry::dump(llvm::raw_ostream &os) const {
  if (isPreconcurrency())
    os << "@preconcurrency ";
  if (isRetroactive())
    os << "@retroactive ";
  if (isUnchecked())
    os << "@unchecked ";
  if (getExplicitSafety() != ExplicitSafety::Unspecified)
    os << '@' << getDumpString(getExplicitSafety()) << ' ';
  if (isSuppressed())
    os << "~";
  getType().print(os);
}

void InheritedEntry::dump() const { dump(llvm::errs()); }
