//===--- TypeReconstruction.cpp -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/IDE/Utils.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Types.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Demangling/Demangle.h"
#include "swift/Demangling/ManglingMacros.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Strings.h"

#include <cstdio>
#include <cstdarg>
#include <mutex> // std::once

using namespace swift;

// FIXME: replace with std::string and StringRef as appropriate to each case.
using ConstString = const std::string;

static std::string stringWithFormat(const char *fmt_str, ...) {
  int final_n, n = ((int)strlen(fmt_str)) * 2;
  std::string str;
  std::unique_ptr<char[]> formatted;
  va_list ap;
  while (1) {
    formatted.reset(new char[n]);
    strcpy(&formatted[0], fmt_str);
    va_start(ap, fmt_str);
    final_n = vsnprintf(&formatted[0], n, fmt_str, ap);
    va_end(ap);
    if (final_n < 0 || final_n >= n)
      n += abs(final_n - n + 1);
    else
      break;
  }
  return std::string(formatted.get());
}

static bool
CompareFunctionTypes(const AnyFunctionType *f, const AnyFunctionType *g,
                     Optional<std::vector<StringRef>> fLabels = None,
                     Optional<std::vector<StringRef>> gLabels = None,
                     bool *input_matches = nullptr,
                     bool *output_matches = nullptr);

static Optional<ClangTypeKind>
GetClangTypeKindFromSwiftKind(DeclKind decl_kind) {
  Optional<ClangTypeKind> clangTypeKind;
  switch (decl_kind) {
  case DeclKind::Class:
    return ClangTypeKind::ObjCClass;
  case DeclKind::Protocol:
    return ClangTypeKind::ObjCProtocol;
  case DeclKind::TypeAlias:
    return ClangTypeKind::Typedef;
  case DeclKind::Struct:
    return ClangTypeKind::Tag;
  default:
    return None;
  }
}

class DeclsLookupSource {
public:
  using ValueDecls = SmallVectorImpl<ValueDecl *>;

private:
  class VisibleDeclsConsumer : public VisibleDeclConsumer {
  private:
    std::vector<ValueDecl *> m_decls;

  public:
    void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) override {
      m_decls.push_back(VD);
    }
    ~VisibleDeclsConsumer() override = default;
    explicit operator bool() { return !m_decls.empty(); }

    decltype(m_decls)::const_iterator begin() { return m_decls.begin(); }

    decltype(m_decls)::const_iterator end() { return m_decls.end(); }
  };

  bool lookupQualified(ModuleDecl *entry, DeclBaseName name, NLOptions options,
                       LazyResolver *typeResolver, ValueDecls &decls) {
    if (!entry)
      return false;
    size_t decls_size = decls.size();
    entry->lookupQualified(entry, name, options, decls);
    return decls.size() > decls_size;
  }

  bool lookupValue(ModuleDecl *entry, DeclBaseName name,
                   ModuleDecl::AccessPathTy accessPath, NLKind lookupKind,
                   ValueDecls &decls) {
    if (!entry)
      return false;
    size_t decls_size = decls.size();
    entry->lookupValue(accessPath, name, lookupKind, decls);
    return decls.size() > decls_size;
  }

public:
  enum class LookupKind {
    SwiftModule,
    ClangImporter,
    Decl,
    Extension,
    Invalid
  };

  using PrivateDeclIdentifier = Optional<std::string>;

  static DeclsLookupSource
  GetDeclsLookupSource(ASTContext &ast, ConstString module_name,
                       bool allow_clang_importer = true) {
    assert(!module_name.empty());
    static ConstString g_ObjectiveCModule(MANGLING_MODULE_OBJC);
    static ConstString g_BuiltinModule(BUILTIN_NAME);
    static ConstString g_CModule(MANGLING_MODULE_CLANG_IMPORTER);
    if (allow_clang_importer) {
      if (module_name == g_ObjectiveCModule || module_name == g_CModule)
        return DeclsLookupSource(&ast, module_name);
    }

    ModuleDecl *module = module_name == g_BuiltinModule
                             ? ast.TheBuiltinModule
                             : ast.getModuleByName(module_name);
    if (module == nullptr)
      return DeclsLookupSource();
    return DeclsLookupSource(module);
  }

  static DeclsLookupSource GetDeclsLookupSource(NominalTypeDecl *decl) {
    assert(decl);
    return DeclsLookupSource(decl);
  }

  static DeclsLookupSource GetDeclsLookupSource(DeclsLookupSource source,
                                                NominalTypeDecl *decl) {
    assert(source._type == LookupKind::SwiftModule);
    assert(source._module);
    assert(decl);
    return DeclsLookupSource(source._module, decl);
  }

  void lookupByMangledName(DeclBaseName name, DeclKind decl_kind,
                           ValueDecls &result) {
    if (_type == LookupKind::ClangImporter) {
      ASTContext *ast_ctx = _clang_crawler._ast;
      if (ast_ctx) {
        ClangImporter *swift_clang_importer =
            (ClangImporter *)ast_ctx->getClangModuleLoader();
        if (!swift_clang_importer)
          return;

        Optional<ClangTypeKind> clangTypeKind =
            GetClangTypeKindFromSwiftKind(decl_kind);
        if (clangTypeKind) {
          swift_clang_importer->lookupTypeDecl(name.getIdentifier().str(),
                                               clangTypeKind.getValue(),
                                               [&](TypeDecl *type_decl) {
            result.push_back(type_decl);
          });
          return;
        } else {
          VisibleDeclsConsumer consumer;
          swift_clang_importer->lookupValue(name, consumer);
          auto iter = consumer.begin(), end = consumer.end();
          while (iter != end) {
            result.push_back(*iter);
            iter++;
          }
          return;
        }
      }
    } else if (_type == LookupKind::SwiftModule) {
      lookupQualified(_module, name, NLOptions(), /*typeResolver*/nullptr,
                      result);
    }
    return;
  }

  void lookupRelatedEntity(StringRef name, StringRef related_entity_kind,
                           DeclKind decl_kind, ValueDecls &result) {
    switch (_type) {
    case LookupKind::ClangImporter: {
      ASTContext *ast_ctx = _clang_crawler._ast;
      if (!ast_ctx)
        return;
      ClangImporter *swift_clang_importer =
          (ClangImporter *)ast_ctx->getClangModuleLoader();
      if (!swift_clang_importer)
        return;
      Optional<ClangTypeKind> clang_kind =
          GetClangTypeKindFromSwiftKind(decl_kind);
      if (!clang_kind)
        return;
      swift_clang_importer->lookupRelatedEntity(name, clang_kind.getValue(),
                                                related_entity_kind,
                                                [&](TypeDecl *found) {
        result.push_back(found);
      });
      return;
    }
    case LookupKind::SwiftModule:
    case LookupKind::Decl:
    case LookupKind::Extension:
      return;
    case LookupKind::Invalid:
      return;
    }
  }

  void lookupValue(ModuleDecl::AccessPathTy path, DeclBaseName name, NLKind kind,
                   ValueDecls &result) {
    if (_type == LookupKind::ClangImporter) {
      ASTContext *ast_ctx = _clang_crawler._ast;
      if (ast_ctx) {
        VisibleDeclsConsumer consumer;
        ClangImporter *swift_clang_importer =
            (ClangImporter *)ast_ctx->getClangModuleLoader();
        if (!swift_clang_importer)
          return;
        swift_clang_importer->lookupValue(name, consumer);
        if (consumer) {
          auto iter = consumer.begin(), end = consumer.end();
          while (iter != end) {
            result.push_back(*iter);
            iter++;
          }
          return;
        }
      }
    } else if (_type == LookupKind::SwiftModule)
      _module->lookupValue(path, name, kind, result);
    else if (_type == LookupKind::Extension) {
      auto results = _extension._decl->lookupDirect(DeclName(name));
      result.append(results.begin(), results.end());
    }
    return;
  }

  void lookupMember(DeclName id, Identifier priv_decl_id, ValueDecls &result) {
    if (_type == LookupKind::Decl)
      return lookupMember(_decl, id, priv_decl_id, result);
    if (_type == LookupKind::SwiftModule)
      return lookupMember(_module, id, priv_decl_id, result);
    if (_type == LookupKind::Extension)
      return lookupMember(_extension._decl, id, priv_decl_id, result);
    return;
  }

  void lookupMember(DeclContext *decl_ctx, DeclName id, Identifier priv_decl_id,
                    ValueDecls &result) {
    if (_type == LookupKind::Decl)
      _decl->getModuleContext()->lookupMember(result, decl_ctx, id,
                                              priv_decl_id);
    else if (_type == LookupKind::SwiftModule)
      _module->lookupMember(result, decl_ctx, id, priv_decl_id);
    else if (_type == LookupKind::Extension)
      _extension._module->lookupMember(result, decl_ctx, id, priv_decl_id);
    return;
  }

  TypeDecl *lookupLocalType(StringRef key) {
    switch (_type) {
    case LookupKind::SwiftModule:
      return _module->lookupLocalType(key);
    case LookupKind::Decl:
      return _decl->getModuleContext()->lookupLocalType(key);
    case LookupKind::Extension:
      return _extension._module->lookupLocalType(key);
    case LookupKind::Invalid:
      return nullptr;
    case LookupKind::ClangImporter:
      return nullptr;
    }

    llvm_unreachable("Unhandled LookupKind in switch.");
  }

  ConstString GetName() const {
    switch (_type) {
    case LookupKind::Invalid:
      return ConstString("Invalid");
    case LookupKind::ClangImporter:
      return ConstString("ClangImporter");
    case LookupKind::SwiftModule:
      return ConstString(_module->getName().get());
    case LookupKind::Decl:
      return ConstString(_decl->getName().get());
    case LookupKind::Extension:
      SmallString<64> builder;
      builder.append("ext ");
      builder.append(_extension._decl->getNameStr());
      builder.append(" in ");
      builder.append(_module->getNameStr());
      return builder.str();
    }

    llvm_unreachable("Unhandled LookupKind in switch.");
  }

  ~DeclsLookupSource() {}

  DeclsLookupSource(const DeclsLookupSource &rhs) : _type(rhs._type) {
    switch (_type) {
    case LookupKind::Invalid:
      break;
    case LookupKind::ClangImporter:
      _clang_crawler._ast = rhs._clang_crawler._ast;
      break;
    case LookupKind::SwiftModule:
      _module = rhs._module;
      break;
    case LookupKind::Decl:
      _decl = rhs._decl;
      _extension._decl = rhs._extension._decl;
      _extension._module = rhs._extension._module;
      break;
    case LookupKind::Extension:
      _extension._decl = rhs._extension._decl;
      _extension._module = rhs._extension._module;
      break;
    }
  }

  DeclsLookupSource &operator=(const DeclsLookupSource &rhs) {
    if (this != &rhs) {
      _type = rhs._type;
      switch (_type) {
      case LookupKind::Invalid:
        break;
      case LookupKind::ClangImporter:
        _clang_crawler._ast = rhs._clang_crawler._ast;
        break;
      case LookupKind::SwiftModule:
        _module = rhs._module;
        break;
      case LookupKind::Decl:
        _decl = rhs._decl;
        _extension._decl = rhs._extension._decl;
        _extension._module = rhs._extension._module;
        break;
      case LookupKind::Extension:
        _extension._decl = rhs._extension._decl;
        _extension._module = rhs._extension._module;
        break;
      }
    }
    return *this;
  }

  void Clear() {
    // no need to explicitly clean either pointer
    _type = LookupKind::Invalid;
  }

  DeclsLookupSource() : _type(LookupKind::Invalid), _module(nullptr) {}

  operator bool() {
    switch (_type) {
    case LookupKind::Invalid:
      return false;
    case LookupKind::ClangImporter:
      return _clang_crawler._ast != nullptr;
    case LookupKind::SwiftModule:
      return _module != nullptr;
    case LookupKind::Decl:
      return _decl != nullptr;
    case LookupKind::Extension:
      return (_extension._decl != nullptr) && (_extension._module != nullptr);
    }

    llvm_unreachable("Unhandled LookupKind in switch.");
  }

  bool IsExtension() {
    return (this->operator bool()) && (_type == LookupKind::Extension);
  }

  NominalTypeDecl *GetExtendedDecl() {
    assert(IsExtension());
    return _extension._decl;
  }

private:
  LookupKind _type;

  union {
    ModuleDecl *_module;
    struct {
      ASTContext *_ast;
    } _clang_crawler;
    NominalTypeDecl *_decl;
    struct {
      ModuleDecl *_module;    // extension in this module
      NominalTypeDecl *_decl; // for this type
    } _extension;
  };

  DeclsLookupSource(ModuleDecl *_m) {
    if (_m) {
      _module = _m;
      _type = LookupKind::SwiftModule;
    } else
      _type = LookupKind::Invalid;
  }

  DeclsLookupSource(ASTContext *_a, ConstString _m) {
    // it is fine for the ASTContext to be null, so don't actually even
    // lldbassert there
    if (_a) {
      _clang_crawler._ast = _a;
      _type = LookupKind::ClangImporter;
    } else
      _type = LookupKind::Invalid;
  }

  DeclsLookupSource(NominalTypeDecl *_d) {
    if (_d) {
      _decl = _d;
      _type = LookupKind::Decl;
    } else
      _type = LookupKind::Invalid;
  }

  DeclsLookupSource(ModuleDecl *_m, NominalTypeDecl *_d) {
    if (_m && _d) {
      _extension._decl = _d;
      _extension._module = _m;
      _type = LookupKind::Extension;
    } else
      _type = LookupKind::Invalid;
  }
};

struct VisitNodeResult {
  DeclsLookupSource _module;
  std::vector<Decl *> _decls;
  std::vector<Type> _types;
  TupleTypeElt _tuple_type_element;
  std::string _error;
  VisitNodeResult()
      : _module(), _decls(), _types(), _tuple_type_element(), _error() {}

  bool HasSingleType() { return _types.size() == 1 && _types.front(); }

  void Clear() {
    _module.Clear();
    _decls.clear();
    _types.clear();
    _tuple_type_element = TupleTypeElt();
    _error.clear();
  }
};

static Identifier
GetIdentifier(ASTContext *ast,
              const DeclsLookupSource::PrivateDeclIdentifier &priv_decl_id) {
  do {
    if (!ast)
      break;
    if (!priv_decl_id.hasValue())
      break;
    return ast->getIdentifier(priv_decl_id.getValue().c_str());
  } while (false);
  return Identifier();
}

static bool FilterDeclsForKind(DeclKind decl_kind,
                               ArrayRef<swift::ValueDecl *> decls,
                               VisitNodeResult &result) {
  for (auto decl : decls) {
    // Note: Clang nodes are filtered ahead of time based on their Clang decl
    // kind.
    if (decl->getKind() != decl_kind && !decl->hasClangNode())
      continue;

    result._decls = {decl};
    Type decl_type;
    if (decl->hasInterfaceType()) {
      decl_type = decl->getInterfaceType();
      MetatypeType *meta_type = decl_type->getAs<MetatypeType>();
      if (meta_type)
        decl_type = meta_type->getInstanceType();
    }
    result._types = {decl_type};
    return true;
  }

  return false;
}

static bool FindFirstNamedDeclWithKind(
    ASTContext *ast, const DeclBaseName &name, DeclKind decl_kind,
    VisitNodeResult &result,
    DeclsLookupSource::PrivateDeclIdentifier priv_decl_id =
        DeclsLookupSource::PrivateDeclIdentifier())

{
  if (!result._decls.empty()) {
    Decl *parent_decl = result._decls.back();
    if (parent_decl) {
      auto nominal_decl = dyn_cast<NominalTypeDecl>(parent_decl);

      if (nominal_decl) {
        DeclsLookupSource lookup(
            DeclsLookupSource::GetDeclsLookupSource(nominal_decl));
        SmallVector<ValueDecl *, 4> decls;
        lookup.lookupMember(name, GetIdentifier(ast, priv_decl_id), decls);

        if (FilterDeclsForKind(decl_kind, decls, result))
          return true;
      }
    }
  } else if (result._module) {
    SmallVector<ValueDecl *, 4> decls;
    if (priv_decl_id)
      result._module.lookupMember(name, GetIdentifier(ast, priv_decl_id),
                                  decls);
    else
      result._module.lookupByMangledName(name, decl_kind, decls);
    if (FilterDeclsForKind(decl_kind, decls, result))
      return true;
  }
  result.Clear();
  result._error = "Generic Error";
  return false;
}

static size_t
FindNamedDecls(ASTContext *ast, const DeclBaseName &name, VisitNodeResult &result,
               DeclsLookupSource::PrivateDeclIdentifier priv_decl_id =
                   DeclsLookupSource::PrivateDeclIdentifier()) {
  if (!result._decls.empty()) {
    Decl *parent_decl = result._decls.back();
    result._decls.clear();
    result._types.clear();
    if (parent_decl) {

      if (auto nominal_decl = dyn_cast<NominalTypeDecl>(parent_decl)) {
        DeclsLookupSource lookup(
            DeclsLookupSource::GetDeclsLookupSource(nominal_decl));
        SmallVector<ValueDecl *, 4> decls;
        lookup.lookupMember(name, GetIdentifier(ast, priv_decl_id), decls);
        if (decls.empty()) {
          result._error = stringWithFormat(
              "no decl found in '%s' (DeclKind=%u)",
              name.userFacingName().str().c_str(),
              nominal_decl->getName().get(), (uint32_t)nominal_decl->getKind());
        } else {
          for (ValueDecl *decl : decls) {
            if (decl->hasInterfaceType()) {
              result._decls.push_back(decl);
              Type decl_type;
              decl_type = decl->getInterfaceType();
              MetatypeType *meta_type = decl_type->getAs<MetatypeType>();
              if (meta_type)
                decl_type = meta_type->getInstanceType();
              result._types.push_back(decl_type);
            }
          }
          return result._types.size();
        }

      } else if (auto FD = dyn_cast<AbstractFunctionDecl>(parent_decl)) {
        // FIXME: We should not end up here at all. For some reason we're trying
        // to look up members of a local type inside the parent context of the
        // local type.
        //
        // This code path is broken and is about to be replaced.
        if (name.isSpecial())
          return result._decls.size();

        // Do a local lookup into the function, using the end loc to approximate
        // being able to see all the local variables.
        // FIXME: Need a more complete/robust lookup mechanism that can handle
        // declarations in sub-stmts, etc.
        UnqualifiedLookup lookup(name, FD, ast->getLazyResolver(),
                                 FD->getEndLoc());
        if (!lookup.isSuccess()) {
          result._error = "no decl found in function";
        } else {
          for (auto decl : lookup.Results) {
            auto *VD = decl.getValueDecl();
            result._decls.push_back(VD);
            result._types.push_back(VD->getInterfaceType());
          }
        }
        return result._decls.size();

      } else {
        result._error = stringWithFormat(
            "decl is not a nominal_decl (DeclKind=%u), lookup for '%s' failed",
            (uint32_t)parent_decl->getKind(),
            name.userFacingName().str().c_str());
      }
    }
  } else if (result._module) {
    ModuleDecl::AccessPathTy access_path;
    SmallVector<ValueDecl *, 4> decls;
    if (priv_decl_id)
      result._module.lookupMember(
          name, ast->getIdentifier(priv_decl_id.getValue().c_str()), decls);
    else
      result._module.lookupValue(access_path, name, NLKind::QualifiedLookup,
                                 decls);
    if (decls.empty()) {
      result._error =
          stringWithFormat("no decl named '%s' found in module '%s'",
                           name.userFacingName().str().c_str(),
                           result._module.GetName().data());
    } else {
      for (auto decl : decls) {
        if (decl->hasInterfaceType()) {
          result._decls.push_back(decl);
          result._types.push_back(decl->getInterfaceType());
          MetatypeType *meta_type =
            result._types.back()->getAs<MetatypeType>();
          if (meta_type)
            result._types.back() = meta_type->getInstanceType();
        }
      }
      return result._types.size();
    }
  }
  result.Clear();
  result._error = "Generic error.";
  return false;
}

static DeclKind GetKindAsDeclKind(Demangle::Node::Kind node_kind) {
  switch (node_kind) {
  case Demangle::Node::Kind::TypeAlias:
    return DeclKind::TypeAlias;
  case Demangle::Node::Kind::Structure:
    return DeclKind::Struct;
  case Demangle::Node::Kind::Class:
    return DeclKind::Class;
  case Demangle::Node::Kind::Allocator:
    return DeclKind::Constructor;
  case Demangle::Node::Kind::Function:
    return DeclKind::Func;
  case Demangle::Node::Kind::Enum:
    return DeclKind::Enum;
  case Demangle::Node::Kind::Protocol:
    return DeclKind::Protocol;
  case Demangle::Node::Kind::Variable:
    return DeclKind::Var;
  default:
    llvm_unreachable("Missing alias");
  }
}

// This should be called with a function type & its associated Decl.  If the
// type is not a function type,
// then we just return the original type, but we don't check that the Decl is
// the associated one.
// It is assumed you will get that right in calling this.
// Returns a version of the input type with the ExtInfo AbstractCC set
// correctly.
// This allows CompilerType::IsSwiftMethod to work properly off the swift Type.
// FIXME: we don't currently distinguish between Method & Witness.  These types
// don't actually get used
// to make Calling Convention choices - originally we were leaving them all at
// Normal...  But if we ever
// need to set it for that purpose we will have to fix that here.
static TypeBase *FixCallingConv(Decl *in_decl, TypeBase *in_type) {
  if (!in_decl)
    return in_type;

  auto *func_type = dyn_cast<AnyFunctionType>(in_type);
  if (func_type) {
    DeclContext *decl_context = in_decl->getDeclContext();
    if (decl_context && decl_context->isTypeContext()) {
      // Add the ExtInfo:
      AnyFunctionType::ExtInfo new_info(
          func_type->getExtInfo().withSILRepresentation(
              SILFunctionTypeRepresentation::Method));
      return func_type->withExtInfo(new_info);
    }
  }
  return in_type;
}

static void
VisitNode(ASTContext *ast,
          Demangle::NodePointer node,
          VisitNodeResult &result);

static void VisitNodeAddressor(
    ASTContext *ast,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  // Addressors are apparently SIL-level functions of the form () -> RawPointer
  // and they bear no connection to their original variable at the interface
  // level
  CanFunctionType swift_can_func_type =
    CanFunctionType::get({}, ast->TheRawPointerType);
  result._types.push_back(swift_can_func_type.getPointer());
}

static void VisitNodeAssociatedTypeRef(
    ASTContext *ast,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  Demangle::NodePointer root = cur_node->getChild(0);
  Demangle::NodePointer ident = cur_node->getChild(1);
  if (!root || !ident)
    return;
  VisitNodeResult type_result;
  VisitNode(ast, root, type_result);
  if (type_result._types.size() == 1) {
    TypeBase *type = type_result._types[0].getPointer();
    if (type) {
      ArchetypeType *archetype = type->getAs<ArchetypeType>();
      if (archetype) {
        Identifier identifier = ast->getIdentifier(ident->getText());
        Type nested;
        if (archetype->hasNestedType(identifier))
          nested = archetype->getNestedType(identifier);
        if (nested) {
          result._types.push_back(nested);
          result._module = type_result._module;
          return;
        }
      }
    }
  }
  result._types.clear();
  result._error = stringWithFormat(
      "unable to find associated type %s in context",
      ident->getText().str().c_str());
}

static void VisitNodeGenericTypealias(ASTContext *ast,
                                      Demangle::NodePointer cur_node,
                                      VisitNodeResult &result) {
  VisitNodeResult generic_type_result;
  VisitNodeResult template_types_result;

  Demangle::Node::iterator end = cur_node->end();
  for (Demangle::Node::iterator pos = cur_node->begin(); pos != end; ++pos) {
    const Demangle::Node::Kind child_node_kind = (*pos)->getKind();
    switch (child_node_kind) {
    case Demangle::Node::Kind::Type:
      VisitNode(ast, *pos, generic_type_result);
      break;
    case Demangle::Node::Kind::TypeList:
      VisitNode(ast, *pos, template_types_result);
      break;
    default:
      break;
    }
  }

  if (generic_type_result._decls.size() != 1 ||
      generic_type_result._types.size() != 1)
    return;

  auto *genericTypeAlias =
      cast<TypeAliasDecl>(generic_type_result._decls.front());
  GenericSignature *signature = genericTypeAlias->getGenericSignature();
  if (signature &&
      template_types_result._types.size() !=
        signature->getGenericParams().size()) {
    result._error = stringWithFormat(
        "wrong number of generic arguments (%d) for generic typealias %s; "
        "expected %d",
        template_types_result._types.size(),
        genericTypeAlias->getBaseName().userFacingName(),
        signature->getGenericParams().size());

    return;
  }

  if (signature && signature->getNumConformanceRequirements() != 0) {
    result._error =
      "cannot handle generic typealias with conformance requirements";
    return;
  }

  // FIXME: handle conformances.
  SubstitutionMap subMap;
  if (signature)
    subMap = SubstitutionMap::get(signature, template_types_result._types,
                                  ArrayRef<ProtocolConformanceRef>({}));
  Type parentType;
  if (auto nominal = genericTypeAlias->getDeclContext()
                         ->getSelfNominalTypeDecl()) {
    parentType = nominal->getDeclaredInterfaceType().subst(subMap);
  }
  NameAliasType *NAT = NameAliasType::get(
      genericTypeAlias, parentType, subMap,
      genericTypeAlias->getDeclaredInterfaceType().subst(subMap));
  result._types.push_back(NAT);
  result._decls.push_back(genericTypeAlias);
}

static void VisitNodeBoundGeneric(
    ASTContext *ast,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  if (cur_node->begin() != cur_node->end()) {
    VisitNodeResult generic_type_result;
    VisitNodeResult template_types_result;

    Demangle::Node::iterator end = cur_node->end();
    for (Demangle::Node::iterator pos = cur_node->begin(); pos != end; ++pos) {
      const Demangle::Node::Kind child_node_kind = (*pos)->getKind();
      switch (child_node_kind) {
      case Demangle::Node::Kind::Type:
        VisitNode(ast, *pos, generic_type_result);
        break;
      case Demangle::Node::Kind::TypeList:
        VisitNode(ast, *pos, template_types_result);
        break;
      default:
        break;
      }
    }

    if (generic_type_result._decls.size() == 1 &&
        generic_type_result._types.size() == 1 &&
        !template_types_result._types.empty()) {
      auto *nominal_type_decl =
          cast<NominalTypeDecl>(generic_type_result._decls.front());
      auto parent_type = generic_type_result._types.front()
          ->getNominalParent();
      result._types.push_back(
        BoundGenericType::get(
          nominal_type_decl, parent_type, template_types_result._types));
      result._decls.push_back(nominal_type_decl);
    }
  }
}

static void VisitNodeBuiltinTypeName(
    ASTContext *ast,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  std::string builtin_name = cur_node->getText();

  StringRef builtin_name_ref(builtin_name);

  if (builtin_name_ref.startswith(BUILTIN_TYPE_NAME_PREFIX)) {
    StringRef stripped_name_ref =
        builtin_name_ref.drop_front(strlen(BUILTIN_TYPE_NAME_PREFIX));
    SmallVector<ValueDecl *, 1> builtin_decls;

    result._module =
        DeclsLookupSource::GetDeclsLookupSource(*ast, ConstString(BUILTIN_NAME));

    if (!FindNamedDecls(ast, ast->getIdentifier(stripped_name_ref), result)) {
      result.Clear();
      result._error = stringWithFormat("Couldn't find %s in the builtin module",
                                       builtin_name.c_str());
    }
  } else {
    result._error = stringWithFormat(
        "BuiltinTypeName %s doesn't start with %s", builtin_name.c_str(), BUILTIN_TYPE_NAME_PREFIX);
  }
}

static void VisitNodeConstructor(
    ASTContext *ast,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  VisitNodeResult kind_type_result;
  VisitNodeResult type_result;
  std::vector<StringRef> labels;

  Demangle::Node::iterator end = cur_node->end();
  for (Demangle::Node::iterator pos = cur_node->begin(); pos != end; ++pos) {
    const Demangle::Node::Kind child_node_kind = (*pos)->getKind();
    switch (child_node_kind) {
    case Demangle::Node::Kind::Enum:
    case Demangle::Node::Kind::Class:
    case Demangle::Node::Kind::Structure:
      VisitNode(ast, *pos, kind_type_result);
      break;
    case Demangle::Node::Kind::LabelList: {
      for (const auto &label : **pos) {
        if (label->getKind() == Demangle::Node::Kind::FirstElementMarker)
          labels.push_back(StringRef());
        else {
          labels.push_back(label->getText());
        }
      }
      break;
    }
    case Demangle::Node::Kind::Type:
      VisitNode(ast, *pos, type_result);
      break;
    default:
      break;
    }
  }

  if (kind_type_result.HasSingleType() && type_result.HasSingleType()) {
    bool found = false;
    const size_t n = FindNamedDecls(ast, DeclBaseName::createConstructor(),
                                    kind_type_result);
    if (n == 1) {
      found = true;
      kind_type_result._types[0] = FixCallingConv(
          kind_type_result._decls[0], kind_type_result._types[0].getPointer());
      result = kind_type_result;
    } else if (n > 0) {
      const size_t num_kind_type_results = kind_type_result._types.size();
      for (size_t i = 0; i < num_kind_type_results && !found; ++i) {
        auto &identifier_type = kind_type_result._types[i];
        if (identifier_type &&
            identifier_type->getKind() ==
                type_result._types.front()->getKind()) {
          // These are the same kind of type, we need to disambiguate them
          switch (identifier_type->getKind()) {
          default:
            break;
          case TypeKind::Function: {
            const AnyFunctionType *identifier_func =
                identifier_type->getAs<AnyFunctionType>();

            // inits are typed as (Foo.Type) -> (args...) -> Foo, but don't
            // assert that in case we're dealing with broken code.
            if (identifier_func->getParams().size() == 1 &&
                identifier_func->getParams()[0].getOldType()->is<AnyMetatypeType>() &&
                identifier_func->getResult()->is<AnyFunctionType>()) {
              identifier_func =
                  identifier_func->getResult()->getAs<AnyFunctionType>();
            }

            const AnyFunctionType *type_func =
                type_result._types.front()->getAs<AnyFunctionType>();

            if (CompareFunctionTypes(type_func, identifier_func, labels)) {
              result._module = kind_type_result._module;
              result._decls.push_back(kind_type_result._decls[i]);
              result._types.push_back(
                  FixCallingConv(kind_type_result._decls[i],
                                 kind_type_result._types[i].getPointer()));
              found = true;
            }
          } break;
          }
        }
      }
    }
    // Didn't find a match, just return the raw function type
    if (!found)
      result = type_result;
  }
}

static void VisitNodeDestructor(
    ASTContext *ast,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  VisitNodeResult kind_type_result;

  Demangle::Node::iterator end = cur_node->end();
  for (Demangle::Node::iterator pos = cur_node->begin(); pos != end; ++pos) {
    const Demangle::Node::Kind child_node_kind = (*pos)->getKind();
    switch (child_node_kind) {
    case Demangle::Node::Kind::Enum:
    case Demangle::Node::Kind::Class:
    case Demangle::Node::Kind::Structure:
      VisitNode(ast, *pos, kind_type_result);
      break;
    default:
      break;
    }
  }

  if (kind_type_result.HasSingleType()) {
    const size_t n = FindNamedDecls(ast, DeclBaseName::createDestructor(),
                                    kind_type_result);
    if (n == 1) {
      kind_type_result._types[0] = FixCallingConv(
          kind_type_result._decls[0], kind_type_result._types[0].getPointer());
      result = kind_type_result;
    } else if (n > 0) {
      // I can't think of a reason why we would get more than one decl called
      // deinit here, but
      // just in case, if it is a function type, we should remember it.
      bool found = false;
      const size_t num_kind_type_results = kind_type_result._types.size();
      for (size_t i = 0; i < num_kind_type_results && !found; ++i) {
        auto &identifier_type = kind_type_result._types[i];
        if (identifier_type) {
          switch (identifier_type->getKind()) {
          default:
            break;
          case TypeKind::Function: {
            result._module = kind_type_result._module;
            result._decls.push_back(kind_type_result._decls[i]);
            result._types.push_back(
                FixCallingConv(kind_type_result._decls[i],
                               kind_type_result._types[i].getPointer()));
            found = true;
          } break;
          }
        }
      }
    }
  }
}

static void VisitNodeDependentMember(ASTContext *ast,
                                     Demangle::NodePointer cur_node,
                                     VisitNodeResult &result) {
  if (cur_node->getNumChildren() == 2) {
    auto dep = cur_node->getChild(0);
    auto assoc = cur_node->getChild(1);
    VisitNodeResult dependency;
    if (dep->getKind() == Demangle::Node::Kind::Type &&
        assoc->getKind() == Demangle::Node::Kind::DependentAssociatedTypeRef) {
      VisitNode(ast, dep, dependency);
      if (dependency._types.size() == 1 && assoc->hasText()) {
        Identifier name = ast->getIdentifier(assoc->getText());
        result._types.push_back(
            DependentMemberType::get(dependency._types[0], name));
        return;
      }
    }
  }
  result._error = "bad dependent member type";
}


static Demangle::NodePointer DropGenericSignature(
    Demangle::NodePointer cur_node) {
  if (cur_node->getKind() != Demangle::Node::Kind::DependentGenericType)
    return nullptr;
  if (cur_node->getChild(0) == nullptr)
    return nullptr;
  return cur_node->getFirstChild();
}

static void VisitNodeDeclContext(
    ASTContext *ast,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  switch (cur_node->getNumChildren()) {
  default:
    result._error =
        stringWithFormat("DeclContext had %llu children, giving up",
                         (unsigned long long)cur_node->getNumChildren());
    break;
  case 0:
    result._error = "empty DeclContext unusable";
    break;
  case 1:
    // nominal type
    VisitNode(ast, cur_node->getFirstChild(), result);
    break;
  case 2:
    // function type: decl-ctx + type
    // FIXME: we should just be able to demangle the DeclCtx and resolve the
    // function
    // this is fragile and will easily break
    Demangle::NodePointer path = cur_node->getFirstChild();
    VisitNodeResult found_decls;
    VisitNode(ast, path, found_decls);
    Demangle::NodePointer generics = cur_node->getChild(1);
    if (generics->getChild(0) == nullptr)
      break;
    generics = generics->getFirstChild();
    generics = DropGenericSignature(generics);
    if (generics == nullptr)
      break;

    AbstractFunctionDecl *func_decl = nullptr;
    for (Decl *decl : found_decls._decls) {
      func_decl = dyn_cast<AbstractFunctionDecl>(decl);
      if (!func_decl)
        continue;
      GenericParamList *gen_params = func_decl->getGenericParams();
      if (!gen_params)
        continue;
    }
    if (func_decl) {
      result._module = found_decls._module;
      result._decls.push_back(func_decl);
      result._types.push_back(func_decl->getInterfaceType().getPointer());
    } else
      result._error = "could not find a matching function for the DeclContext";
    break;
  }
}

static void VisitNodeExplicitClosure(
    ASTContext *ast,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  // FIXME: closures are mangled as hanging off a function, but they really
  // aren't
  // so we cannot really do a lot about them, other than make a function type
  // for whatever their advertised type is, and cross fingers
  VisitNodeResult function_result;
  VisitNodeResult closure_type_result;
  VisitNodeResult module_result;
  Demangle::Node::iterator end = cur_node->end();
  for (Demangle::Node::iterator pos = cur_node->begin(); pos != end; ++pos) {
    const Demangle::Node::Kind child_node_kind = (*pos)->getKind();
    switch (child_node_kind) {
    default:
      result._error =
          stringWithFormat("%s encountered in ExplicitClosure children",
                      Demangle::getNodeKindString(child_node_kind));
      break;
    case Demangle::Node::Kind::Module:
      VisitNode(ast, *pos, module_result);
      break;
    case Demangle::Node::Kind::Function:
      VisitNode(ast, *pos, function_result);
      break;
    case Demangle::Node::Kind::Number:
      break;
    case Demangle::Node::Kind::Type:
      VisitNode(ast, *pos, closure_type_result);
      break;
    }
  }
  if (closure_type_result.HasSingleType())
    result._types.push_back(closure_type_result._types.front());
  else
    result._error = "multiple potential candidates to be this closure's type";
  // FIXME: closures are not lookupable by compiler team's decision
  // ("You cannot perform lookup into local contexts." x3)
  // so we at least store the module the closure came from to enable
  // further local types lookups
  if (module_result._module)
    result._module = module_result._module;
}

static void VisitNodeExtension(
    ASTContext *ast,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  VisitNodeResult module_result;
  VisitNodeResult type_result;
  std::string error;
  Demangle::Node::iterator end = cur_node->end();
  for (Demangle::Node::iterator pos = cur_node->begin(); pos != end; ++pos) {
    const Demangle::Node::Kind child_node_kind = (*pos)->getKind();
    switch (child_node_kind) {
    default:
      result._error =
          stringWithFormat("%s encountered in extension children",
                      Demangle::getNodeKindString(child_node_kind));
      break;

    case Demangle::Node::Kind::Module:
      VisitNode(ast, *pos, module_result);
      break;

    case Demangle::Node::Kind::Class:
    case Demangle::Node::Kind::Enum:
    case Demangle::Node::Kind::Structure:
    case Demangle::Node::Kind::Protocol:
      VisitNode(ast, *pos, type_result);
      break;
    }
  }

  if (module_result._module) {
    if (type_result._decls.size() == 1) {
      Decl *decl = type_result._decls[0];
      NominalTypeDecl *nominal_decl = dyn_cast_or_null<NominalTypeDecl>(decl);
      if (nominal_decl) {
        result._module = DeclsLookupSource::GetDeclsLookupSource(
            module_result._module, nominal_decl);
      } else
        result._error = "unable to find nominal type for extension";
    } else
      result._error = "unable to find unique type for extension";
  } else
    result._error = "unable to find module name for extension";
}

static bool AreBothFunctionTypes(TypeKind a, TypeKind b) {
  bool is_first = false, is_second = false;
  if (a >= TypeKind::First_AnyFunctionType &&
      a <= TypeKind::Last_AnyFunctionType)
    is_first = true;
  if (b >= TypeKind::First_AnyFunctionType &&
      b <= TypeKind::Last_AnyFunctionType)
    is_second = true;
  return (is_first && is_second);
}

static bool CompareFunctionTypes(const AnyFunctionType *f,
                                 const AnyFunctionType *g,
                                 Optional<std::vector<StringRef>> fLabels,
                                 Optional<std::vector<StringRef>> gLabels,
                                 bool *input_matches, bool *output_matches) {
  if (nullptr == f)
    return (nullptr == g);
  if (nullptr == g)
    return false;

  auto getLabel = [&](Optional<std::vector<StringRef>> labels,
                      AnyFunctionType::Param &param,
                      unsigned index) -> StringRef {
    return (labels && labels->size() > index) ? (*labels)[index]
                                              : param.getLabel().str();
  };

  auto params1 = f->getParams();
  auto params2 = g->getParams();

  bool in_matches = params1.size() == params2.size(), out_matches = true;
  auto numParams = std::min(params2.size(), params1.size());

  for (unsigned i = 0; i != numParams; ++i) {
    auto param1 = params1[i];
    auto param2 = params2[i];

    auto label1 = getLabel(fLabels, param1, i);
    auto label2 = getLabel(gLabels, param2, i);

    if (label1.equals(label2) &&
        param1.getOldType()->isEqual(param2.getOldType()))
      continue;

    in_matches = false;
    break;
  }

  out_matches = f->getResult()->isEqual(g->getResult());

  if (input_matches)
    *input_matches = in_matches;
  if (output_matches)
    *output_matches = out_matches;

  return (in_matches && out_matches);
}

static void VisitNodePrivateDeclName(
    ASTContext *ast,
    Demangle::NodePointer parent_node,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  DeclKind decl_kind = GetKindAsDeclKind(parent_node->getKind());

  if (cur_node->getNumChildren() != 2) {
    if (result._error.empty())
      result._error = stringWithFormat(
          "unable to retrieve content for Node::Kind::PrivateDeclName");
    return;
  }

  Demangle::NodePointer priv_decl_id_node(cur_node->getChild(0));
  Demangle::NodePointer id_node(cur_node->getChild(1));

  if (!priv_decl_id_node->hasText() || !id_node->hasText()) {
    if (result._error.empty())
      result._error = stringWithFormat(
          "unable to retrieve content for Node::Kind::PrivateDeclName");
    return;
  }

  if (!FindFirstNamedDeclWithKind(ast, ast->getIdentifier(id_node->getText()),
                                  decl_kind, result,
                                  priv_decl_id_node->getText().str())) {
    if (result._error.empty())
      result._error = stringWithFormat(
          "unable to find Node::Kind::PrivateDeclName '%s' in '%s'",
          id_node->getText().str().c_str(),
          priv_decl_id_node->getText().str().c_str());
  }
}

static void VisitLocalDeclVariableName(ASTContext *ast,
                                       Demangle::NodePointer child,
                                       VisitNodeResult &result) {
  if (child->getNumChildren() != 2 || !child->getChild(1)->hasText()) {
    if (result._error.empty())
      result._error =
          "unable to retrieve content for Node::Kind::LocalDeclName";
    return;
  }

  auto name = child->getChild(1);
  FindNamedDecls(ast, ast->getIdentifier(name->getText()), result);
  if (result._decls.empty())
    result._error = stringWithFormat(
      "demangled identifier '%s' could not be found by name lookup",
      name->getText());
}

// VisitNodeFunction gets used for Function and Variable.
static void VisitNodeFunction(
    ASTContext *ast,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  VisitNodeResult identifier_result;
  VisitNodeResult type_result;
  VisitNodeResult decl_scope_result;
  std::vector<StringRef> labels;

  Demangle::Node::iterator end = cur_node->end();
  bool found_univocous = false;
  for (Demangle::Node::iterator pos = cur_node->begin(); pos != end; ++pos) {
    if (found_univocous)
      break;
    auto child = *pos;
    const Demangle::Node::Kind child_node_kind = child->getKind();
    switch (child_node_kind) {
    default:
      result._error =
          stringWithFormat("%s encountered in function children",
                    Demangle::getNodeKindString(child_node_kind));
      break;

    // TODO: any other possible containers?
    case Demangle::Node::Kind::Function:
    case Demangle::Node::Kind::Class:
    case Demangle::Node::Kind::Enum:
    case Demangle::Node::Kind::Module:
    case Demangle::Node::Kind::Structure:
    case Demangle::Node::Kind::Protocol:
    case Demangle::Node::Kind::Extension:
      VisitNode(ast, *pos, decl_scope_result);
      break;

    case Demangle::Node::Kind::PrivateDeclName: {
      VisitNodePrivateDeclName(ast, cur_node, child, decl_scope_result);

      // No results found, giving up.
      if (decl_scope_result._decls.empty())
        break;

      std::copy(decl_scope_result._decls.begin(),
                decl_scope_result._decls.end(),
                back_inserter(identifier_result._decls));
      std::copy(decl_scope_result._types.begin(),
                decl_scope_result._types.end(),
                back_inserter(identifier_result._types));
      identifier_result._module = decl_scope_result._module;
      if (decl_scope_result._decls.size() == 1)
        found_univocous = true;
      break;
    }

    case Demangle::Node::Kind::LabelList: {
      for (const auto &label : **pos) {
        if (label->getKind() == Demangle::Node::Kind::FirstElementMarker)
          labels.push_back(StringRef());
        else {
          labels.push_back(label->getText());
        }
      }
      break;
    }

    case Demangle::Node::Kind::LocalDeclName: {
      VisitLocalDeclVariableName(ast, child, decl_scope_result);
      if (decl_scope_result._decls.empty())
        break;
      std::copy(decl_scope_result._decls.begin(),
                decl_scope_result._decls.end(),
                back_inserter(identifier_result._decls));
      std::copy(decl_scope_result._types.begin(),
                decl_scope_result._types.end(),
                back_inserter(identifier_result._types));
      identifier_result._module = decl_scope_result._module;
      if (decl_scope_result._decls.size() == 1)
        found_univocous = true;
      break;
    }
    case Demangle::Node::Kind::Identifier:
    case Demangle::Node::Kind::InfixOperator:
    case Demangle::Node::Kind::PrefixOperator:
    case Demangle::Node::Kind::PostfixOperator:
      FindNamedDecls(ast, ast->getIdentifier((*pos)->getText()),
                     decl_scope_result);
      if (decl_scope_result._decls.empty()) {
        result._error = stringWithFormat(
            "demangled identifier %s could not be found by name lookup",
            (*pos)->getText().str().c_str());
        break;
      }
      std::copy(decl_scope_result._decls.begin(),
                decl_scope_result._decls.end(),
                back_inserter(identifier_result._decls));
      std::copy(decl_scope_result._types.begin(),
                decl_scope_result._types.end(),
                back_inserter(identifier_result._types));
      identifier_result._module = decl_scope_result._module;
      if (decl_scope_result._decls.size() == 1)
        found_univocous = true;
      break;

    case Demangle::Node::Kind::Type:
      VisitNode(ast, *pos, type_result);
      break;
    }
  }

  do {
    if (cur_node->getKind() == Demangle::Node::Kind::Subscript) {
      FindNamedDecls(ast, DeclBaseName::createSubscript(),
                     decl_scope_result);
      if (decl_scope_result._decls.empty()) {
        result._error = stringWithFormat(
          "subscript identifier could not be found by name lookup");
        break;
      }
      std::copy(decl_scope_result._decls.begin(),
                decl_scope_result._decls.end(),
                back_inserter(identifier_result._decls));
      std::copy(decl_scope_result._types.begin(),
                decl_scope_result._types.end(),
                back_inserter(identifier_result._types));
      identifier_result._module = decl_scope_result._module;
      if (decl_scope_result._decls.size() == 1)
        found_univocous = true;
      break;
    }
  } while (0);

  if (identifier_result._types.size() == 1) {
    result._module = identifier_result._module;
    result._decls.push_back(identifier_result._decls[0]);
    result._types.push_back(FixCallingConv(
        identifier_result._decls[0], identifier_result._types[0].getPointer()));
  } else if (type_result.HasSingleType()) {
    const size_t num_identifier_results = identifier_result._types.size();
    bool found = false;
    for (size_t i = 0; i < num_identifier_results && !found; ++i) {
      auto &identifier_type = identifier_result._types[i];
      if (!identifier_type)
        continue;
      if (AreBothFunctionTypes(identifier_type->getKind(),
                               type_result._types.front()->getKind())) {
        const AnyFunctionType *identifier_func =
            identifier_type->getAs<AnyFunctionType>();
        const AnyFunctionType *type_func =
            type_result._types.front()->getAs<AnyFunctionType>();
        if (CompareFunctionTypes(type_func, identifier_func, labels)) {
          result._module = identifier_result._module;
          result._decls.push_back(identifier_result._decls[i]);
          result._types.push_back(
              FixCallingConv(identifier_result._decls[i],
                             identifier_result._types[i].getPointer()));
          found = true;
        }
      }
    }
    // Didn't find a match, just return the raw function type
    if (!found)
      result = type_result;
  }
}

static void CreateFunctionType(ASTContext *ast,
                               const VisitNodeResult &arg_type_result,
                               const VisitNodeResult &return_type_result,
                               bool escaping,
                               bool throws,
                               VisitNodeResult &result) {
  Type arg_type;
  Type return_type;

  switch (arg_type_result._types.size()) {
  case 0:
    arg_type = TupleType::getEmpty(*ast);
    break;
  case 1:
    arg_type = arg_type_result._types.front().getPointer();
    break;
  default:
    result._error = "too many argument types for a function type";
    break;
  }

  switch (return_type_result._types.size()) {
  case 0:
    return_type = TupleType::getEmpty(*ast);
    break;
  case 1:
    return_type = return_type_result._types.front().getPointer();
    break;
  default:
    result._error = "too many return types for a function type";
    break;
  }

  if (arg_type && return_type) {
    // FIXME: We need to either refactor this code to build function parameters
    // directly, or better yet, scrap TypeReconstruction altogether in favor of
    // TypeDecoder which already does the right thing.
    SmallVector<AnyFunctionType::Param, 8> params;
    AnyFunctionType::decomposeInput(arg_type, params);

    auto ext_info =
      FunctionType::ExtInfo()
        .withNoEscape(!escaping)
        .withThrows(throws);
    result._types.push_back(FunctionType::get(params, return_type, ext_info));
  }
}

static void VisitNodeFunctionType(
    ASTContext *ast,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  VisitNodeResult arg_type_result;
  VisitNodeResult return_type_result;
  Demangle::Node::iterator end = cur_node->end();
  bool throws = false;
  for (Demangle::Node::iterator pos = cur_node->begin(); pos != end; ++pos) {
    const Demangle::Node::Kind child_node_kind = (*pos)->getKind();
    switch (child_node_kind) {
    case Demangle::Node::Kind::ArgumentTuple:
      VisitNode(ast, *pos, arg_type_result);
      break;
    case Demangle::Node::Kind::ThrowsAnnotation:
      throws = true;
      break;
    case Demangle::Node::Kind::ReturnType:
      VisitNode(ast, *pos, return_type_result);
      break;
    default:
      break;
    }
  }
  CreateFunctionType(ast, arg_type_result, return_type_result,
                     cur_node->getKind() == Demangle::Node::Kind::FunctionType,
                     throws, result);
}

static void VisitNodeImplFunctionType(
    ASTContext *ast,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  VisitNodeResult arg_type_result;
  VisitNodeResult return_type_result;
  Demangle::Node::iterator end = cur_node->end();
  bool escaping = false;
  bool throws = false;
  for (Demangle::Node::iterator pos = cur_node->begin(); pos != end; ++pos) {
    const Demangle::Node::Kind child_node_kind = (*pos)->getKind();
    switch (child_node_kind) {
    case Demangle::Node::Kind::ImplEscaping:
      escaping = true;
      break;
    case Demangle::Node::Kind::ImplConvention:
      // Ignore the ImplConvention it is only a hint for the SIL ARC optimizer.
      break;
    case Demangle::Node::Kind::ImplParameter:
      VisitNode(ast, *pos, arg_type_result);
      break;
    case Demangle::Node::Kind::ThrowsAnnotation:
      throws = true;
      break;
    case Demangle::Node::Kind::ImplResult:
      VisitNode(ast, *pos, return_type_result);
      break;
    default:
      break;
    }
  }
  CreateFunctionType(ast, arg_type_result, return_type_result, escaping, throws,
                     result);
}


static void VisitNodeSetterGetter(
    ASTContext *ast,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  VisitNodeResult decl_ctx_result;
  std::string identifier;
  std::vector<StringRef> labels;

  VisitNodeResult type_result;

  assert(cur_node->getNumChildren() == 1 &&
         "Accessor should have a single abstract storage child");
  Demangle::NodePointer referenced_node = cur_node->getFirstChild();
  assert((referenced_node->getKind() == Demangle::Node::Kind::Variable ||
          referenced_node->getKind() == Demangle::Node::Kind::Subscript) &&
         "Accessor child should be a storage node");

  Demangle::Node::Kind node_kind = cur_node->getKind();

  for (Demangle::Node::iterator pos = referenced_node->begin(),
                                end = referenced_node->end();
       pos != end; ++pos) {
    const Demangle::Node::Kind child_node_kind = (*pos)->getKind();
    switch (child_node_kind) {
    case Demangle::Node::Kind::Class:
    case Demangle::Node::Kind::Module:
    case Demangle::Node::Kind::Structure:
      VisitNode(ast, *pos, decl_ctx_result);
      break;
    case Demangle::Node::Kind::Identifier:
      identifier.assign((*pos)->getText());
      break;
    case Demangle::Node::Kind::Type: {
      auto type = (*pos)->getFirstChild();
      if (DropGenericSignature(type))
        type = type->getChild(1);
      VisitNode(ast, type, type_result);
      break;
    }
    case Demangle::Node::Kind::LabelList: {
      for (const auto &label : **pos) {
        if (label->getKind() == Demangle::Node::Kind::FirstElementMarker)
          labels.push_back(StringRef());
        else {
          labels.push_back(label->getText());
        }
      }
      break;
    }
    default:
      result._error =
          stringWithFormat("%s encountered in generic type children",
                      Demangle::getNodeKindString(child_node_kind));
      break;
    }
  }

  if (!type_result.HasSingleType()) {
    result._error = "bad type";
    return;
  }

  if (referenced_node->getKind() == Demangle::Node::Kind::Subscript) {
    // Since there can be many subscripts for the same nominal type, we need to
    // find the one matching the specified type.

    FindNamedDecls(ast, DeclBaseName::createSubscript(), decl_ctx_result);
    size_t num_decls = decl_ctx_result._decls.size();

    if (num_decls == 0) {
      result._error = "Could not find a subscript decl";
      return;
    }

    SubscriptDecl *subscript_decl;
    const AnyFunctionType *type_func =
        type_result._types.front()->getAs<AnyFunctionType>();

    FuncDecl *identifier_func = nullptr;

    for (size_t i = 0; i < num_decls; i++) {
      subscript_decl =
          dyn_cast_or_null<SubscriptDecl>(decl_ctx_result._decls[i]);
      if (subscript_decl) {
        switch (node_kind) {
        case Demangle::Node::Kind::Getter:
          identifier_func = subscript_decl->getGetter();
          break;
        case Demangle::Node::Kind::Setter:
          identifier_func = subscript_decl->getSetter();
          break;
        case Demangle::Node::Kind::DidSet:
          identifier_func = subscript_decl->getDidSetFunc();
          break;
        case Demangle::Node::Kind::WillSet:
          identifier_func = subscript_decl->getWillSetFunc();
          break;
        default:
          identifier_func = nullptr;
          break;
        }

        if (identifier_func &&
            subscript_decl->getGetter() &&
            subscript_decl->getGetter()->getInterfaceType()) {
          auto subscript_type =
            subscript_decl->getGetter()->getInterfaceType()->getAs<AnyFunctionType>();

          if (subscript_type) {
            // Swift function types are formally functions that take the class
            // and return the method,
            // we have to strip off the first level of function call to compare
            // against the type
            // from the demangled name.
            auto subscript_uncurried_result =
                subscript_type->getResult()->getAs<AnyFunctionType>();
            if (subscript_uncurried_result) {
              if (CompareFunctionTypes(type_func,
                                       subscript_uncurried_result,
                                       labels)) {
                break;
              }
            }
          }
        }
      }
      identifier_func = nullptr;
    }

    if (identifier_func) {
      result._decls.push_back(identifier_func);
      result._types.push_back(FixCallingConv(
          identifier_func, identifier_func->getInterfaceType().getPointer()));
    } else {
      result._error = "could not find a matching subscript signature";
    }
  } else {
    // Otherwise this is a getter/setter/etc for a variable.  Currently you
    // can't write a getter/setter that
    // takes a different type from the type of the variable.  So there is only
    // one possible function.
    AbstractStorageDecl *var_decl = nullptr;

    FindFirstNamedDeclWithKind(ast, ast->getIdentifier(identifier),
                               DeclKind::Var, decl_ctx_result);

    if (decl_ctx_result._decls.size() == 1) {
      var_decl = dyn_cast_or_null<VarDecl>(decl_ctx_result._decls[0]);
    } else if (!decl_ctx_result._decls.empty()) {
      // TODO: can we use the type to pick the right one? can we really have
      // multiple variables with the same name?
      result._error = stringWithFormat(
          "multiple variables with the same name %s", identifier.c_str());
      return;
    } else {
      result._error =
          stringWithFormat("no variables with the name %s", identifier.c_str());
      return;
    }

    if (var_decl) {
      FuncDecl *decl = nullptr;

      if (node_kind == Demangle::Node::Kind::DidSet &&
          var_decl->getDidSetFunc()) {
        decl = var_decl->getDidSetFunc();
      } else if (node_kind == Demangle::Node::Kind::Getter &&
                 var_decl->getGetter()) {
        decl = var_decl->getGetter();
      } else if (node_kind == Demangle::Node::Kind::Setter &&
                 var_decl->getSetter()) {
        decl = var_decl->getSetter();
      } else if (node_kind == Demangle::Node::Kind::WillSet &&
                 var_decl->getWillSetFunc()) {
        decl = var_decl->getWillSetFunc();
      }

      if (decl) {
        result._decls.push_back(decl);
        result._types.push_back(
            FixCallingConv(decl, decl->getInterfaceType().getPointer()));
      } else {
        result._error = stringWithFormat(
            "could not retrieve %s for variable %s",
            Demangle::getNodeKindString(node_kind),
            identifier.c_str());
        return;
      }
    } else {
      result._error =
          stringWithFormat("no decl object for %s", identifier.c_str());
      return;
    }
  }
}

static void VisitNodeIdentifier(
    ASTContext *ast,
    Demangle::NodePointer parent_node,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  DeclKind decl_kind = GetKindAsDeclKind(parent_node->getKind());

  if (!FindFirstNamedDeclWithKind(ast, ast->getIdentifier(cur_node->getText()),
                                  decl_kind, result)) {
    if (result._error.empty())
      result._error =
          stringWithFormat("unable to find Node::Kind::Identifier '%s'",
                           cur_node->getText().str().c_str());
  }
}

static void VisitNodeLocalDeclName(
    ASTContext *ast,
    Demangle::NodePointer parent_node,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  std::string remangledNode = Demangle::mangleNode(parent_node);
  TypeDecl *decl = result._module.lookupLocalType(remangledNode);
  if (!decl)
    result._error = stringWithFormat("unable to lookup local type %s",
                                     remangledNode.c_str());
  else {
    // if this were to come from a closure, there may be no decl - just a module
    if (!result._decls.empty())
      result._decls.pop_back();
    if (!result._types.empty())
      result._types.pop_back();

    result._decls.push_back(decl);
    auto type = decl->getDeclaredInterfaceType();
    result._types.push_back(type);
  }
}

static void VisitNodeRelatedEntityDeclName(
    ASTContext *ast,
    Demangle::NodePointer parent_node,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  DeclKind decl_kind = GetKindAsDeclKind(parent_node->getKind());

  if (cur_node->getNumChildren() != 1 || !cur_node->hasText()) {
    if (result._error.empty())
      result._error = stringWithFormat(
          "unable to retrieve content for Node::Kind::RelatedEntityDeclName");
    return;
  }

  Demangle::NodePointer id_node(cur_node->getChild(0));

  if (!id_node->hasText()) {
    if (result._error.empty())
      result._error = stringWithFormat(
          "unable to retrieve content for Node::Kind::RelatedEntityDeclName");
    return;
  }

  SmallVector<ValueDecl *, 4> decls;
  if (result._module) {
    result._module.lookupRelatedEntity(id_node->getText(), cur_node->getText(),
                                       decl_kind, decls);
  }

  if (!FilterDeclsForKind(decl_kind, decls, result)) {
    if (result._error.empty())
      result._error = stringWithFormat(
          "unable to find Node::Kind::RelatedEntityDeclName '%s' for '%s'",
          cur_node->getText().str().c_str(),
          id_node->getText().str().c_str());
    return;
  }
}

static void VisitNodeNominal(
    ASTContext *ast,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  Type parent_type;

  Demangle::Node::iterator child_end = cur_node->end();
  for (Demangle::Node::iterator child_pos = cur_node->begin();
       child_pos != child_end; ++child_pos) {
    auto child = *child_pos;
    switch(child->getKind()) {
    case Demangle::Node::Kind::Identifier:
      VisitNodeIdentifier(ast, cur_node, child, result);
      break;
    case Demangle::Node::Kind::LocalDeclName:
      VisitNodeLocalDeclName(ast, cur_node, child, result);
      break;
    case Demangle::Node::Kind::PrivateDeclName:
      VisitNodePrivateDeclName(ast, cur_node, child, result);
      break;
    case Demangle::Node::Kind::RelatedEntityDeclName:
      VisitNodeRelatedEntityDeclName(ast, cur_node, child, result);
      break;
    default:
      VisitNode(ast, child, result);
      if (result._types.size() == 1)
        parent_type = result._types.front();
      break;
    }
  }

  if (parent_type &&
      parent_type->getAnyNominal() &&
      result._decls.size() == 1 &&
      result._types.size() == 1) {
    auto nominal_type_decl = cast<NominalTypeDecl>(result._decls.front());
    auto subMap = parent_type->getMemberSubstitutionMap(
      nominal_type_decl->getParentModule(), nominal_type_decl);

    result._types[0] = result._types[0].subst(subMap);
  }
}

static void VisitNodeTypeAlias(
    ASTContext *ast,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  Type parent_type;

  Demangle::Node::iterator child_end = cur_node->end();
  for (Demangle::Node::iterator child_pos = cur_node->begin();
       child_pos != child_end; ++child_pos) {
    auto child = *child_pos;
    switch(child->getKind()) {
    case Demangle::Node::Kind::Identifier:
      VisitNodeIdentifier(ast, cur_node, child, result);
      break;
    case Demangle::Node::Kind::LocalDeclName:
      VisitNodeLocalDeclName(ast, cur_node, child, result);
      break;
    case Demangle::Node::Kind::PrivateDeclName:
      VisitNodePrivateDeclName(ast, cur_node, child, result);
      break;
    case Demangle::Node::Kind::RelatedEntityDeclName:
      VisitNodeRelatedEntityDeclName(ast, cur_node, child, result);
      break;
    default:
      VisitNode(ast, child, result);
      break;
    }
  }
}

static void VisitNodeExistentialMetatype(ASTContext *ast,
                                         Demangle::NodePointer cur_node,
                                         VisitNodeResult &result) {
  VisitNodeResult type_result;
  Optional<MetatypeRepresentation> metatype_repr;

  for (auto &child : *cur_node) {
    switch (child->getKind()) {
    case Demangle::Node::Kind::Type:
      VisitNode(ast, child, type_result);
      break;
    default:
      break;
    }
  }

  if (type_result.HasSingleType())
    result._types.push_back(
        ExistentialMetatypeType::get(type_result._types[0], metatype_repr));
  else
    result._error = stringWithFormat(
        "instance type for existential metatype cannot be uniquely resolved");
}

static void VisitNodeMetatype(
    ASTContext *ast,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  auto iter = cur_node->begin();
  auto end = cur_node->end();

  Optional<MetatypeRepresentation> metatype_repr;
  VisitNodeResult type_result;

  for (; iter != end; ++iter) {
    switch ((*iter)->getKind()) {
    case Demangle::Node::Kind::Type:
      VisitNode(ast, *iter, type_result);
      break;
    case Demangle::Node::Kind::MetatypeRepresentation:
      if ((*iter)->getText() == "@thick")
        metatype_repr = MetatypeRepresentation::Thick;
      else if ((*iter)->getText() == "@thin")
        metatype_repr = MetatypeRepresentation::Thin;
      else if ((*iter)->getText() == "@objc")
        metatype_repr = MetatypeRepresentation::ObjC;
      else
        ; // leave it alone if we don't understand the representation
      break;
    default:
      break;
    }
  }

  if (type_result.HasSingleType()) {
    result._types.push_back(
        MetatypeType::get(type_result._types[0], metatype_repr));
  } else {
    result._error = stringWithFormat(
        "instance type for metatype cannot be uniquely resolved");
    return;
  }
}

static void VisitNodeModule(
    ASTContext *ast,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  std::string error;
  StringRef module_name = cur_node->getText();
  if (module_name.empty()) {
    result._error = stringWithFormat("error: empty module name.");
    return;
  }

  result._module = DeclsLookupSource::GetDeclsLookupSource(*ast,
                                                ConstString(module_name.str()));
  if (!result._module) {
    result._error = stringWithFormat("unable to load module '%s' (%s)",
                                     module_name.str().c_str(), error.data());
  }
}

static void VisitNodeTuple(
    ASTContext *ast,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  if (cur_node->begin() == cur_node->end()) {
    // No children of this tuple, make an empty tuple

    result._types.push_back(TupleType::getEmpty(*ast));
  } else {
    std::vector<TupleTypeElt> tuple_fields;
    Demangle::Node::iterator end = cur_node->end();
    for (Demangle::Node::iterator pos = cur_node->begin(); pos != end; ++pos) {
      VisitNodeResult tuple_element_result;
      VisitNode(ast, *pos, tuple_element_result);
      if (tuple_element_result._error.empty() &&
          tuple_element_result._tuple_type_element.getType()) {
        tuple_fields.push_back(tuple_element_result._tuple_type_element);
      } else {
        result._error = tuple_element_result._error;
      }
    }
    if (result._error.empty()) {
      result._types.push_back(TupleType::get(tuple_fields, *ast));
    }
  }
}

static void VisitNodeProtocolList(
    ASTContext *ast,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  if (cur_node->begin() != cur_node->end()) {
    VisitNodeResult protocol_types_result;
    VisitNode(ast, cur_node->getFirstChild(), protocol_types_result);
    if (protocol_types_result._error.empty()) {
      result._types.push_back(
        ProtocolCompositionType::get(*ast, protocol_types_result._types,
                                     /*HasExplicitAnyObject=*/false));
    }
  }
}

static void VisitNodeProtocolListWithClass(
  ASTContext *ast,
  Demangle::NodePointer cur_node, VisitNodeResult &result) {
  if (cur_node->begin() != cur_node->end()) {
    VisitNodeResult class_type_result;
    VisitNodeResult protocol_types_result;
    Demangle::Node::iterator child_end = cur_node->end();
    for (Demangle::Node::iterator child_pos = cur_node->begin();
         child_pos != child_end; ++child_pos) {
      auto child = *child_pos;
      switch(child->getKind()) {
      case Demangle::Node::Kind::ProtocolList:
        VisitNode(ast, child, protocol_types_result);
        break;
      case Demangle::Node::Kind::Type:
        VisitNode(ast, child, class_type_result);
        break;
      default:
        result._error = "invalid subclass existential";
        break;
      }
    }

    if (protocol_types_result._error.empty() &&
        !protocol_types_result._types.empty() &&
        class_type_result._types.size() == 1) {
      SmallVector<Type, 2> members;
      members.push_back(class_type_result._types.front());
      for (auto member : protocol_types_result._types)
        members.push_back(member);
      result._types.push_back(
        ProtocolCompositionType::get(*ast, members,
                                     /*HasExplicitAnyObject=*/false));
    }
  }
}

static void VisitNodeProtocolListWithAnyObject(
  ASTContext *ast,
  Demangle::NodePointer cur_node, VisitNodeResult &result) {
  if (cur_node->begin() != cur_node->end()) {
    VisitNodeResult protocol_types_result;
    VisitNode(ast, cur_node->getFirstChild(), protocol_types_result);
    if (protocol_types_result._error.empty()) {
      result._types.push_back(
        ProtocolCompositionType::get(*ast, protocol_types_result._types,
                                     /*HasExplicitAnyObject=*/true));
    }
  }
}

static void VisitNodeTupleElement(
    ASTContext *ast,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  StringRef tuple_name;
  VisitNodeResult tuple_type_result;
  Demangle::Node::iterator end = cur_node->end();
  for (Demangle::Node::iterator pos = cur_node->begin(); pos != end; ++pos) {
    const Demangle::Node::Kind child_node_kind = (*pos)->getKind();
    switch (child_node_kind) {
    case Demangle::Node::Kind::TupleElementName:
      tuple_name = (*pos)->getText();
      break;
    case Demangle::Node::Kind::Type:
      VisitNode(ast, *pos, tuple_type_result);
      break;
    default:
      break;
    }
  }

  if (!tuple_type_result._error.empty() || tuple_type_result._types.size() != 1)
    return;

  auto tupleType = tuple_type_result._types.front();
  auto typeFlags = ParameterTypeFlags();
  Identifier idName =
      tuple_name.empty() ? Identifier() : ast->getIdentifier(tuple_name);
  result._tuple_type_element = TupleTypeElt(tupleType, idName, typeFlags);
}

static void VisitNodeTypeList(
    ASTContext *ast,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  if (cur_node->begin() != cur_node->end()) {
    Demangle::Node::iterator end = cur_node->end();
    for (Demangle::Node::iterator pos = cur_node->begin(); pos != end; ++pos) {
      VisitNodeResult type_result;
      VisitNode(ast, *pos, type_result);
      if (type_result._error.empty() && type_result._types.size() == 1) {
        if (type_result._decls.empty())
          result._decls.push_back(nullptr);
        else
          result._decls.push_back(type_result._decls.front());
        result._types.push_back(type_result._types.front());
      } else {
        // If we run into any errors parsing getting any types for a type list
        // we need to bail out and return an error. We have cases where a
        // private
        // typealias was used in a mangled variable name. The private typealias
        // are not included in the modules that are available to the debugger.
        // This was contained inside of a bound generic structure type that was
        // expecting two types. If we don't bail out, we end up filling in just
        // one type in "result" instead of two and then the compiler will crash
        // later when it tries to create the bound generic from only 1 type.
        result.Clear();
        result._error = type_result._error;
        break;
      }
    }
  }
}

static void VisitNodeUnowned(
    ASTContext *ast,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  VisitNodeResult type_result;
  VisitNode(ast, cur_node->getFirstChild(), type_result);
  if (type_result._types.size() == 1 && type_result._types[0]) {
    result._types.push_back(
      UnownedStorageType::get(type_result._types[0], *ast));
  } else {
    result._error = "couldn't resolve referent type";
  }
}

static void
VisitNodeWeak(ASTContext *ast,
              Demangle::NodePointer cur_node, VisitNodeResult &result) {
  VisitNodeResult type_result;
  VisitNode(ast, cur_node->getFirstChild(), type_result);
  if (type_result._types.size() == 1 && type_result._types[0]) {
    result._types.push_back(
      WeakStorageType::get(type_result._types[0], *ast));
  } else {
    result._error = "couldn't resolve referent type";
  }
}

static void
VisitNodeGenericParam(ASTContext *ast,
                      Demangle::NodePointer cur_node,
                      VisitNodeResult &result) {
  if (cur_node->getNumChildren() == 2) {
    auto first = cur_node->getChild(0);
    auto second = cur_node->getChild(1);
    if (first->getKind() == Demangle::Node::Kind::Index &&
        second->getKind() == Demangle::Node::Kind::Index) {
      result._types.push_back(
        GenericTypeParamType::get(first->getIndex(),
                                  second->getIndex(), *ast));
      return;
    }
  }

  result._error = "bad generic param type";
}

static void VisitFirstChildNode(
    ASTContext *ast,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  if (cur_node->begin() != cur_node->end()) {
    VisitNode(ast, cur_node->getFirstChild(), result);
  }
}

static void VisitAllChildNodes(
    ASTContext *ast,
    Demangle::NodePointer cur_node, VisitNodeResult &result) {
  Demangle::Node::iterator child_end = cur_node->end();
  for (Demangle::Node::iterator child_pos = cur_node->begin();
       child_pos != child_end; ++child_pos) {
    VisitNode(ast, *child_pos, result);
  }
}

static void VisitNodeGlobal(ASTContext *ast, Demangle::NodePointer cur_node,
                            VisitNodeResult &result) {
  assert(result._error.empty());

  for (const auto &child : *cur_node) {
    switch (child->getKind()) {
    case Demangle::Node::Kind::Identifier:
      result._error = "invalid global node";
      break;
    default:
      VisitNode(ast, child, result);
      break;
    }
  }
}

static void VisitNode(
    ASTContext *ast,
    Demangle::NodePointer node, VisitNodeResult &result) {
  // If we have an error, no point in going forward.
  if (!result._error.empty())
    return;

  const Demangle::Node::Kind nodeKind = node->getKind();

  switch (nodeKind) {
  case Demangle::Node::Kind::OwningAddressor:
  case Demangle::Node::Kind::OwningMutableAddressor:
  case Demangle::Node::Kind::UnsafeAddressor:
  case Demangle::Node::Kind::UnsafeMutableAddressor:
    VisitNodeAddressor(ast, node, result);
    break;

  case Demangle::Node::Kind::ArgumentTuple:
    VisitFirstChildNode(ast, node, result);
    break;

  case Demangle::Node::Kind::AssociatedTypeRef:
    VisitNodeAssociatedTypeRef(ast, node, result);
    break;

  case Demangle::Node::Kind::BoundGenericClass:
  case Demangle::Node::Kind::BoundGenericStructure:
  case Demangle::Node::Kind::BoundGenericEnum:
  case Demangle::Node::Kind::BoundGenericOtherNominalType:
    VisitNodeBoundGeneric(ast, node, result);
    break;

  case Demangle::Node::Kind::BoundGenericProtocol:
    if (node->getNumChildren() < 2)
      return;

    // Only visit the conforming type.
    VisitNode(ast, node->getChild(1), result);
    break;

  case Demangle::Node::Kind::BoundGenericTypeAlias:
    VisitNodeGenericTypealias(ast, node, result);
    break;

  case Demangle::Node::Kind::BuiltinTypeName:
    VisitNodeBuiltinTypeName(ast, node, result);
    break;

  case Demangle::Node::Kind::Structure:
  case Demangle::Node::Kind::Class:
  case Demangle::Node::Kind::Enum:
  case Demangle::Node::Kind::OtherNominalType:
  case Demangle::Node::Kind::Protocol:
    VisitNodeNominal(ast, node, result);
    break;

  case Demangle::Node::Kind::TypeAlias:
    VisitNodeTypeAlias(ast, node, result);
    break;

  case Demangle::Node::Kind::Global:
    VisitNodeGlobal(ast, node, result);
    break;

  case Demangle::Node::Kind::Static:
  case Demangle::Node::Kind::Type:
  case Demangle::Node::Kind::TypeMangling:
  case Demangle::Node::Kind::ReturnType:
    VisitAllChildNodes(ast, node, result);
    break;

  case Demangle::Node::Kind::Allocator:
  case Demangle::Node::Kind::Constructor:
    VisitNodeConstructor(ast, node, result);
    break;

 case Demangle::Node::Kind::DependentMemberType:
    VisitNodeDependentMember(ast, node, result);
    break;

  case Demangle::Node::Kind::Destructor:
    VisitNodeDestructor(ast, node, result);
    break;

  case Demangle::Node::Kind::DeclContext:
    VisitNodeDeclContext(ast, node, result);
    break;

  case Demangle::Node::Kind::ErrorType:
    result._error = "error type encountered while demangling name";
    break;

  case Demangle::Node::Kind::Extension:
    VisitNodeExtension(ast, node, result);
    break;

  case Demangle::Node::Kind::ExplicitClosure:
    VisitNodeExplicitClosure(ast, node, result);
    break;

  case Demangle::Node::Kind::Function:
  case Demangle::Node::Kind::Variable:
  case Demangle::Node::Kind::Subscript: // Out of order on purpose
    VisitNodeFunction(ast, node, result);
    break;

  case Demangle::Node::Kind::FunctionType:
  case Demangle::Node::Kind::NoEscapeFunctionType:
  case Demangle::Node::Kind::UncurriedFunctionType: // Out of order on
                                                    // purpose.
    VisitNodeFunctionType(ast, node, result);
    break;

  case Demangle::Node::Kind::ImplFunctionType:
    VisitNodeImplFunctionType(ast, node, result);
    break;
  
  case Demangle::Node::Kind::DidSet:
  case Demangle::Node::Kind::Getter:
  case Demangle::Node::Kind::Setter:
  case Demangle::Node::Kind::WillSet: // out of order on purpose
    VisitNodeSetterGetter(ast, node, result);
    break;

  case Demangle::Node::Kind::ExistentialMetatype:
    VisitNodeExistentialMetatype(ast, node, result);
    break;

  case Demangle::Node::Kind::Metatype:
    VisitNodeMetatype(ast, node, result);
    break;

  case Demangle::Node::Kind::Module:
    VisitNodeModule(ast, node, result);
    break;

  case Demangle::Node::Kind::Tuple:
    VisitNodeTuple(ast, node, result);
    break;

  case Demangle::Node::Kind::ProtocolList:
    VisitNodeProtocolList(ast, node, result);
    break;

  case Demangle::Node::Kind::ProtocolListWithClass:
    VisitNodeProtocolListWithClass(ast, node, result);
    break;

  case Demangle::Node::Kind::ProtocolListWithAnyObject:
    VisitNodeProtocolListWithAnyObject(ast, node, result);
    break;

  case Demangle::Node::Kind::TupleElement:
    VisitNodeTupleElement(ast, node, result);
    break;

  case Demangle::Node::Kind::TypeList:
    VisitNodeTypeList(ast, node, result);
    break;

  case Demangle::Node::Kind::Unowned:
    VisitNodeUnowned(ast, node, result);
    break;

  case Demangle::Node::Kind::Weak:
    VisitNodeWeak(ast, node, result);
    break;

  case Demangle::Node::Kind::DependentGenericParamType:
    VisitNodeGenericParam(ast, node, result);
    break;

  case Demangle::Node::Kind::LocalDeclName:
  case Demangle::Node::Kind::Identifier:
  case Demangle::Node::Kind::PrivateDeclName:
    llvm_unreachable("Must handle these as part of another node");

  default:
    break;
  }
}

Decl *ide::getDeclFromUSR(ASTContext &context, StringRef USR,
                          std::string &error) {
  if (!USR.startswith("s:")) {
    error = "not a Swift USR";
    return nullptr;
  }

  std::string mangledName(USR);
  // Convert to a symbol name by replacing the USR prefix.
  // This relies on USR generation being very close to symbol mangling; if we
  // need to support entities with customized USRs (e.g. extensions), we will
  // need to do something smarter here.
  mangledName.replace(0, 2, MANGLING_PREFIX_STR);

  return getDeclFromMangledSymbolName(context, mangledName, error);
}

Decl *ide::getDeclFromMangledSymbolName(ASTContext &context,
                                        StringRef mangledName,
                                        std::string &error) {
  Demangle::Context DemangleCtx;
  auto node = DemangleCtx.demangleSymbolAsNode(mangledName);
  VisitNodeResult result;

  if (node)
    VisitNode(&context, node, result);
  error = result._error;
  if (error.empty() && result._decls.size() == 1) {
    return result._decls.front();
  } else {
    llvm::raw_string_ostream OS(error);
    OS << "decl for symbol name '" << mangledName << "' was not found";
    return nullptr;
  }
  return nullptr;
}

Type ide::getTypeFromMangledSymbolname(ASTContext &Ctx,
                                       StringRef mangledName,
                                       std::string &error) {
  Demangle::Context DemangleCtx;
  auto node = DemangleCtx.demangleSymbolAsNode(mangledName);
  VisitNodeResult result;

  if (node)
    VisitNode(&Ctx, node, result);
  error = result._error;
  if (error.empty() && result._types.size() == 1) {
    return result._types.front().getPointer();
  } else {
    error = stringWithFormat("type for symbolname '%s' was not found",
                             mangledName);
    return Type();
  }
  return Type();
}
