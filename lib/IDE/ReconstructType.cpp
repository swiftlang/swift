//===--- ReconstructType.cpp ------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// C++ Includes
#include <mutex> // std::once
#include <queue>
#include <set>

#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/TargetOptions.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetSubtargetInfo.h"
#include "llvm/Target/TargetOptions.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclObjC.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DebuggerClient.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/Mangle.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/SearchPathOptions.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "swift/ASTSectionImporter/ASTSectionImporter.h"
#include "swift/Basic/Demangle.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/Platform.h"
#include "swift/Basic/SourceManager.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangImporterOptions.h"
#include "swift/Driver/Util.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/SIL/SILModule.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Strings.h"

#include "swift/IDE/Utils.h"

typedef const std::string ConstString;
typedef void Log;
typedef swift::ASTContext SwiftASTContext;

static std::string stringWithFormat(const std::string fmt_str, ...) {
  int final_n, n = ((int)fmt_str.size()) * 2;
  std::string str;
  std::unique_ptr<char[]> formatted;
  va_list ap;
  while(1) {
    formatted.reset(new char[n]);
    strcpy(&formatted[0], fmt_str.c_str());
    va_start(ap, fmt_str);
    final_n = vsnprintf(&formatted[0], n, fmt_str.c_str(), ap);
    va_end(ap);
    if (final_n < 0 || final_n >= n)
      n += abs(final_n - n + 1);
    else
      break;
  }
  return std::string(formatted.get());
}

enum class MemberType : uint32_t {
  Invalid,
  BaseClass,
  Field
};

struct MemberInfo {
  swift::Type clang_type;
  const std::string name;
  uint64_t byte_size;
  uint32_t byte_offset;
  MemberType member_type;
  bool is_fragile;

  MemberInfo(MemberType member_type) :
  clang_type(),
  name(),
  byte_size(0),
  byte_offset(0),
  member_type(member_type),
  is_fragile(false)
  {
  }
};

struct CachedMemberInfo {
  std::vector<MemberInfo> member_infos;
};

struct EnumElementInfo {
  swift::Type clang_type;
  ConstString name;
  uint64_t byte_size;
  uint32_t value;         // The value for this enumeration element
  uint32_t extra_value;   // If not UINT32_MAX, then this value is an extra value
  // that appears at offset 0 to tell one or more empty
  // enums apart. This value will only be filled in if there
  // are one or more enum elements that have a non-zero byte size

  EnumElementInfo() :
  clang_type(),
  name(),
  byte_size(0),
  extra_value(UINT32_MAX)
  {
  }
};

class DeclsLookupSource {
public:
  typedef llvm::SmallVectorImpl<swift::ValueDecl *> ValueDecls;

private:
  class VisibleDeclsConsumer : public swift::VisibleDeclConsumer {
  private:
    std::vector<swift::ValueDecl *> m_decls;
  public:
    virtual void foundDecl(swift::ValueDecl *VD, swift::DeclVisibilityKind Reason)
    {
      m_decls.push_back(VD);
    }
    virtual ~VisibleDeclsConsumer() = default;
    explicit operator bool()
    {
      return m_decls.size() > 0;
    }

    decltype(m_decls)::const_iterator begin ()
    {
      return m_decls.begin();
    }

    decltype(m_decls)::const_iterator end ()
    {
      return m_decls.end();
    }
  };

  bool
  lookupQualified (swift::ModuleDecl* entry,
                   swift::Identifier name,
                   unsigned options,
                   swift::LazyResolver *typeResolver,
                   ValueDecls& decls) {
    if (!entry)
      return false;
    size_t decls_size = decls.size();
    entry->lookupQualified(swift::ModuleType::get(entry), name, options, typeResolver, decls);
    return decls.size() > decls_size;
  }

  bool
  lookupValue (swift::ModuleDecl* entry,
               swift::Identifier name,
               swift::Module::AccessPathTy accessPath,
               swift::NLKind lookupKind,
               ValueDecls &decls) {
    if (!entry)
      return false;
    size_t decls_size = decls.size();
    entry->lookupValue(accessPath, name, lookupKind, decls);
    return decls.size() > decls_size;
  }

public:
  enum class Type {
    SwiftModule,
    Crawler,
    Decl,
    Extension,
    Invalid
  };

  typedef llvm::Optional<std::string> PrivateDeclIdentifier;

  static DeclsLookupSource
  GetDeclsLookupSource (swift::ASTContext& ast,
                        ConstString module_name,
                        bool allow_crawler = true)
  {
    assert(!module_name.empty());
    static ConstString g_ObjectiveCModule(swift::MANGLING_MODULE_OBJC);
    static ConstString g_BuiltinModule("Builtin");
    static ConstString g_CModule(swift::MANGLING_MODULE_C);
    if (allow_crawler)
    {
      if (module_name == g_ObjectiveCModule || module_name == g_CModule)
        return DeclsLookupSource(&ast, module_name);
    }

    swift::ModuleDecl * module = module_name == g_BuiltinModule ?
                                    ast.TheBuiltinModule :
                                    ast.getModuleByName(module_name);
    if (module == nullptr)
      return DeclsLookupSource();
    return DeclsLookupSource(module);
  }

  static DeclsLookupSource
  GetDeclsLookupSource (swift::NominalTypeDecl *decl)
  {
    assert(decl);
    return DeclsLookupSource(decl);
  }

  static DeclsLookupSource
  GetDeclsLookupSource (DeclsLookupSource source, swift::NominalTypeDecl *decl)
  {
    assert(source._type == Type::SwiftModule);
    assert(source._module);
    assert(decl);
    return DeclsLookupSource(source._module, decl);
  }

  void lookupQualified(swift::Identifier name,
                       unsigned options,
                       swift::LazyResolver *typeResolver,
                       ValueDecls &result)
  {
    if (_type == Type::Crawler)
    {
      swift::ASTContext *ast_ctx = _crawler._ast;
      if (ast_ctx)
      {
        VisibleDeclsConsumer consumer;
        swift::ClangImporter *swift_clang_importer = (swift::ClangImporter *)
          ast_ctx->getClangModuleLoader();
        if (!swift_clang_importer)
          return;
        swift_clang_importer->lookupValue(name, consumer);
        if (consumer)
        {
          auto iter = consumer.begin(), end = consumer.end();
          while (iter != end)
          {
            result.push_back(*iter);
            iter++;
          }
          return;
        }
        else
        {
          const bool allow_crawler = false;
          if (_crawler._module)
            GetDeclsLookupSource(*ast_ctx, ConstString(_crawler._module),
                                 allow_crawler).lookupQualified(name, options, typeResolver, result);
        }
      }
    }
    else if (_type == Type::SwiftModule)
      lookupQualified(_module, name, options, typeResolver, result);
    return;
  }

  void lookupValue(swift::Module::AccessPathTy path,
                   swift::Identifier name,
                   swift::NLKind kind,
                   ValueDecls &result)
  {
    if (_type == Type::Crawler)
    {
      swift::ASTContext *ast_ctx = _crawler._ast;
      if (ast_ctx)
      {
        VisibleDeclsConsumer consumer;
        swift::ClangImporter *swift_clang_importer = (swift::ClangImporter *)
          ast_ctx->getClangModuleLoader();
        if (!swift_clang_importer)
          return;
        swift_clang_importer->lookupValue(name, consumer);
        if (consumer)
        {
          auto iter = consumer.begin(), end = consumer.end();
          while (iter != end)
          {
            result.push_back(*iter);
            iter++;
          }
          return;
        }
        else
        {
          const bool allow_crawler = false;
          if (_crawler._module)
            GetDeclsLookupSource(*ast_ctx, ConstString(_crawler._module),
                                 allow_crawler).lookupValue(path, name, kind,
                                                            result);
        }
      }
    }
    else if (_type == Type::SwiftModule)
      _module->lookupValue(path, name, kind, result);
    return;
  }

  void lookupMember (swift::DeclName id,
                     swift::Identifier priv_decl_id,
                     ValueDecls &result)
  {
    if (_type == Type::Decl)
      return lookupMember(_decl, id, priv_decl_id, result);
    if (_type == Type::SwiftModule)
      return lookupMember(_module, id, priv_decl_id, result);
    if (_type == Type::Extension)
      return lookupMember(_extension._decl, id, priv_decl_id, result);
    return;
  }

  void
  lookupMember (swift::DeclContext *decl_ctx,
                swift::DeclName id,
                swift::Identifier priv_decl_id,
                ValueDecls &result)
  {
    if (_type == Type::Decl)
      _decl->getModuleContext()->lookupMember(result, decl_ctx, id, priv_decl_id);
    else if (_type == Type::SwiftModule)
      _module->lookupMember(result, decl_ctx, id, priv_decl_id);
    else if (_type == Type::Extension)
      _extension._module->lookupMember(result, decl_ctx, id, priv_decl_id);
    return;
  }

  swift::TypeDecl *
  lookupLocalType (llvm::StringRef key)
  {
    switch (_type)
    {
      case Type::SwiftModule:
        return _module->lookupLocalType(key);
      case Type::Decl:
        return _decl->getModuleContext()->lookupLocalType(key);
      case Type::Extension:
        return _extension._module->lookupLocalType(key);
      case Type::Invalid:
        return nullptr;
      case Type::Crawler:
        return nullptr;
    }
  }

  ConstString
  GetName () const
  {
    switch(_type)
    {
      case Type::Invalid:
        return ConstString("Invalid");
      case Type::Crawler:
        return ConstString("Crawler");
      case Type::SwiftModule:
        return ConstString(_module->getName().get());
      case Type::Decl:
        return ConstString(_decl->getName().get());
      case Type::Extension:
        llvm::SmallString<64> builder;
        builder.append("ext ");
        builder.append(_extension._decl->getNameStr());
        builder.append(" in ");
        builder.append(_module->getNameStr());
        return builder.str();
    }
  }

  DeclsLookupSource (const DeclsLookupSource &rhs) :
  _type(rhs._type)
  {
    switch (_type)
    {
      case Type::Invalid:
        break;
      case Type::Crawler:
        _crawler._ast = rhs._crawler._ast;
        _crawler._module = rhs._crawler._module;
        break;
      case Type::SwiftModule:
        _module = rhs._module;
        break;
      case Type::Decl:
        _decl = rhs._decl;
        _extension._decl = rhs._extension._decl;
        _extension._module = rhs._extension._module;
        break;
      case Type::Extension:
        _extension._decl = rhs._extension._decl;
        _extension._module = rhs._extension._module;
        break;
    }
  }

  DeclsLookupSource&
  operator = (const DeclsLookupSource& rhs)
  {
    if (this != &rhs)
    {
      _type = rhs._type;
      switch (_type)
      {
        case Type::Invalid:
          break;
        case Type::Crawler:
          _crawler._ast = rhs._crawler._ast;
          _crawler._module = rhs._crawler._module;
          break;
        case Type::SwiftModule:
          _module = rhs._module;
          break;
        case Type::Decl:
          _decl = rhs._decl;
          _extension._decl = rhs._extension._decl;
          _extension._module = rhs._extension._module;
          break;
        case Type::Extension:
          _extension._decl = rhs._extension._decl;
          _extension._module = rhs._extension._module;
          break;
      }
    }
    return *this;
  }

  void
  Clear ()
  {
    // no need to explicitly clean either pointer
    _type = Type::Invalid;
  }

  DeclsLookupSource () :
  _type(Type::Invalid),
  _module(nullptr)
  {}

  operator bool ()
  {
    switch (_type)
    {
      case Type::Invalid:
        return false;
      case Type::Crawler:
        return _crawler._ast != nullptr;
      case Type::SwiftModule:
        return _module != nullptr;
      case Type::Decl:
        return _decl != nullptr;
      case Type::Extension:
        return (_extension._decl != nullptr) && (_extension._module != nullptr);
    }
  }

  bool
  IsExtension ()
  {
    return (this->operator bool()) && (_type == Type::Extension);
  }

private:
  Type _type;

  union {
    swift::ModuleDecl *_module;
    struct {
      swift::ASTContext* _ast;
      const char* _module;
    } _crawler;
    swift::NominalTypeDecl *_decl;
    struct {
      swift::ModuleDecl *_module; // extension in this module
      swift::NominalTypeDecl *_decl; // for this type
    } _extension;
  };

  DeclsLookupSource(swift::ModuleDecl* _m)
  {
    if (_m)
    {
      _module = _m;
      _type = Type::SwiftModule;
    }
    else
      _type = Type::Invalid;
  }

  DeclsLookupSource(swift::ASTContext* _a,
                    ConstString _m)
  {
    // it is fine for the ASTContext to be null, so don't actually even lldbassert there
    if (_a)
    {
      _crawler._ast = _a;
      _crawler._module = _m.data();
      _type = Type::Crawler;
    }
    else
      _type = Type::Invalid;
  }

  DeclsLookupSource (swift::NominalTypeDecl * _d)
  {
    if (_d)
    {
      _decl = _d;
      _type = Type::Decl;
    }
    else
      _type = Type::Invalid;
  }

  DeclsLookupSource (swift::ModuleDecl *_m,
                     swift::NominalTypeDecl * _d)
  {
    if (_m && _d)
    {
      _extension._decl = _d;
      _extension._module = _m;
      _type = Type::Extension;
    }
    else
      _type = Type::Invalid;
  }
};

struct VisitNodeResult {
  DeclsLookupSource _module;
  std::vector<swift::Decl *> _decls;
  std::vector<swift::Type> _types;
  swift::TupleTypeElt _tuple_type_element;
  std::string _error;
  VisitNodeResult () :
  _module(),
  _decls(),
  _types(),
  _tuple_type_element(),
  _error()
  {
  }

  bool
  HasSingleType ()
  {
    return _types.size() == 1 && _types.front();
  }

  bool
  HasSingleDecl ()
  {
    return _decls.size() == 1 && _decls.front();
  }

  swift::Type
  GetFirstType ()
  {
    // Must ensure there is a type prior to calling this
    return _types.front();
  }

  swift::Decl*
  GetFirstDecl ()
  {
    // Must ensure there is a decl prior to calling this
    return _decls.front();
  }

  void
  Clear()
  {
    _module.Clear();
    _decls.clear();
    _types.clear();
    _tuple_type_element = swift::TupleTypeElt();
    _error = "";
  }

  bool
  HasAnyDecls ()
  {
    return !_decls.empty();
  }

  bool
  HasAnyTypes ()
  {
    return !_types.empty();
  }
};

static swift::Identifier
GetIdentifier (SwiftASTContext *ast,
               const DeclsLookupSource::PrivateDeclIdentifier& priv_decl_id)
{
  do {
    if (!ast)
      break;
    if (!priv_decl_id.hasValue())
      break;
    return ast->getIdentifier(priv_decl_id.getValue().c_str());
  } while (false);
  return swift::Identifier();
}

static bool
FindFirstNamedDeclWithKind (SwiftASTContext *ast,
                            const llvm::StringRef &name,
                            swift::DeclKind decl_kind,
                            VisitNodeResult &result,
                            DeclsLookupSource::PrivateDeclIdentifier priv_decl_id = DeclsLookupSource::PrivateDeclIdentifier())

{
  if (!result._decls.empty())
  {
    swift::Decl *parent_decl = result._decls.back();
    if (parent_decl)
    {
      auto nominal_decl = llvm::dyn_cast<swift::NominalTypeDecl>(parent_decl);

      if (nominal_decl)
      {
        bool check_type_aliases = false;

        DeclsLookupSource lookup(DeclsLookupSource::GetDeclsLookupSource(nominal_decl));
        llvm::SmallVector<swift::ValueDecl *, 4> decls;
        lookup.lookupMember(ast->getIdentifier(name),
                            GetIdentifier(ast,priv_decl_id),
                            decls);

        for (auto decl : decls)
        {
          const swift::DeclKind curr_decl_kind = decl->getKind();

          if (curr_decl_kind == decl_kind)
          {
            result._decls.back() = decl;
            swift::Type decl_type;
            if (decl->hasType())
            {
              decl_type = decl->getType();
              swift::MetatypeType *meta_type = decl_type->getAs<swift::MetatypeType>();
              if (meta_type)
                decl_type = meta_type->getInstanceType();
            }
            if (result._types.empty())
              result._types.push_back(decl_type);
            else
              result._types.back() = decl_type;
            return true;
          } else if (curr_decl_kind == swift::DeclKind::TypeAlias)
            check_type_aliases = true;
        }

        if (check_type_aliases)
        {
          for (auto decl : decls)
          {
            const swift::DeclKind curr_decl_kind = decl->getKind();

            if (curr_decl_kind == swift::DeclKind::TypeAlias)
            {
              result._decls.back() = decl;
              swift::Type decl_type;
              if (decl->hasType())
              {
                decl_type = decl->getType();
                swift::MetatypeType *meta_type = decl_type->getAs<swift::MetatypeType>();
                if (meta_type)
                  decl_type = meta_type->getInstanceType();
              }
              if (result._types.empty())
                result._types.push_back(decl_type);
              else
                result._types.back() = decl_type;
              return true;
            }
          }
        }
      }
    }
  }
  else if (result._module)
  {
    swift::Module::AccessPathTy access_path;
    swift::Identifier name_ident(ast->getIdentifier(name));
    llvm::SmallVector<swift::ValueDecl*, 4> decls;
    if (priv_decl_id)
      result._module.lookupMember(name_ident, ast->getIdentifier(priv_decl_id.getValue().c_str()), decls);
    else
      result._module.lookupQualified(name_ident, 0,  NULL, decls);
    if (!decls.empty())
    {
      bool check_type_aliases = false;
      // Look for an exact match first
      for (auto decl : decls)
      {
        const swift::DeclKind curr_decl_kind = decl->getKind();
        if (curr_decl_kind == decl_kind)
        {
          result._decls.assign(1, decl);
          if (decl->hasType())
          {
            result._types.assign(1, decl->getType());
            swift::MetatypeType *meta_type = result._types.back()->getAs<swift::MetatypeType>();
            if (meta_type)
              result._types.back() = meta_type->getInstanceType();
          }
          else
          {
            result._types.assign(1, swift::Type());
          }
          return true;
        } else if (curr_decl_kind == swift::DeclKind::TypeAlias)
          check_type_aliases = true;
      }
      // If we didn't find any exact matches, accept any type aliases
      if (check_type_aliases)
      {
        for (auto decl : decls)
        {
          if (decl->getKind() == swift::DeclKind::TypeAlias)
          {
            result._decls.assign(1, decl);
            if (decl->hasType())
            {
              result._types.assign(1, decl->getType());
              swift::MetatypeType *meta_type = result._types.back()->getAs<swift::MetatypeType>();
              if (meta_type)
                result._types.back() = meta_type->getInstanceType();
            }
            else
            {
              result._types.assign(1, swift::Type());
            }
            return true;
          }
        }
      }
    }
  }
  result.Clear();
  result._error = "Generic Error";
  return false;
}

static size_t
FindNamedDecls (SwiftASTContext *ast,
                const llvm::StringRef &name,
                VisitNodeResult &result,
                DeclsLookupSource::PrivateDeclIdentifier priv_decl_id = DeclsLookupSource::PrivateDeclIdentifier())
{
  if (!result._decls.empty())
  {
    swift::Decl *parent_decl = result._decls.back();
    result._decls.clear();
    result._types.clear();
    if (parent_decl)
    {
      auto nominal_decl = llvm::dyn_cast<swift::NominalTypeDecl>(parent_decl);

      if (nominal_decl)
      {
        DeclsLookupSource lookup(DeclsLookupSource::GetDeclsLookupSource(nominal_decl));
        llvm::SmallVector<swift::ValueDecl *, 4> decls;
        lookup.lookupMember(ast->getIdentifier(name),
                            GetIdentifier(ast,priv_decl_id),
                            decls);
        if (decls.empty())
        {
          result._error = stringWithFormat("no decl found in '%s' (DeclKind=%u)",
                                           name.str().c_str(),
                                           nominal_decl->getName().get(),
                                           (uint32_t)nominal_decl->getKind());
        }
        else
        {
          for (swift::ValueDecl *decl : decls)
          {
            if (decl->hasType())
            {
              result._decls.push_back(decl);
              swift::Type decl_type;
              if (decl->hasType())
              {
                decl_type = decl->getType();
                swift::MetatypeType *meta_type = decl_type->getAs<swift::MetatypeType>();
                if (meta_type)
                  decl_type = meta_type->getInstanceType();
              }
              result._types.push_back(decl_type);
            }
          }
          return result._types.size();
        }
      }
      else
      {
        result._error = stringWithFormat("decl is not a nominal_decl (DeclKind=%u), lookup for '%s' failed",
                                         (uint32_t)parent_decl->getKind(),
                                         name.str().c_str());
      }
    }
  }
  else  if (result._module)
  {
    swift::Module::AccessPathTy access_path;
    llvm::SmallVector<swift::ValueDecl*, 4> decls;
    if (priv_decl_id)
      result._module.lookupMember(ast->getIdentifier(name),
                                  ast->getIdentifier(priv_decl_id.getValue().c_str()),
                                  decls);
    else
      result._module.lookupValue(access_path, ast->getIdentifier(name), swift::NLKind::QualifiedLookup, decls);
    if (decls.empty())
    {
      result._error = stringWithFormat("no decl named '%s' found in module '%s'",
                                       name.str().c_str(),
                                       result._module.GetName().data());
    }
    else
    {
      for (auto decl : decls)
      {
        if (decl->hasType())
        {
          result._decls.push_back(decl);
          if (decl->hasType())
          {
            result._types.push_back(decl->getType());
            swift::MetatypeType *meta_type = result._types.back()->getAs<swift::MetatypeType>();
            if (meta_type)
              result._types.back() = meta_type->getInstanceType();
          }
          else
          {
            result._types.push_back(swift::Type());
          }
        }
      }
      return result._types.size();
    }
  }
  result.Clear();
  result._error = "Generic error.";
  return false;
}

static const char *
SwiftDemangleNodeKindToCString(const swift::Demangle::Node::Kind node_kind)
{
#define NODE(e) case swift::Demangle::Node::Kind::e: return #e;

  switch (node_kind)
  {
#include "swift/Basic/DemangleNodes.def"
  }
  return "swift::Demangle::Node::Kind::???";
#undef NODE
}

static
swift::DeclKind
GetKindAsDeclKind (swift::Demangle::Node::Kind node_kind)
{
  switch (node_kind)
  {
    case swift::Demangle::Node::Kind::TypeAlias:
      return swift::DeclKind::TypeAlias;
    case swift::Demangle::Node::Kind::Structure:
      return swift::DeclKind::Struct;
    case swift::Demangle::Node::Kind::Class:
      return swift::DeclKind::Class;
    case swift::Demangle::Node::Kind::Allocator:
      return swift::DeclKind::Constructor;
    case swift::Demangle::Node::Kind::Function:
      return swift::DeclKind::Func;
    case swift::Demangle::Node::Kind::Enum:
      return swift::DeclKind::Enum;
    case swift::Demangle::Node::Kind::Protocol:
      return swift::DeclKind::Protocol;
    default:
      printf ("Missing alias for %s.\n", SwiftDemangleNodeKindToCString(node_kind));
      assert (0);
  }
}

// This should be called with a function type & its associated Decl.  If the type is not a function type,
// then we just return the original type, but we don't check that the Decl is the associated one.
// It is assumed you will get that right in calling this.
// Returns a version of the input type with the ExtInfo AbstractCC set correctly.
// This allows CompilerType::IsSwiftMethod to work properly off the swift Type.
// FIXME: we don't currently distinguish between Method & Witness.  These types don't actually get used
// to make Calling Convention choices - originally we were leaving them all at Normal...  But if we ever
// need to set it for that purpose we will have to fix that here.
static swift::TypeBase *
FixCallingConv (swift::Decl *in_decl, swift::TypeBase *in_type)
{
  if (!in_decl)
    return in_type;

  swift::AnyFunctionType *func_type = llvm::dyn_cast<swift::AnyFunctionType>(in_type);
  if (func_type)
  {
    swift::DeclContext *decl_context = in_decl->getDeclContext();
    if (decl_context && decl_context->isTypeContext())
    {
      // Add the ExtInfo:
      swift::AnyFunctionType::ExtInfo new_info(func_type->getExtInfo().withSILRepresentation(swift::SILFunctionTypeRepresentation::Method));
      return func_type->withExtInfo(new_info);
    }
  }
  return in_type;
}

static void
VisitNode (SwiftASTContext *ast,
           std::vector<swift::Demangle::NodePointer> &nodes,
           VisitNodeResult &result,
           const VisitNodeResult &generic_context, // set by GenericType case
           Log *log);


static void
VisitNodeAddressor (SwiftASTContext *ast,
                    std::vector<swift::Demangle::NodePointer> &nodes,
                    swift::Demangle::NodePointer& cur_node,
                    VisitNodeResult &result,
                    const VisitNodeResult &generic_context, // set by GenericType case
                    Log *log)
{
  // Addressors are apparently SIL-level functions of the form () -> RawPointer and they bear no connection to their original variable at the interface level
  swift::CanFunctionType swift_can_func_type = swift::CanFunctionType::get(ast->TheEmptyTupleType, ast->TheRawPointerType);
  result._types.push_back(swift_can_func_type.getPointer());
}

static void
VisitNodeGenerics (SwiftASTContext *ast,
                   std::vector<swift::Demangle::NodePointer> &nodes,
                   swift::Demangle::NodePointer& cur_node,
                   VisitNodeResult &result,
                   const VisitNodeResult &generic_context, // set by GenericType case
                   Log *log)
{
  llvm::SmallVector<swift::Type, 4> nested_types;
  VisitNodeResult associated_type_result;
  VisitNodeResult archetype_ref_result;
  VisitNodeResult archetype_type_result;
  for (swift::Demangle::Node::iterator pos = cur_node->begin(), end = cur_node->end(); pos != end; ++pos)
  {
    const swift::Demangle::Node::Kind child_node_kind = (*pos)->getKind();
    switch (child_node_kind)
    {
      case swift::Demangle::Node::Kind::ArchetypeRef:
        nodes.push_back(*pos);
        VisitNode (ast, nodes, archetype_ref_result, generic_context, log);
        break;
      case swift::Demangle::Node::Kind::Archetype:
        nodes.push_back(*pos);
        VisitNode (ast, nodes, archetype_type_result, generic_context, log);
        break;
      case swift::Demangle::Node::Kind::AssociatedType:
        nodes.push_back(*pos);
        VisitNode (ast, nodes, associated_type_result, generic_context, log);
        if (associated_type_result.HasSingleType())
          nested_types.push_back(associated_type_result.GetFirstType());
        break;
      default:
        result._error = stringWithFormat("%s encountered in generics children",
                                         SwiftDemangleNodeKindToCString(child_node_kind));
        break;
    }
  }

  if (archetype_ref_result.HasAnyDecls() || archetype_ref_result.HasAnyTypes())
    result = archetype_ref_result;
  else
    result = archetype_type_result;
}

static void
VisitNodeArchetype(SwiftASTContext *ast,
                   std::vector<swift::Demangle::NodePointer> &nodes,
                   swift::Demangle::NodePointer& cur_node,
                   VisitNodeResult &result,
                   const VisitNodeResult &generic_context, // set by GenericType case
                   Log *log)
{
  const llvm::StringRef& archetype_name(cur_node->getText());
  VisitNodeResult protocol_list;
  for (swift::Demangle::Node::iterator pos = cur_node->begin(), end = cur_node->end(); pos != end; ++pos)
  {
    const swift::Demangle::Node::Kind child_node_kind = (*pos)->getKind();
    switch (child_node_kind)
    {
      case swift::Demangle::Node::Kind::ProtocolList:
        nodes.push_back(*pos);
        VisitNode (ast, nodes, protocol_list, generic_context, log);
        break;
      default:
        result._error = stringWithFormat("%s encountered in generics children",
                                         SwiftDemangleNodeKindToCString(child_node_kind));
        break;
    }
  }

  llvm::SmallVector<swift::Type, 1> conforms_to;
  if (protocol_list.HasSingleType())
    conforms_to.push_back(protocol_list.GetFirstType());

  if (ast)
  {
    result._types.push_back(swift::ArchetypeType::getNew(*ast,
                                                         nullptr,
                                                         (swift::AssociatedTypeDecl *)nullptr,
                                                         ast->getIdentifier(archetype_name),
                                                         conforms_to, swift::Type()));
  }
  else
  {
    result._error = "invalid ASTContext";
  }
}


static void
VisitNodeArchetypeRef(SwiftASTContext *ast,
                      std::vector<swift::Demangle::NodePointer> &nodes,
                      swift::Demangle::NodePointer& cur_node,
                      VisitNodeResult &result,
                      const VisitNodeResult &generic_context, // set by GenericType case
                      Log *log)
{
  const llvm::StringRef& archetype_name(cur_node->getText());
  swift::Type result_type;
  for (const swift::Type &archetype : generic_context._types)
  {
    const swift::ArchetypeType *cast_archetype = llvm::dyn_cast<swift::ArchetypeType>(archetype.getPointer());

    if (cast_archetype && !cast_archetype->getName().str().compare(archetype_name))
    {
      result_type = archetype;
      break;
    }
  }

  if (result_type)
    result._types.push_back(result_type);
  else
  {
    if (ast)
    {
      result._types.push_back(swift::ArchetypeType::getNew(*ast,
                                                           nullptr,
                                                           (swift::AssociatedTypeDecl *)nullptr,
                                                           ast->getIdentifier(archetype_name),
                                                           llvm::ArrayRef<swift::Type>(), swift::Type()));
    }
    else
    {
      result._error = "invalid ASTContext";
    }
  }
}

static void
VisitNodeAssociatedTypeRef (SwiftASTContext *ast,
                            std::vector<swift::Demangle::NodePointer> &nodes,
                            swift::Demangle::NodePointer& cur_node,
                            VisitNodeResult &result,
                            const VisitNodeResult &generic_context, // set by GenericType case
                            Log *log)
{
  swift::Demangle::NodePointer root = cur_node->getChild(0);
  swift::Demangle::NodePointer ident = cur_node->getChild(1);
  if (!root || !ident)
    return;
  nodes.push_back(root);
  VisitNodeResult type_result;
  VisitNode (ast, nodes, type_result, generic_context, log);
  if (type_result._types.size() == 1)
  {
    swift::TypeBase* type = type_result._types[0].getPointer();
    if (type)
    {
      swift::ArchetypeType* archetype = type->getAs<swift::ArchetypeType>();
      if (archetype)
      {
        swift::Identifier identifier = ast->getIdentifier(ident->getText());
        if (archetype->hasNestedType(identifier))
        {
          swift::Type nested = archetype->getNestedTypeValue(identifier);
          if (nested)
          {
            result._types.push_back(nested);
            result._module = type_result._module;
            return;
          }
        }
      }
    }
  }
  result._types.clear();
  result._error = stringWithFormat("unable to find associated type %s in context", ident->getText().c_str());
}

static void
VisitNodeBoundGeneric (SwiftASTContext *ast,
                       std::vector<swift::Demangle::NodePointer> &nodes,
                       swift::Demangle::NodePointer& cur_node,
                       VisitNodeResult &result,
                       const VisitNodeResult &generic_context, // set by GenericType case
                       Log *log)
{
  if (cur_node->begin() != cur_node->end())
  {
    VisitNodeResult generic_type_result;
    VisitNodeResult template_types_result;

    swift::Demangle::Node::iterator end = cur_node->end();
    for (swift::Demangle::Node::iterator pos = cur_node->begin(); pos != end; ++pos)
    {
      const swift::Demangle::Node::Kind child_node_kind = (*pos)->getKind();
      switch (child_node_kind)
      {
        case swift::Demangle::Node::Kind::Type:
        case swift::Demangle::Node::Kind::Metatype:
          nodes.push_back(*pos);
          VisitNode (ast, nodes, generic_type_result, generic_context, log);
          break;
        case swift::Demangle::Node::Kind::TypeList:
          nodes.push_back(*pos);
          VisitNode (ast, nodes, template_types_result, generic_context, log);
          break;
        default:
          break;
      }
    }

    if (generic_type_result._types.size() == 1 && !template_types_result._types.empty())
    {
      swift::NominalTypeDecl *nominal_type_decl = llvm::dyn_cast<swift::NominalTypeDecl>(generic_type_result._decls.front());
      swift::DeclContext * parent_decl = nominal_type_decl->getParent();
      swift::Type parent_type;
      if (parent_decl->isTypeContext())
        parent_type = parent_decl->getDeclaredTypeOfContext();
      result._types.push_back(swift::Type(swift::BoundGenericType::get(nominal_type_decl,
                                                                       parent_type,
                                                                       template_types_result._types)));

    }
  }
}

static void
VisitNodeBuiltinTypeName (SwiftASTContext *ast,
                          std::vector<swift::Demangle::NodePointer> &nodes,
                          swift::Demangle::NodePointer& cur_node,
                          VisitNodeResult &result,
                          const VisitNodeResult &generic_context, // set by GenericType case
                          Log *log)
{
  std::string builtin_name = cur_node->getText();

  llvm::StringRef builtin_name_ref(builtin_name);

  if (builtin_name_ref.startswith("Builtin."))
  {
    llvm::StringRef stripped_name_ref = builtin_name_ref.drop_front(strlen("Builtin."));
    llvm::SmallVector<swift::ValueDecl *, 1> builtin_decls;

    result._module = DeclsLookupSource::GetDeclsLookupSource(*ast, ConstString("Builtin"));

    if (!FindNamedDecls(ast, stripped_name_ref, result))
    {
      result.Clear();
      result._error = stringWithFormat("Couldn't find %s in the builtin module",
                                       builtin_name.c_str());
    }
  }
  else
  {
    result._error = stringWithFormat("BuiltinTypeName %s doesn't start with Builtin.",
                                     builtin_name.c_str());
  }
}

static void
VisitNodeConstructor (SwiftASTContext *ast,
                      std::vector<swift::Demangle::NodePointer> &nodes,
                      swift::Demangle::NodePointer& cur_node,
                      VisitNodeResult &result,
                      const VisitNodeResult &generic_context, // set by GenericType case
                      Log *log)
{
  VisitNodeResult kind_type_result;
  VisitNodeResult type_result;

  swift::Demangle::Node::iterator end = cur_node->end();
  for (swift::Demangle::Node::iterator pos = cur_node->begin(); pos != end; ++pos)
  {
    const swift::Demangle::Node::Kind child_node_kind = (*pos)->getKind();
    switch (child_node_kind)
    {
      case swift::Demangle::Node::Kind::Enum:
      case swift::Demangle::Node::Kind::Class:
      case swift::Demangle::Node::Kind::Structure:
        nodes.push_back(*pos);
        VisitNode (ast, nodes, kind_type_result, generic_context, log);
        break;
      case swift::Demangle::Node::Kind::Type:
        nodes.push_back(*pos);
        VisitNode (ast, nodes, type_result, generic_context, log);
        break;
      default:
        break;
    }
  }

  if (kind_type_result.HasSingleType() && type_result.HasSingleType())
  {
    bool found = false;
    const size_t n = FindNamedDecls(ast, llvm::StringRef("init"), kind_type_result);
    if (n == 1)
    {
      found = true;
      kind_type_result._types[0] = FixCallingConv(kind_type_result._decls[0], kind_type_result._types[0].getPointer());
      result = kind_type_result;
    }
    else if (n > 0)
    {
      const size_t num_kind_type_results = kind_type_result._types.size();
      for (size_t i=0; i<num_kind_type_results && !found; ++i)
      {
        auto &identifier_type = kind_type_result._types[i];
        if (identifier_type && identifier_type->getKind() == type_result._types.front()->getKind())
        {
          // These are the same kind of type, we need to disambiguate them
          switch (identifier_type->getKind())
          {
            default:
              break;
            case swift::TypeKind::Function:
            {
              const swift::AnyFunctionType* identifier_func = identifier_type->getAs<swift::AnyFunctionType>();
              const swift::AnyFunctionType* type_func = type_result._types.front()->getAs<swift::AnyFunctionType>();
              if (swift::CanType(identifier_func->getResult()->getDesugaredType()->getCanonicalType()) == swift::CanType(type_func->getResult()->getDesugaredType()->getCanonicalType()) &&
                  swift::CanType(identifier_func->getInput()->getDesugaredType()->getCanonicalType()) == swift::CanType(type_func->getInput()->getDesugaredType()->getCanonicalType()))
              {
                result._module = kind_type_result._module;
                result._decls.push_back(kind_type_result._decls[i]);
                result._types.push_back(FixCallingConv(kind_type_result._decls[i], kind_type_result._types[i].getPointer()));
                found = true;
              }
            }
              break;
          }
        }
      }
    }
    // Didn't find a match, just return the raw function type
    if (!found)
      result = type_result;

  }
}

static void
VisitNodeDestructor (SwiftASTContext *ast,
                     std::vector<swift::Demangle::NodePointer> &nodes,
                     swift::Demangle::NodePointer& cur_node,
                     VisitNodeResult &result,
                     const VisitNodeResult &generic_context, // set by GenericType case
                     Log *log)
{
  VisitNodeResult kind_type_result;

  swift::Demangle::Node::iterator end = cur_node->end();
  for (swift::Demangle::Node::iterator pos = cur_node->begin(); pos != end; ++pos)
  {
    const swift::Demangle::Node::Kind child_node_kind = (*pos)->getKind();
    switch (child_node_kind)
    {
      case swift::Demangle::Node::Kind::Enum:
      case swift::Demangle::Node::Kind::Class:
      case swift::Demangle::Node::Kind::Structure:
        nodes.push_back(*pos);
        VisitNode (ast, nodes, kind_type_result, generic_context, log);
        break;
      default:
        break;
    }
  }

  if (kind_type_result.HasSingleType())
  {
    bool found = false;
    const size_t n = FindNamedDecls(ast, llvm::StringRef("deinit"), kind_type_result);
    if (n == 1)
    {
      found = true;
      kind_type_result._types[0] = FixCallingConv(kind_type_result._decls[0],
                                                  kind_type_result._types[0].getPointer());
      result = kind_type_result;
    }
    else if (n > 0)
    {
      // I can't think of a reason why we would get more than one decl called deinit here, but
      // just in case, if it is a function type, we should remember it.
      const size_t num_kind_type_results = kind_type_result._types.size();
      for (size_t i=0; i<num_kind_type_results && !found; ++i)
      {
        auto &identifier_type = kind_type_result._types[i];
        if (identifier_type)
        {
          switch (identifier_type->getKind())
          {
            default:
              break;
            case swift::TypeKind::Function:
            {
              result._module = kind_type_result._module;
              result._decls.push_back(kind_type_result._decls[i]);
              result._types.push_back(FixCallingConv(kind_type_result._decls[i],
                                                     kind_type_result._types[i].getPointer()));
              found = true;
            }
              break;
          }
        }
      }
    }
  }
}

static void
VisitNodeDeclContext (SwiftASTContext *ast,
                      std::vector<swift::Demangle::NodePointer> &nodes,
                      swift::Demangle::NodePointer& cur_node,
                      VisitNodeResult &result,
                      const VisitNodeResult &generic_context, // set by GenericType case
                      Log *log)
{
  switch (cur_node->getNumChildren())
  {
    default:
      result._error = stringWithFormat("DeclContext had %llu children, giving up",
                                       (unsigned long long)cur_node->getNumChildren());
      break;
    case 0:
      result._error = "empty DeclContext unusable";
      break;
    case 1:
      // nominal type
      nodes.push_back(cur_node->getFirstChild());
      VisitNode (ast, nodes, result, generic_context, log);
      break;
    case 2:
      // function type: decl-ctx + type
      // FIXME: we should just be able to demangle the DeclCtx and resolve the function
      // this is fragile and will easily break
      swift::Demangle::NodePointer path = cur_node->getFirstChild();
      nodes.push_back(path);
      VisitNodeResult found_decls;
      VisitNode (ast, nodes, found_decls, generic_context, log);
      swift::Demangle::NodePointer generics = cur_node->getChild(1);
      if (generics->getChild(0) == nullptr)
        break;
      generics = generics->getFirstChild();
      if (generics->getKind() != swift::Demangle::Node::Kind::GenericType)
        break;
      if (generics->getChild(0) == nullptr)
        break;
      generics = generics->getFirstChild();
      //                        if (generics->getKind() != swift::Demangle::Node::Kind::ArchetypeList)
      //                            break;
      swift::AbstractFunctionDecl *func_decl = nullptr;
      for (swift::Decl* decl : found_decls._decls)
      {
        func_decl = llvm::dyn_cast<swift::AbstractFunctionDecl>(decl);
        if (!func_decl)
          continue;
        swift::GenericParamList *gen_params = func_decl->getGenericParams();
        if (!gen_params)
          continue;
      }
      if (func_decl)
      {
        result._module = found_decls._module;
        result._decls.push_back(func_decl);
        result._types.push_back(func_decl->getType().getPointer());
      }
      else
        result._error = "could not find a matching function for the DeclContext";
      break;
  }
}

static void
VisitNodeExplicitClosure (SwiftASTContext *ast,
                          std::vector<swift::Demangle::NodePointer> &nodes,
                          swift::Demangle::NodePointer& cur_node,
                          VisitNodeResult &result,
                          const VisitNodeResult &generic_context, // set by GenericType case
                          Log *log)
{
  // FIXME: closures are mangled as hanging off a function, but they really aren't
  // so we cannot really do a lot about them, other than make a function type
  // for whatever their advertised type is, and cross fingers
  VisitNodeResult function_result;
  uint64_t index = UINT64_MAX;
  VisitNodeResult closure_type_result;
  VisitNodeResult module_result;
  swift::Demangle::Node::iterator end = cur_node->end();
  for (swift::Demangle::Node::iterator pos = cur_node->begin(); pos != end; ++pos)
  {
    const swift::Demangle::Node::Kind child_node_kind = (*pos)->getKind();
    switch (child_node_kind)
    {
      default:
        result._error = stringWithFormat("%s encountered in ExplicitClosure children",
                                         SwiftDemangleNodeKindToCString(child_node_kind));
        break;
      case swift::Demangle::Node::Kind::Module:
        nodes.push_back((*pos));
        VisitNode (ast, nodes, module_result, generic_context, log);
        break;
      case swift::Demangle::Node::Kind::Function:
        nodes.push_back((*pos));
        VisitNode (ast, nodes, function_result, generic_context, log);
        break;
      case swift::Demangle::Node::Kind::Number:
        index = (*pos)->getIndex();
        break;
      case swift::Demangle::Node::Kind::Type:
        nodes.push_back((*pos));
        VisitNode (ast, nodes, closure_type_result, generic_context, log);
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

static void
VisitNodeExtension (SwiftASTContext *ast,
                    std::vector<swift::Demangle::NodePointer> &nodes,
                    swift::Demangle::NodePointer& cur_node,
                    VisitNodeResult &result,
                    const VisitNodeResult &generic_context, // set by GenericType case
                    Log *log)
{
  VisitNodeResult module_result;
  VisitNodeResult type_result;
  std::string error;
  swift::Demangle::Node::iterator end = cur_node->end();
  for (swift::Demangle::Node::iterator pos = cur_node->begin(); pos != end; ++pos)
  {
    const swift::Demangle::Node::Kind child_node_kind = (*pos)->getKind();
    switch (child_node_kind)
    {
      default:
        result._error = stringWithFormat("%s encountered in extension children", SwiftDemangleNodeKindToCString(child_node_kind));
        break;

      case swift::Demangle::Node::Kind::Module:
        nodes.push_back((*pos));
        VisitNode(ast, nodes, module_result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::Class:
      case swift::Demangle::Node::Kind::Enum:
      case swift::Demangle::Node::Kind::Structure:
        nodes.push_back((*pos));
        VisitNode(ast, nodes, type_result, generic_context, log);
        break;
    }
  }

  if (module_result._module)
  {
    if (type_result._decls.size() == 1)
    {
      swift::Decl *decl = type_result._decls[0];
      swift::NominalTypeDecl *nominal_decl = llvm::dyn_cast_or_null<swift::NominalTypeDecl>(decl);
      if (nominal_decl)
      {
        result._module = DeclsLookupSource::GetDeclsLookupSource(module_result._module, nominal_decl);
      }
      else
        result._error = "unable to find nominal type for extension";
    }
    else
      result._error = "unable to find unique type for extension";
  }
  else
    result._error = "unable to find module name for extension";
}

static bool
AreBothFunctionTypes (swift::TypeKind a,
                      swift::TypeKind b)
{
  bool is_first = false, is_second = false;
  if (a >= swift::TypeKind::First_AnyFunctionType &&
      a <= swift::TypeKind::Last_AnyFunctionType)
    is_first = true;
  if (b >= swift::TypeKind::First_AnyFunctionType &&
      b <= swift::TypeKind::Last_AnyFunctionType)
    is_second = true;
  return (is_first && is_second);
}

static bool
CompareFunctionTypes (const swift::AnyFunctionType *f,
                      const swift::AnyFunctionType *g,
                      bool *input_matches = nullptr,
                      bool *output_matches = nullptr)
{
  bool in_matches = false, out_matches = false;
  if (nullptr == f)
    return (nullptr == g);
  if (nullptr == g)
    return false;

  auto f_input = f->getInput().getCanonicalTypeOrNull();
  auto g_input = g->getInput().getCanonicalTypeOrNull();

  auto f_output = f->getResult().getCanonicalTypeOrNull();
  auto g_output = g->getResult().getCanonicalTypeOrNull();

  if (f_input == g_input)
  {
    in_matches = true;
    if (f_output == g_output)
      out_matches = true;
  }

  if (input_matches)
    *input_matches = in_matches;
  if (output_matches)
    *output_matches = out_matches;

  return (in_matches && out_matches);
}



// VisitNodeFunction gets used for Function, Variable and Allocator:
static void
VisitNodeFunction (SwiftASTContext *ast,
                   std::vector<swift::Demangle::NodePointer> &nodes,
                   swift::Demangle::NodePointer& cur_node,
                   VisitNodeResult &result,
                   const VisitNodeResult &generic_context, // set by GenericType case
                   Log *log)
{
  VisitNodeResult identifier_result;
  VisitNodeResult type_result;
  VisitNodeResult decl_scope_result;
  swift::Demangle::Node::iterator end = cur_node->end();
  bool found_univocous = false;
  for (swift::Demangle::Node::iterator pos = cur_node->begin(); pos != end; ++pos)
  {
    if (found_univocous)
      break;
    const swift::Demangle::Node::Kind child_node_kind = (*pos)->getKind();
    switch (child_node_kind)
    {
      default:
        result._error = stringWithFormat("%s encountered in function children",
                                         SwiftDemangleNodeKindToCString(child_node_kind));
        break;

        // TODO: any other possible containers?
      case swift::Demangle::Node::Kind::Class:
      case swift::Demangle::Node::Kind::Enum:
      case swift::Demangle::Node::Kind::Module:
      case swift::Demangle::Node::Kind::Structure:
        nodes.push_back((*pos));
        VisitNode (ast, nodes, decl_scope_result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::Identifier:
      case swift::Demangle::Node::Kind::InfixOperator:
      case swift::Demangle::Node::Kind::PrefixOperator:
      case swift::Demangle::Node::Kind::PostfixOperator:
        FindNamedDecls(ast, (*pos)->getText(), decl_scope_result);
        if (decl_scope_result._decls.size() == 0)
        {
          result._error = stringWithFormat("demangled identifier %s could not be found by name lookup",
                                           (*pos)->getText().c_str());
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

      case swift::Demangle::Node::Kind::Type:
        nodes.push_back((*pos));
        VisitNode (ast, nodes, type_result, generic_context, log);
        break;
    }
  }

  //                    if (node_kind == swift::Demangle::Node::Kind::Allocator)
  //                    {
  //                        // For allocators we don't have an identifier for the name, we will
  //                        // need to extract it from the class or struct in "identifier_result"
  //                        //Find
  //                        if (identifier_result.HasSingleType())
  //                        {
  //                            // This contains the class or struct
  //                            llvm::StringRef init_name("init");
  //
  //                            if (FindFirstNamedDeclWithKind(ast, init_name, swift::DeclKind::Constructor, identifier_result))
  //                            {
  //                            }
  //                        }
  //                    }

  if (identifier_result._types.size() == 1)
  {
    result._module = identifier_result._module;
    result._decls.push_back(identifier_result._decls[0]);
    result._types.push_back(FixCallingConv(identifier_result._decls[0], identifier_result._types[0].getPointer()));
  }
  else if (type_result.HasSingleType())
  {
    const size_t num_identifier_results = identifier_result._types.size();
    bool found = false;
    for (size_t i=0; i<num_identifier_results && !found; ++i)
    {
      auto &identifier_type = identifier_result._types[i];
      if (!identifier_type)
        continue;
      if (AreBothFunctionTypes(identifier_type->getKind(), type_result._types.front()->getKind()))
      {
        const swift::AnyFunctionType* identifier_func = identifier_type->getAs<swift::AnyFunctionType>();
        const swift::AnyFunctionType* type_func = type_result._types.front()->getAs<swift::AnyFunctionType>();
        if (CompareFunctionTypes(identifier_func, type_func))
        {
          result._module = identifier_result._module;
          result._decls.push_back(identifier_result._decls[i]);
          result._types.push_back(FixCallingConv(identifier_result._decls[i], identifier_result._types[i].getPointer()));
          found = true;
        }
      }
    }
    // Didn't find a match, just return the raw function type
    if (!found)
      result = type_result;
  }
}

static void
VisitNodeFunctionType (SwiftASTContext *ast,
                       std::vector<swift::Demangle::NodePointer> &nodes,
                       swift::Demangle::NodePointer& cur_node,
                       VisitNodeResult &result,
                       const VisitNodeResult &generic_context, // set by GenericType case
                       Log *log)
{
  VisitNodeResult arg_type_result;
  VisitNodeResult return_type_result;
  swift::Demangle::Node::iterator end = cur_node->end();
  bool is_in_class = false;
  bool throws = false;
  for (swift::Demangle::Node::iterator pos = cur_node->begin(); pos != end; ++pos)
  {
    const swift::Demangle::Node::Kind child_node_kind = (*pos)->getKind();
    switch (child_node_kind)
    {
      case swift::Demangle::Node::Kind::Class:
      {
        is_in_class = true;
        VisitNodeResult class_type_result;
        nodes.push_back(*pos);
        VisitNode (ast, nodes, class_type_result, generic_context, log);
      }
        break;
      case swift::Demangle::Node::Kind::Structure:
      {
        VisitNodeResult class_type_result;
        nodes.push_back(*pos);
        VisitNode (ast, nodes, class_type_result, generic_context, log);
      }
        break;
      case swift::Demangle::Node::Kind::ArgumentTuple:
      case swift::Demangle::Node::Kind::Metatype:
      {
        nodes.push_back(*pos);
        VisitNode (ast, nodes, arg_type_result, generic_context, log);
      }
        break;
      case swift::Demangle::Node::Kind::ThrowsAnnotation:
        throws = true;
        break;
      case swift::Demangle::Node::Kind::ReturnType:
      {
        nodes.push_back(*pos);
        VisitNode (ast, nodes, return_type_result, generic_context, log);
      }
        break;
      default:
        break;
    }
  }
  swift::Type arg_clang_type;
  swift::Type return_clang_type;

  switch (arg_type_result._types.size())
  {
    case 0:
      arg_clang_type = swift::TupleType::getEmpty(*ast);
      break;
    case 1:
      arg_clang_type = arg_type_result._types.front().getPointer();
      break;
    default:
      result._error = "too many argument types for a function type";
      break;
  }

  switch (return_type_result._types.size())
  {
    case 0:
      return_clang_type = swift::TupleType::getEmpty(*ast);
      break;
    case 1:
      return_clang_type = return_type_result._types.front().getPointer();
      break;
    default:
      result._error = "too many return types for a function type";
      break;
  }

  if (arg_clang_type && return_clang_type)
  {
    result._types.push_back(swift::FunctionType::get(arg_clang_type,
                                                     return_clang_type,
                                                     swift::FunctionType::ExtInfo().
                                                       withThrows(throws)));
  }
}

static void
VisitNodeGenericType (SwiftASTContext *ast,
                      std::vector<swift::Demangle::NodePointer> &nodes,
                      swift::Demangle::NodePointer& cur_node,
                      VisitNodeResult &result,
                      const VisitNodeResult &generic_context, // set by GenericType case
                      Log *log)
{
  VisitNodeResult new_generic_context;
  std::copy(generic_context._types.begin(), generic_context._types.end(), back_inserter(new_generic_context._types));
  for (swift::Demangle::Node::iterator pos = cur_node->begin(), end = cur_node->end(); pos != end; ++pos)
  {
    const swift::Demangle::Node::Kind child_node_kind = (*pos)->getKind();
    switch (child_node_kind)
    {
      case swift::Demangle::Node::Kind::Generics:
        nodes.push_back(*pos);
        VisitNode (ast, nodes, new_generic_context, generic_context, log);
        break;
      case swift::Demangle::Node::Kind::Type:
        nodes.push_back(*pos);
        VisitNode (ast, nodes, result, new_generic_context, log);
        break;
      default:
        result._error = stringWithFormat("%s encountered in generic type children", SwiftDemangleNodeKindToCString(child_node_kind));
        break;
    }
  }
}

static void
VisitNodeSetterGetter (SwiftASTContext *ast,
                       std::vector<swift::Demangle::NodePointer> &nodes,
                       swift::Demangle::NodePointer& cur_node,
                       VisitNodeResult &result,
                       const VisitNodeResult &generic_context, // set by GenericType case
                       Log *log)
{
  VisitNodeResult decl_ctx_result;
  std::string identifier;
  VisitNodeResult type_result;
  swift::Demangle::Node::Kind node_kind = cur_node->getKind();

  for (swift::Demangle::Node::iterator pos = cur_node->begin(), end = cur_node->end(); pos != end; ++pos)
  {
    const swift::Demangle::Node::Kind child_node_kind = (*pos)->getKind();
    switch (child_node_kind)
    {
      case swift::Demangle::Node::Kind::Class:
      case swift::Demangle::Node::Kind::Module:
      case swift::Demangle::Node::Kind::Structure:
        nodes.push_back(*pos);
        VisitNode (ast, nodes, decl_ctx_result, generic_context, log);
        break;
      case swift::Demangle::Node::Kind::Identifier:
        identifier.assign((*pos)->getText());
        break;
      case swift::Demangle::Node::Kind::Type:
        nodes.push_back(*pos);
        VisitNode (ast, nodes, type_result, generic_context, log);
        break;
      default:
        result._error = stringWithFormat("%s encountered in generic type children", SwiftDemangleNodeKindToCString(child_node_kind));
        break;
    }
  }

  if (identifier == "subscript")
  {
    // Subscript setters and getters are named with the reserved word "subscript".
    // Since there can be many subscripts for the same nominal type, we need to
    // find the one matching the specified type.

    FindNamedDecls (ast, identifier, decl_ctx_result);
    size_t num_decls = decl_ctx_result._decls.size();

    if (num_decls == 0)
    {
      result._error = "Could not find a subscript decl";
      return;
    }

    swift::SubscriptDecl *subscript_decl;
    const swift::AnyFunctionType* type_func = type_result._types.front()->getAs<swift::AnyFunctionType>();

    swift::CanType type_result_type(type_func->getResult()->getDesugaredType()->getCanonicalType());
    swift::CanType type_input_type(type_func->getInput()->getDesugaredType()->getCanonicalType());


    swift::FuncDecl *identifier_func = nullptr;

    for (size_t i = 0; i < num_decls; i++)
    {
      subscript_decl = llvm::dyn_cast_or_null<swift::SubscriptDecl>(decl_ctx_result._decls[i]);
      if (subscript_decl)
      {
        switch (node_kind)
        {
          case swift::Demangle::Node::Kind::Getter:
            identifier_func = subscript_decl->getGetter();
            break;
          case swift::Demangle::Node::Kind::Setter:
            identifier_func = subscript_decl->getGetter();
            break;
          case swift::Demangle::Node::Kind::DidSet:
            identifier_func = subscript_decl->getDidSetFunc();
            break;
          case swift::Demangle::Node::Kind::WillSet:
            identifier_func = subscript_decl->getWillSetFunc();
            break;
          default:
            identifier_func = nullptr;
            break;
        }

        if (identifier_func && identifier_func->getType())
        {
          const swift::AnyFunctionType *identifier_func_type = identifier_func->getType()->getAs<swift::AnyFunctionType>();
          if (identifier_func_type)
          {
            // Swift function types are formally functions that take the class and return the method,
            // we have to strip off the first level of function call to compare against the type
            // from the demangled name.
            const swift::AnyFunctionType *identifier_uncurried_result = identifier_func_type->getResult()->getAs<swift::AnyFunctionType>();
            if (identifier_uncurried_result)
            {
              swift::CanType identifier_result_type(identifier_uncurried_result->getResult()->getDesugaredType()->getCanonicalType());
              swift::CanType identifier_input_type(identifier_uncurried_result->getInput()->getDesugaredType()->getCanonicalType());
              if (identifier_result_type == type_result_type &&
                  identifier_input_type == type_input_type)
              {
                break;
              }
            }
          }
        }
      }
      identifier_func = nullptr;
    }

    if (identifier_func)
    {
      result._decls.push_back(identifier_func);
      result._types.push_back(FixCallingConv(identifier_func, identifier_func->getType().getPointer()));
    }
    else
    {
      result._error = "could not find a matching subscript signature";
    }
  }
  else
  {
    // Otherwise this is a getter/setter/etc for a variable.  Currently you can't write a getter/setter that
    // takes a different type from the type of the variable.  So there is only one possible function.
    swift::AbstractStorageDecl *var_decl = nullptr;

    FindFirstNamedDeclWithKind(ast, identifier, swift::DeclKind::Var, decl_ctx_result);

    if (decl_ctx_result._decls.size() == 1)
    {
      var_decl = llvm::dyn_cast_or_null<swift::VarDecl>(decl_ctx_result._decls[0]);
    }
    else if (decl_ctx_result._decls.size() > 0)
    {
      // TODO: can we use the type to pick the right one? can we really have multiple variables with the same name?
      result._error = stringWithFormat("multiple variables with the same name %s",identifier.c_str());
      return;
    }
    else
    {
      result._error =stringWithFormat("no variables with the name %s",identifier.c_str());
      return;
    }

    if (var_decl)
    {
      swift::FuncDecl *decl = nullptr;

      if (node_kind == swift::Demangle::Node::Kind::DidSet && var_decl->getDidSetFunc())
      {
        decl = var_decl->getDidSetFunc();
      }
      else if (node_kind == swift::Demangle::Node::Kind::Getter && var_decl->getGetter())
      {
        decl = var_decl->getGetter();
      }
      else if (node_kind == swift::Demangle::Node::Kind::Setter && var_decl->getSetter())
      {
        decl = var_decl->getSetter();
      }
      else if (node_kind == swift::Demangle::Node::Kind::WillSet && var_decl->getWillSetFunc())
      {
        decl = var_decl->getWillSetFunc();
      }

      if (decl)
      {
        result._decls.push_back(decl);
        result._types.push_back(FixCallingConv(decl, decl->getType().getPointer()));
      }
      else
      {
        result._error = stringWithFormat("could not retrieve %s for variable %s",
                                         SwiftDemangleNodeKindToCString(node_kind),
                                         identifier.c_str());
        return;
      }
    }
    else
    {
      result._error = stringWithFormat("no decl object for %s",
                                       identifier.c_str());
      return;
    }
  }
}

static void
VisitNodeIdentifier (SwiftASTContext *ast,
                     std::vector<swift::Demangle::NodePointer> &nodes,
                     swift::Demangle::NodePointer& cur_node,
                     VisitNodeResult &result,
                     const VisitNodeResult &generic_context, // set by GenericType case
                     Log *log)
{
  swift::Demangle::NodePointer parent_node = nodes[nodes.size() - 2];
  swift::DeclKind decl_kind = GetKindAsDeclKind (parent_node->getKind());

  if (!FindFirstNamedDeclWithKind (ast, cur_node->getText(), decl_kind, result))
  {
    if (result._error.empty())
      result._error = stringWithFormat("unable to find Node::Kind::Identifier '%s'", cur_node->getText().c_str());
  }
}

static void
VisitNodeLocalDeclName (SwiftASTContext *ast,
                        std::vector<swift::Demangle::NodePointer> &nodes,
                        swift::Demangle::NodePointer& cur_node,
                        VisitNodeResult &result,
                        const VisitNodeResult &generic_context, // set by GenericType case
                        Log *log)
{
  swift::Demangle::NodePointer parent_node = nodes[nodes.size() - 2];
  std::string remangledNode = swift::Demangle::mangleNode(parent_node);
  swift::TypeDecl *decl = result._module.lookupLocalType(remangledNode);
  if (!decl)
    result._error = stringWithFormat("unable to lookup local type %s",
                                     remangledNode.c_str());
  else
  {
    // if this were to come from a closure, there may be no decl - just a module
    if (!result._decls.empty())
      result._decls.pop_back();
    if (!result._types.empty())
      result._types.pop_back();

    result._decls.push_back(decl);
    auto type = decl->getType();
    if (swift::MetatypeType *metatype = llvm::dyn_cast_or_null<swift::MetatypeType>(type.getPointer()))
      type = metatype->getInstanceType();
    result._types.push_back(type);
  }
}

static void
VisitNodePrivateDeclName (SwiftASTContext *ast,
                          std::vector<swift::Demangle::NodePointer> &nodes,
                          swift::Demangle::NodePointer& cur_node,
                          VisitNodeResult &result,
                          const VisitNodeResult &generic_context, // set by GenericType case
                          Log *log)
{
  swift::Demangle::NodePointer parent_node = nodes[nodes.size() - 2];
  swift::DeclKind decl_kind = GetKindAsDeclKind (parent_node->getKind());

  if (cur_node->getNumChildren() != 2)
  {
    if (result._error.empty())
      result._error = stringWithFormat("unable to retrieve content for Node::Kind::PrivateDeclName");
    return;
  }

  swift::Demangle::NodePointer priv_decl_id_node (cur_node->getChild(0));
  swift::Demangle::NodePointer id_node (cur_node->getChild(1));

  if (!priv_decl_id_node->hasText() || !id_node->hasText())
  {
    if (result._error.empty())
      result._error = stringWithFormat("unable to retrieve content for Node::Kind::PrivateDeclName");
    return;
  }

  if (!FindFirstNamedDeclWithKind (ast, id_node->getText(), decl_kind, result, priv_decl_id_node->getText()))
  {
    if (result._error.empty())
      result._error = stringWithFormat("unable to find Node::Kind::PrivateDeclName '%s' in '%s'", id_node->getText().c_str(), priv_decl_id_node->getText().c_str());
  }
}

static void
VisitNodeInOut (SwiftASTContext *ast,
                std::vector<swift::Demangle::NodePointer> &nodes,
                swift::Demangle::NodePointer& cur_node,
                VisitNodeResult &result,
                const VisitNodeResult &generic_context, // set by GenericType case
                Log *log)
{
  nodes.push_back(cur_node->getFirstChild());
  VisitNodeResult type_result;
  VisitNode (ast, nodes, type_result, generic_context, log);
  if (type_result._types.size() == 1 && type_result._types[0])
  {
    result._types.push_back(swift::Type(swift::LValueType::get(type_result._types[0])));
  }
  else
  {
    result._error = "couldn't resolve referent type";
  }
}

static void
VisitNodeMetatype (SwiftASTContext *ast,
                   std::vector<swift::Demangle::NodePointer> &nodes,
                   swift::Demangle::NodePointer& cur_node,
                   VisitNodeResult &result,
                   const VisitNodeResult &generic_context, // set by GenericType case
                   Log *log)
{
  auto iter = cur_node->begin();
  auto end = cur_node->end();

  llvm::Optional<swift::MetatypeRepresentation> metatype_repr;
  VisitNodeResult type_result;

  for (; iter != end; ++iter)
  {
    switch ((*iter)->getKind())
    {
      case swift::Demangle::Node::Kind::Type:
        nodes.push_back(*iter);
        VisitNode(ast, nodes, type_result, generic_context, log);
        break;
      case swift::Demangle::Node::Kind::MetatypeRepresentation:
        if ( (*iter)->getText() == "@thick" )
          metatype_repr = swift::MetatypeRepresentation::Thick;
        else if ( (*iter)->getText() == "@thin" )
          metatype_repr = swift::MetatypeRepresentation::Thin;
        else if ( (*iter)->getText() == "@objc" )
          metatype_repr = swift::MetatypeRepresentation::ObjC;
        else
          ; // leave it alone if we don't understand the representation
        break;
      default:
        break;
    }

  }

  if (type_result.HasSingleType())
  {
    result._types.push_back(swift::MetatypeType::get(type_result._types[0], metatype_repr));
  }
  else
  {
    result._error = stringWithFormat("instance type for metatype cannot be uniquely resolved");
    return;
  }

}

static void
VisitNodeModule (SwiftASTContext *ast,
                 std::vector<swift::Demangle::NodePointer> &nodes,
                 swift::Demangle::NodePointer& cur_node,
                 VisitNodeResult &result,
                 const VisitNodeResult &generic_context, // set by GenericType case
                 Log *log)
{
  std::string error;
  const char *module_name = cur_node->getText().c_str();
  if (!module_name || *module_name == '\0')
  {
    result._error = stringWithFormat("error: empty module name.");
    return;
  }

  result._module = DeclsLookupSource::GetDeclsLookupSource(*ast, ConstString(module_name));
  if (!result._module)
  {
    result._error = stringWithFormat("unable to load module '%s' (%s)",
                                     module_name, error.data());
  }
}

static void
VisitNodeNonVariadicTuple (SwiftASTContext *ast,
                           std::vector<swift::Demangle::NodePointer> &nodes,
                           swift::Demangle::NodePointer& cur_node,
                           VisitNodeResult &result,
                           const VisitNodeResult &generic_context, // set by GenericType case
                           Log *log)
{
  if (cur_node->begin() == cur_node->end())
  {
    // No children of this tuple, make an empty tuple

    if (ast)
    {
      result._types.push_back(swift::TupleType::getEmpty(*ast));
    }
    else
    {
      result._error = "invalid ASTContext";
    }
  }
  else
  {
    std::vector<swift::TupleTypeElt> tuple_fields;
    swift::Demangle::Node::iterator end = cur_node->end();
    for (swift::Demangle::Node::iterator pos = cur_node->begin(); pos != end; ++pos)
    {
      nodes.push_back(*pos);
      VisitNodeResult tuple_element_result;
      VisitNode (ast, nodes, tuple_element_result, generic_context, log);
      if (tuple_element_result._error.empty() && tuple_element_result._tuple_type_element.getType())
      {
        tuple_fields.push_back(tuple_element_result._tuple_type_element);
      }
      else
      {
        result._error = tuple_element_result._error;
      }
    }
    if (result._error.empty())
    {
      if (ast)
      {
        result._types.push_back(swift::TupleType::get(tuple_fields, *ast));
      }
      else
      {
        result._error = "invalid ASTContext";
      }
    }
  }
}

static void
VisitNodeProtocolList (SwiftASTContext *ast,
                       std::vector<swift::Demangle::NodePointer> &nodes,
                       swift::Demangle::NodePointer& cur_node,
                       VisitNodeResult &result,
                       const VisitNodeResult &generic_context, // set by GenericType case
                       Log *log)
{
  if (cur_node->begin() != cur_node->end())
  {
    VisitNodeResult protocol_types_result;
    nodes.push_back(cur_node->getFirstChild());
    VisitNode (ast, nodes, protocol_types_result, generic_context, log);
    if (protocol_types_result._error.empty() /* cannot check for empty type list as protocol<> is allowed */)
    {
      if (ast)
      {
        result._types.push_back(swift::ProtocolCompositionType::get(*ast, protocol_types_result._types));
      }
      else
      {
        result._error = "invalid ASTContext";
      }
    }
  }
}

static void
VisitNodeQualifiedArchetype (SwiftASTContext *ast,
                             std::vector<swift::Demangle::NodePointer> &nodes,
                             swift::Demangle::NodePointer& cur_node,
                             VisitNodeResult &result,
                             const VisitNodeResult &generic_context, // set by GenericType case
                             Log *log)
{
}

static void
VisitNodeSelfTypeRef (SwiftASTContext *ast,
                      std::vector<swift::Demangle::NodePointer> &nodes,
                      swift::Demangle::NodePointer& cur_node,
                      VisitNodeResult &result,
                      const VisitNodeResult &generic_context, // set by GenericType case
                      Log *log)
{
  nodes.push_back(cur_node->getFirstChild());
  VisitNodeResult type_result;
  VisitNode (ast, nodes, type_result, generic_context, log);
  if (type_result.HasSingleType())
  {
    swift::Type supposed_protocol_type(type_result.GetFirstType());
    swift::ProtocolType *protocol_type = supposed_protocol_type->getAs<swift::ProtocolType>();
    swift::ProtocolDecl *protocol_decl = protocol_type ? protocol_type->getDecl() : nullptr;
    if (protocol_decl)
    {
      swift::ArchetypeType::AssocTypeOrProtocolType assoc_protocol_type(protocol_decl);
      if (ast)
      {
        swift::CanTypeWrapper<swift::ArchetypeType> self_type = swift::ArchetypeType::getNew(*ast,
                                                                                             nullptr,
                                                                                             assoc_protocol_type,
                                                                                             ast->getIdentifier("Self"),
                                                                                             {supposed_protocol_type},
                                                                                             swift::Type(),
                                                                                             false);
        if (self_type.getPointer())
          result._types.push_back(swift::Type(self_type));
        else
          result._error = "referent type cannot be made into an archetype";
      }
      else
      {
        result._error = "invalid ASTContext";
      }
    }
    else
    {
      result._error = "referent type does not resolve to a protocol";
    }
  }
  else
  {
    result._error = "couldn't resolve referent type";
  }
}

static void
VisitNodeTupleElement (SwiftASTContext *ast,
                       std::vector<swift::Demangle::NodePointer> &nodes,
                       swift::Demangle::NodePointer& cur_node,
                       VisitNodeResult &result,
                       const VisitNodeResult &generic_context, // set by GenericType case
                       Log *log)
{
  const char *tuple_name = NULL;
  VisitNodeResult tuple_type_result;
  swift::Demangle::Node::iterator end = cur_node->end();
  for (swift::Demangle::Node::iterator pos = cur_node->begin(); pos != end; ++pos)
  {
    const swift::Demangle::Node::Kind child_node_kind = (*pos)->getKind();
    switch (child_node_kind)
    {
      case swift::Demangle::Node::Kind::TupleElementName:
        tuple_name = (*pos)->getText().c_str();
        break;
      case swift::Demangle::Node::Kind::Type:
        nodes.push_back((*pos)->getFirstChild());
        VisitNode (ast, nodes, tuple_type_result, generic_context, log);
        break;
      default:
        break;
    }
  }

  if (tuple_type_result._error.empty() && tuple_type_result._types.size() == 1)
  {
    if (tuple_name)
      result._tuple_type_element = swift::TupleTypeElt(tuple_type_result._types.front().getPointer(),
                                                       ast->getIdentifier(tuple_name));
    else
      result._tuple_type_element = swift::TupleTypeElt(tuple_type_result._types.front().getPointer());
  }
}

static void
VisitNodeTypeList (SwiftASTContext *ast,
                   std::vector<swift::Demangle::NodePointer> &nodes,
                   swift::Demangle::NodePointer& cur_node,
                   VisitNodeResult &result,
                   const VisitNodeResult &generic_context, // set by GenericType case
                   Log *log)
{
  if (cur_node->begin() != cur_node->end())
  {
    swift::Demangle::Node::iterator end = cur_node->end();
    for (swift::Demangle::Node::iterator pos = cur_node->begin(); pos != end; ++pos)
    {
      nodes.push_back(*pos);
      VisitNodeResult type_result;
      VisitNode (ast, nodes, type_result, generic_context, log);
      if (type_result._error.empty() && type_result._types.size() == 1)
      {
        if (type_result._decls.empty())
          result._decls.push_back(NULL);
        else
          result._decls.push_back(type_result._decls.front());
        result._types.push_back(type_result._types.front());
      }
      else
      {
        result._error = type_result._error;
      }
    }
  }
}

static void
VisitNodeUnowned (SwiftASTContext *ast,
                  std::vector<swift::Demangle::NodePointer> &nodes,
                  swift::Demangle::NodePointer& cur_node,
                  VisitNodeResult &result,
                  const VisitNodeResult &generic_context, // set by GenericType case
                  Log *log)
{
  nodes.push_back(cur_node->getFirstChild());
  VisitNodeResult type_result;
  VisitNode (ast, nodes, type_result, generic_context, log);
  if (type_result._types.size() == 1 && type_result._types[0])
  {
    if (ast)
    {
      result._types.push_back(swift::Type(swift::UnownedStorageType::get(type_result._types[0],
                                                                         *ast)));
    }
    else
    {
      result._error = "invalid ASTContext";
    }
  }
  else
  {
    result._error = "couldn't resolve referent type";
  }
}

static void
VisitNodeWeak (SwiftASTContext *ast,
               std::vector<swift::Demangle::NodePointer> &nodes,
               swift::Demangle::NodePointer& cur_node,
               VisitNodeResult &result,
               const VisitNodeResult &generic_context, // set by GenericType case
               Log *log)
{
  nodes.push_back(cur_node->getFirstChild());
  VisitNodeResult type_result;
  VisitNode (ast, nodes, type_result, generic_context, log);
  if (type_result._types.size() == 1 && type_result._types[0])
  {
    if (ast)
    {
      result._types.push_back(swift::Type(swift::WeakStorageType::get(type_result._types[0],
                                                                      *ast)));
    }
    else
    {
      result._error = "invalid ASTContext";
    }
  }
  else
  {
    result._error = "couldn't resolve referent type";
  }
}

static void
VisitFirstChildNode (SwiftASTContext *ast,
                     std::vector<swift::Demangle::NodePointer> &nodes,
                     swift::Demangle::NodePointer& cur_node,
                     VisitNodeResult &result,
                     const VisitNodeResult &generic_context, // set by GenericType case
                     Log *log)
{
  if (cur_node->begin() != cur_node->end())
  {
    nodes.push_back(cur_node->getFirstChild());
    VisitNode (ast, nodes, result, generic_context, log);
  }
}

static void
VisitAllChildNodes (SwiftASTContext *ast,
                    std::vector<swift::Demangle::NodePointer> &nodes,
                    swift::Demangle::NodePointer& cur_node,
                    VisitNodeResult &result,
                    const VisitNodeResult &generic_context, // set by GenericType case
                    Log *log)
{
  swift::Demangle::Node::iterator child_end = cur_node->end();
  for (swift::Demangle::Node::iterator child_pos = cur_node->begin(); child_pos != child_end; ++child_pos)
  {
    nodes.push_back(*child_pos);
    VisitNode (ast, nodes, result, generic_context, log);
  }
}

static void
VisitNode (SwiftASTContext *ast,
           std::vector<swift::Demangle::NodePointer> &nodes,
           VisitNodeResult &result,
           const VisitNodeResult &generic_context, // set by GenericType case
           Log *log)
{
  if (nodes.empty())
    result._error = "no node";
  else if (nodes.back() == nullptr)
    result._error = "last node is NULL";
  else
    result._error = "";

  if (result._error.empty())
  {
    swift::Demangle::NodePointer node = nodes.back();
    const swift::Demangle::Node::Kind node_kind = node->getKind();

    switch (node_kind)
    {
      case swift::Demangle::Node::Kind::OwningAddressor:
      case swift::Demangle::Node::Kind::OwningMutableAddressor:
      case swift::Demangle::Node::Kind::UnsafeAddressor:
      case swift::Demangle::Node::Kind::UnsafeMutableAddressor:
        VisitNodeAddressor (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::Generics:
        VisitNodeGenerics (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::ArchetypeRef:
        VisitNodeArchetypeRef (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::Archetype:
        VisitNodeArchetype (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::ArgumentTuple:
        VisitFirstChildNode (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::AssociatedTypeRef:
        VisitNodeAssociatedTypeRef (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::BoundGenericClass:
      case swift::Demangle::Node::Kind::BoundGenericStructure:
      case swift::Demangle::Node::Kind::BoundGenericEnum:
        VisitNodeBoundGeneric (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::BuiltinTypeName:
        VisitNodeBuiltinTypeName (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::Structure:
      case swift::Demangle::Node::Kind::Class:
      case swift::Demangle::Node::Kind::Enum:
      case swift::Demangle::Node::Kind::Global:
      case swift::Demangle::Node::Kind::Static:
      case swift::Demangle::Node::Kind::TypeAlias:
      case swift::Demangle::Node::Kind::Type:
      case swift::Demangle::Node::Kind::TypeMangling:
      case swift::Demangle::Node::Kind::ReturnType:
      case swift::Demangle::Node::Kind::Protocol:
        VisitAllChildNodes (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::Constructor:
        VisitNodeConstructor (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::Destructor:
        VisitNodeDestructor (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::DeclContext:
        VisitNodeDeclContext (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::ErrorType:
        result._error = "error type encountered while demangling name";
        break;

      case swift::Demangle::Node::Kind::Extension:
        VisitNodeExtension(ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::ExplicitClosure:
        VisitNodeExplicitClosure (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::Function:
      case swift::Demangle::Node::Kind::Allocator:
      case swift::Demangle::Node::Kind::Variable:    // Out of order on purpose
        VisitNodeFunction (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::FunctionType:
      case swift::Demangle::Node::Kind::UncurriedFunctionType:      // Out of order on purpose.
        VisitNodeFunctionType (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::GenericType:
        VisitNodeGenericType (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::DidSet:
      case swift::Demangle::Node::Kind::Getter:
      case swift::Demangle::Node::Kind::Setter:
      case swift::Demangle::Node::Kind::WillSet: // out of order on purpose
        VisitNodeSetterGetter (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::LocalDeclName:
        VisitNodeLocalDeclName(ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::Identifier:
        VisitNodeIdentifier (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::InOut:
        VisitNodeInOut (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::Metatype:
        VisitNodeMetatype (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::Module:
        VisitNodeModule (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::NonVariadicTuple:
        VisitNodeNonVariadicTuple (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::PrivateDeclName:
        VisitNodePrivateDeclName(ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::ProtocolList:
        VisitNodeProtocolList (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::QualifiedArchetype:
        VisitNodeQualifiedArchetype (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::SelfTypeRef:
        VisitNodeSelfTypeRef (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::TupleElement:
        VisitNodeTupleElement (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::TypeList:
        VisitNodeTypeList (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::Unowned:
        VisitNodeUnowned (ast, nodes, node, result, generic_context, log);
        break;

      case swift::Demangle::Node::Kind::Weak:
        VisitNodeWeak (ast, nodes, node, result, generic_context, log);
        break;
      default:
        break;
    }
  }
  nodes.pop_back();
}

swift::Type swift::ide::getTypeFromMangledTypename(swift::ASTContext &Ctx,
                                                   const char *mangled_typename,
                                                   std::string &error)
{
  ConstString mangled_name (mangled_typename);
  std::vector<swift::Demangle::NodePointer> nodes;
  nodes.push_back(swift::Demangle::demangleTypeAsNode(mangled_typename,
                                                      mangled_name.length()));
  VisitNodeResult empty_generic_context;
  VisitNodeResult result;

  VisitNode(&Ctx, nodes, result, empty_generic_context, nullptr);
  error = result._error;
  if (error.empty() && result._types.size() == 1)
  {
    return result._types.front().getPointer();
  }
  else
  {
    error = stringWithFormat("type for typename '%s' was not found",mangled_typename);
    return swift::Type();
  }
  return swift::Type();
}
