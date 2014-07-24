//===--- APINotesYAMLCompiler.cpp - API Notes YAML format reader *- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file reads API notes specified in YAML format.
//
//===----------------------------------------------------------------------===//
#include "swift/APINotes/APINotesYAMLCompiler.h"
#include "swift/APINotes/APINotesReader.h"
#include "swift/APINotes/Types.h"
#include "swift/APINotes/APINotesWriter.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/YAMLTraits.h"
#include <algorithm>

/*
 
 YAML Format specification.

 Nullability should be expressed using one of the following values:
   O - Optional (or Nullable)
   N - Not Optional
   S - Scalar
   U - Unknown
 Note, the API is considered 'audited' when at least the return value or a
 parameter has a nullability value. For 'audited' APIs, we assume the default
 nullability for any underspecified type.

 FactoryAsInit can have the following values:
   C - Treat as class method.
   I - Treat as initializer.
   A - Automatically infer based on the name and type (default).

---
 Name: AppKit             # The name of the framework

 Availability: OSX        # Optional: Specifies which platform the API is
                          # available on. [OSX / iOS / none/ available]

 AvailabilityMessage: ""  # Optional: Custom availability message to display to
                          # the user, when API is not available.

 Classes:                 # List of classes
 ...
 Protocols:               # List of protocols
 ...
 Functions:               # List of functions
 ...
 Globals:                 # List of globals
 ...

 Each class and protocol is defined as following:

 - Name: NSView                       # The name of the class
 
   AuditedForNullability: false       # Optional: Specifies if the whole class
                                      # has been audited for nullability.
                                      # If yes, we assume all the methods and
                                      # properties of the class have default
                                      # nullability unless it is overwritten by
                                      # a method/property specific info below.
                                      # This applies to all classes, extensions,
                                      # and categories of the class defined in
                                      # the current framework/module.
                                      # (false/true)

   Availability: OSX

   AvailabilityMessage: ""

   Methods:
     - Selector: "setSubviews:"       # Full name

       MethodKind: Instance           # [Class/Instance]

       Nullability: [N, N, O, S]      # The nullability of parameters in
                                      # the signature.

       NullabilityOfRet: O            # The nullability of the return value.

       Availability: OSX

       AvailabilityMessage: ""

       FactoryAsInit: C               # Optional: Specifies if this method is a
                                      # factory initializer (false/true)
       DesignatedInit: false          # Optional: Specifies if this method is a
                                      # designated initializer (false/true)

       Required: false                # Optional: Specifies if this method is a
                                      # required initializer (false/true)

   Properties:
     - Name: window

       Nullability: O

       Availability: OSX

       AvailabilityMessage: ""

 The protocol definition format is the same as the class definition.

 Each function definition is of the following form:

 - Name: "myGlobalFunction"           # Full name

   Nullability: [N, N, O, S]          # The nullability of parameters in
                                      # the signature.

   NullabilityOfRet: O                # The nullability of the return value.

   Availability: OSX

   AvailabilityMessage: ""

Each global variable definition is of the following form:

 - Name: MyGlobalVar

   Nullability: O

   Availability: OSX

   AvailabilityMessage: ""

*/

using llvm::StringRef;
using namespace swift;
namespace {
  enum class APIAvailability {
    Available = 0,
    OSX,
    IOS,
    None,
  };

  enum class MethodKind {
    Class,
    Instance,
  };

  struct AvailabilityItem {
    APIAvailability Mode = APIAvailability::Available;
    StringRef Msg;
    AvailabilityItem() : Mode(APIAvailability::Available), Msg("") {}
  };

  static api_notes::NullableKind AbsentNullability =
    api_notes::NullableKind::Absent;
  static api_notes::NullableKind DefaultNullability =
    api_notes::NullableKind::NonNullable;
  typedef std::vector<swift::api_notes::NullableKind> NullabilitySeq;

  struct Method {
    StringRef Selector;
    MethodKind Kind;
    NullabilitySeq Nullability;
    api_notes::NullableKind NullabilityOfRet = api_notes::NullableKind::Unknown;
    AvailabilityItem Availability;
    api_notes::FactoryAsInitKind FactoryAsInit
      = api_notes::FactoryAsInitKind::Infer;
    bool DesignatedInit = false;
    bool Required = false;
  };
  typedef std::vector<Method> MethodsSeq;

  struct Property {
    StringRef Name;
    api_notes::NullableKind Nullability = api_notes::NullableKind::Unknown;
    AvailabilityItem Availability;
  };
  typedef std::vector<Property> PropertiesSeq;

  struct Class {
    StringRef Name;
    bool AuditedForNullability = false;
    AvailabilityItem Availability;
    MethodsSeq Methods;
    PropertiesSeq Properties;
  };
  typedef std::vector<Class> ClassesSeq;

  struct Function {
    StringRef Name;
    NullabilitySeq Nullability;
    api_notes::NullableKind NullabilityOfRet = api_notes::NullableKind::Unknown;
    AvailabilityItem Availability;
  };
  typedef std::vector<Function> FunctionsSeq;

  struct GlobalVariable {
    StringRef Name;
    api_notes::NullableKind Nullability = api_notes::NullableKind::Unknown;
    AvailabilityItem Availability;
  };
  typedef std::vector<GlobalVariable> GlobalVariablesSeq;

  struct Module {
    StringRef Name;
    AvailabilityItem Availability;
    ClassesSeq Classes;
    ClassesSeq Protocols;
    FunctionsSeq Functions;
    GlobalVariablesSeq Globals;
  };

};

LLVM_YAML_IS_FLOW_SEQUENCE_VECTOR(swift::api_notes::NullableKind);
LLVM_YAML_IS_SEQUENCE_VECTOR(Method);
LLVM_YAML_IS_SEQUENCE_VECTOR(Property);
LLVM_YAML_IS_SEQUENCE_VECTOR(Class);
LLVM_YAML_IS_SEQUENCE_VECTOR(Function);
LLVM_YAML_IS_SEQUENCE_VECTOR(GlobalVariable);

namespace llvm {
  namespace yaml {

    template <>
    struct ScalarEnumerationTraits<api_notes::NullableKind > {
      static void enumeration(IO &io, api_notes::NullableKind  &value) {
        io.enumCase(value, "N", api_notes::NullableKind::NonNullable);
        io.enumCase(value, "O", api_notes::NullableKind::Nullable);
        io.enumCase(value, "U", api_notes::NullableKind::Unknown);
        // TODO: Mapping this to it's own value would allow for better cross
        // checking. Also the default should be Unknown.
        io.enumCase(value, "S", api_notes::NullableKind::Unknown);
      }
    };

    template <>
    struct ScalarEnumerationTraits<api_notes::FactoryAsInitKind > {
      static void enumeration(IO &io, api_notes::FactoryAsInitKind  &value) {
        io.enumCase(value, "A", api_notes::FactoryAsInitKind::Infer);
        io.enumCase(value, "C", api_notes::FactoryAsInitKind::AsClassMethod);
        io.enumCase(value, "I", api_notes::FactoryAsInitKind::AsInitializer);
      }
    };

    template <>
    struct ScalarEnumerationTraits<MethodKind> {
      static void enumeration(IO &io, MethodKind &value) {
        io.enumCase(value, "Class",    MethodKind::Class);
        io.enumCase(value, "Instance", MethodKind::Instance);
      }
    };

    template <>
    struct ScalarEnumerationTraits<APIAvailability> {
      static void enumeration(IO &io, APIAvailability &value) {
        io.enumCase(value, "OSX",       APIAvailability::OSX);
        io.enumCase(value, "iOS",       APIAvailability::IOS);
        io.enumCase(value, "none",      APIAvailability::None);
        io.enumCase(value, "available", APIAvailability::Available);
      }
    };

    template <>
    struct MappingTraits<Property> {
      static void mapping(IO &io, Property& p) {
        io.mapRequired("Name",            p.Name);
        io.mapOptional("Nullability",     p.Nullability,
                                          AbsentNullability);
        io.mapOptional("Availability",    p.Availability.Mode);
        io.mapOptional("AvailabilityMsg", p.Availability.Msg);
      }
    };

    template <>
    struct MappingTraits<Method> {
      static void mapping(IO &io, Method& m) {
        io.mapRequired("Selector",        m.Selector);
        io.mapRequired("MethodKind",      m.Kind);
        io.mapOptional("Nullability",     m.Nullability);
        io.mapOptional("NullabilityOfRet",  m.NullabilityOfRet,
                                            AbsentNullability);
        io.mapOptional("Availability",    m.Availability.Mode);
        io.mapOptional("AvailabilityMsg", m.Availability.Msg);
        io.mapOptional("FactoryAsInit",   m.FactoryAsInit,
                                          api_notes::FactoryAsInitKind::Infer);
        io.mapOptional("DesignatedInit",  m.DesignatedInit, false);
        io.mapOptional("Required",        m.Required, false);
      }
    };

    template <>
    struct MappingTraits<Class> {
      static void mapping(IO &io, Class& c) {
        io.mapRequired("Name",                  c.Name);
        io.mapOptional("AuditedForNullability", c.AuditedForNullability, false);
        io.mapOptional("Availability",          c.Availability.Mode);
        io.mapOptional("AvailabilityMsg",       c.Availability.Msg);
        io.mapOptional("Methods",               c.Methods);
        io.mapOptional("Properties",            c.Properties);
      }
    };

    template <>
    struct MappingTraits<Function> {
      static void mapping(IO &io, Function& f) {
        io.mapRequired("Name",             f.Name);
        io.mapOptional("Nullability",      f.Nullability);
        io.mapOptional("NullabilityOfRet", f.NullabilityOfRet,
                                           AbsentNullability);
        io.mapOptional("Availability",     f.Availability.Mode);
        io.mapOptional("AvailabilityMsg",  f.Availability.Msg);
      }
    };

    template <>
    struct MappingTraits<GlobalVariable> {
      static void mapping(IO &io, GlobalVariable& v) {
        io.mapRequired("Name",            v.Name);
        io.mapOptional("Nullability",     v.Nullability,
                                          AbsentNullability);
        io.mapOptional("Availability",    v.Availability.Mode);
        io.mapOptional("AvailabilityMsg", v.Availability.Msg);
      }
    };

    template <>
    struct MappingTraits<Module> {
      static void mapping(IO &io, Module& m) {
        io.mapRequired("Name",            m.Name);
        io.mapOptional("Availability",    m.Availability.Mode);
        io.mapOptional("AvailabilityMsg", m.Availability.Msg);
        io.mapOptional("Classes",         m.Classes);
        io.mapOptional("Protocols",       m.Protocols);
        io.mapOptional("Functions",       m.Functions);
        io.mapOptional("Globals",         m.Globals);
      }
    };
  }
}

using llvm::yaml::Input;
using llvm::yaml::Output;

static bool parseAPINotes(StringRef yamlInput, Module &module) {
  Input yin(yamlInput);
  yin >> module;

  if (std::error_code ec = yin.error()) {
    llvm::errs() << "\n Could not parse the input file: "
                 << ec.message() << '\n';
    return true;
  }
  return false;
}

static bool compile(const Module &module, llvm::raw_ostream &os,
                    api_notes::OSType targetOS){
  using namespace api_notes;

  class YAMLConverter {
    const Module &TheModule;
    APINotesWriter *Writer;
    OSType TargetOS;
    llvm::raw_ostream &OS;

    bool ErrorOccured;

  public:
    YAMLConverter(const Module &module,
                 OSType targetOS,
                 llvm::raw_ostream &os) :
      TheModule(module), Writer(0), TargetOS(targetOS), OS(os),
      ErrorOccured(false) {}

    bool isAvailable(const AvailabilityItem &in) {
      // Check if the API is available on the OS for which we are building.
      if (in.Mode == APIAvailability::OSX && TargetOS != OSType::OSX)
        return false;
      if (in.Mode == APIAvailability::IOS && TargetOS != OSType::IOS)
        return false;
      return true;
    }

    bool convertAvailability(const AvailabilityItem &in,
                             CommonEntityInfo &outInfo,
                             llvm::StringRef apiName) {
      // Populate the 'Unavailable' information.
      outInfo.Unavailable = (in.Mode == APIAvailability::None);
      if (outInfo.Unavailable) {
        outInfo.UnavailableMsg = in.Msg;
      } else {
        if (!in.Msg.empty()) {
          llvm::errs() << "Availability message for available class '"
                       << apiName << "' will not be used.";
          ErrorOccured = true;
        }
      }
      return false;
    }

    void convertNullability(const NullabilitySeq &nullability,
                                   NullableKind nullabilityOfRet,
                                   FunctionInfo &outInfo,
                                   llvm::StringRef apiName) {
      if (nullability.size() > FunctionInfo::getMaxNullabilityIndex()) {
        llvm::errs() << "Nullability info for " << apiName << " does not fit.";
        ErrorOccured = true;
        return;
      }

      bool audited = false;
      unsigned int idx = 1;
      for (auto i = nullability.begin(),
                e = nullability.end(); i != e; ++i, ++idx){
        outInfo.addTypeInfo(idx, *i);
        audited = true;
      }
      if (nullabilityOfRet != AbsentNullability) {
        outInfo.addTypeInfo(0, nullabilityOfRet);
        audited = true;
      } else if (audited) {
        outInfo.addTypeInfo(0, DefaultNullability);
      }
      if (audited) {
        outInfo.NullabilityAudited = audited;
        outInfo.NumAdjustedNullable = idx;
      }
    }

    // Translate from Method into ObjCMethodInfo and write it out.
    void convertMethod(const Method &meth,
                       ContextID classID, StringRef className) {
      ObjCMethodInfo mInfo;

      if (!isAvailable(meth.Availability))
        return;

      convertAvailability(meth.Availability, mInfo, meth.Selector);

      // Check if the selector ends with ':' to determine if it takes arguments.
      bool takesArguments = meth.Selector.endswith(":");

      // Split the selector into pieces.
      llvm::SmallVector<StringRef, 4> a;
      meth.Selector.split(a, ":", /*MaxSplit*/ -1, /*KeepEmpty*/ false);
      if (!takesArguments && a.size() > 1 ) {
        llvm::errs() << "Selector " << meth.Selector
                     << "is missing a ':' at the end\n";
        ErrorOccured = true;
        return;
      }

      // Construct ObjCSelectorRef.
      api_notes::ObjCSelectorRef selectorRef;
      selectorRef.NumPieces = !takesArguments ? 0 : a.size();
      selectorRef.Identifiers = a;

      // Translate the initializer info.
      mInfo.DesignatedInit = meth.DesignatedInit;
      mInfo.Required = meth.Required;
      if (meth.FactoryAsInit != FactoryAsInitKind::Infer)
        mInfo.setFactoryAsInitKind(meth.FactoryAsInit);

      // Translate nullability info.
      convertNullability(meth.Nullability, meth.NullabilityOfRet,
                         mInfo, meth.Selector);

      // Write it.
      Writer->addObjCMethod(classID, selectorRef,
                           meth.Kind == MethodKind::Instance,
                           mInfo);
    }

    void convertContext(const Class &cl, bool isClass) {
      // Write the class.
      ObjCContextInfo cInfo;

      // First, translate and check availability info.
      if (!isAvailable(cl.Availability))
        return;

      convertAvailability(cl.Availability, cInfo, cl.Name);

      if (cl.AuditedForNullability)
        cInfo.setDefaultNullability(DefaultNullability);

      ContextID clID = isClass ? Writer->addObjCClass(cl.Name, cInfo) :
                                 Writer->addObjCProtocol(cl.Name, cInfo);

      // Write all methods.
      llvm::StringMap<std::pair<bool, bool>> knownMethods;
      for (const auto &method : cl.Methods) {
        // Check for duplicate method definitions.
        bool isInstanceMethod = method.Kind == MethodKind::Instance;
        bool &known = isInstanceMethod ? knownMethods[method.Selector].first
                                       : knownMethods[method.Selector].second;
        if (known) {
          llvm::errs() << "Duplicate definition of method '"
                       << (isInstanceMethod? '-' : '+')
                       << "[" << cl.Name << " " << method.Selector << "]'\n";
          ErrorOccured = true;
          continue;
        }
        known = true;

        convertMethod(method, clID, cl.Name);
      }

      // Write all properties.
      llvm::StringSet<> knownProperties;
      for (const auto &prop : cl.Properties) {
        // Check for duplicate property definitions.
        if (!knownProperties.insert(prop.Name)) {
          llvm::errs() << "Duplicate definition of property '" << cl.Name << "."
                       << prop.Name << "'\n";
          ErrorOccured = true;
          continue;
        }

        // Translate from Property into ObjCPropertyInfo.
        ObjCPropertyInfo pInfo;
        if (!isAvailable(prop.Availability))
          continue;
        convertAvailability(prop.Availability, pInfo, prop.Name);
        pInfo.setNullabilityAudited(prop.Nullability);
        Writer->addObjCProperty(clID, prop.Name, pInfo);
      }
    }

    bool convertModule() {
      if (!isAvailable(TheModule.Availability))
        return false;

      // Set up the writer.
      // FIXME: This is kindof ugly.
      APINotesWriter writer(TheModule.Name);
      Writer = &writer;

      // Write all classes.
      llvm::StringSet<> knownClasses;
      for (const auto &cl : TheModule.Classes) {
        // Check for duplicate class definitions.
        if (!knownClasses.insert(cl.Name)) {
          llvm::errs() << "Multiple definitions of class '" << cl.Name << "'\n";
          ErrorOccured = true;
          continue;
        }

        convertContext(cl, /*isClass*/ true);
      }

      // Write all protocols.
      llvm::StringSet<> knownProtocols;
      for (const auto &pr : TheModule.Protocols) {
        // Check for duplicate protocol definitions.
        if (!knownProtocols.insert(pr.Name)) {
          llvm::errs() << "Multiple definitions of protocol '"
                       << pr.Name << "'\n";
          ErrorOccured = true;
          continue;
        }

        convertContext(pr, /*isClass*/ false);
      }

      // Write all global variables.
      llvm::StringSet<> knownGlobals;
      for (const auto &global : TheModule.Globals) {
        // Check for duplicate global variables.
        if (!knownGlobals.insert(global.Name)) {
          llvm::errs() << "Multiple definitions of global variable '"
                       << global.Name << "'\n";
          ErrorOccured = true;
          continue;
        }

        GlobalVariableInfo info;
        if (!isAvailable(global.Availability))
          continue;
        convertAvailability(global.Availability, info, global.Name);
        info.setNullabilityAudited(global.Nullability);
        Writer->addGlobalVariable(global.Name, info);
      }

      // Write all global functions.
      llvm::StringSet<> knownFunctions;
      for (const auto &function : TheModule.Functions) {
        // Check for duplicate global functions.
        if (!knownFunctions.insert(function.Name)) {
          llvm::errs() << "Multiple definitions of global function '"
                       << function.Name << "'\n";
          ErrorOccured = true;
          continue;
        }

        GlobalFunctionInfo info;
        if (!isAvailable(function.Availability))
          continue;
        convertAvailability(function.Availability, info, function.Name);
        convertNullability(function.Nullability,
                           function.NullabilityOfRet,
                           info, function.Name);

        Writer->addGlobalFunction(function.Name, info);
      }

      if (!ErrorOccured)
        Writer->writeToStream(OS);

      return ErrorOccured;
    }
  };

  YAMLConverter c(module, targetOS, os);
  return c.convertModule();
}

bool api_notes::parseAndDumpAPINotes(StringRef yamlInput)  {
  Module module;

  if (parseAPINotes(yamlInput, module))
    return true;

  Output yout(llvm::outs());
  yout << module;

  return false;
}

bool api_notes::compileAPINotes(StringRef yamlInput,
                                llvm::raw_ostream &os,
                                OSType targetOS) {
  Module module;

  if (parseAPINotes(yamlInput, module))
    return true;

  return compile(module, os, targetOS);
}

bool api_notes::decompileAPINotes(std::unique_ptr<llvm::MemoryBuffer> input,
                                  llvm::raw_ostream &os) {
  // Try to read the file.
  auto reader = APINotesReader::get(std::move(input));
  if (!reader) {
    llvm::errs() << "not a well-formed API notes binary file\n";
    return true;
  }

  // Deserialize the API notes file into a module.
  class DecompileVisitor : public APINotesReader::Visitor {
    /// Allocator used to clone those strings that need it.
    llvm::BumpPtrAllocator Allocator;

    /// The module we're building.
    Module TheModule;

    /// A mapping from context ID to a pair (index, is-protocol) that indicates
    /// the index of that class or protocol in the global "classes" or
    /// "protocols" list.
    llvm::DenseMap<unsigned, std::pair<unsigned, bool>> knownContexts;

    /// Copy a string into allocated memory so it does disappear on us.
    StringRef copyString(StringRef string) {
      if (string.empty()) return StringRef();

      void *ptr = Allocator.Allocate(string.size(), 1);
      memcpy(ptr, string.data(), string.size());
      return StringRef(reinterpret_cast<const char *>(ptr), string.size());
    }

    /// Map Objective-C context info.
    void handleObjCContext(Class &record, StringRef name,
                           const ObjCContextInfo &info) {
      record.Name = name;

      // Handle class information.
      handleAvailability(record.Availability, info);
      if (info.getDefaultNullability()) {
        record.AuditedForNullability = true;
      }
    }

    /// Map availability information, if present.
    void handleAvailability(AvailabilityItem &availability,
                            const CommonEntityInfo &info) {
      if (info.Unavailable) {
        availability.Mode = APIAvailability::None;
        availability.Msg = copyString(info.UnavailableMsg);
      }
    }

    /// Map nullability information for a function.
    void handleNullability(NullabilitySeq &nullability,
                           NullableKind &nullabilityOfRet,
                           const FunctionInfo &info,
                           unsigned numParams) {
      if (info.NullabilityAudited) {
        nullabilityOfRet = info.getReturnTypeInfo();

        // Figure out the number of parameters from the selector.
        for (unsigned i = 0; i != numParams; ++i)
          nullability.push_back(info.getParamTypeInfo(i));
      }
    }

  public:
    virtual void visitObjCClass(ContextID contextID, StringRef name,
                                const ObjCContextInfo &info) {
      // Record this known context.
      knownContexts[contextID.Value] = { TheModule.Classes.size(), false };

      // Add the class.
      TheModule.Classes.push_back(Class());
      handleObjCContext(TheModule.Classes.back(), name, info);
    }

    virtual void visitObjCProtocol(ContextID contextID, StringRef name,
                                   const ObjCContextInfo &info) {
      // Record this known context.
      knownContexts[contextID.Value] = { TheModule.Protocols.size(), true };

      // Add the protocol.
      TheModule.Protocols.push_back(Class());
      handleObjCContext(TheModule.Protocols.back(), name, info);
    }

    virtual void visitObjCMethod(ContextID contextID, StringRef selector,
                                 bool isInstanceMethod,
                                 const ObjCMethodInfo &info) {
      Method method;
      method.Selector = copyString(selector);
      method.Kind = isInstanceMethod ? MethodKind::Instance : MethodKind::Class;

      handleNullability(method.Nullability, method.NullabilityOfRet, info,
                        selector.count(':'));
      handleAvailability(method.Availability, info);
      method.FactoryAsInit = info.getFactoryAsInitKind();
      method.DesignatedInit = info.DesignatedInit;
      method.Required = info.Required;

      auto known = knownContexts[contextID.Value];
      if (known.second)
        TheModule.Protocols[known.first].Methods.push_back(method);
      else
        TheModule.Classes[known.first].Methods.push_back(method);
    }

    virtual void visitObjCProperty(ContextID contextID, StringRef name,
                                   const ObjCPropertyInfo &info) {
      Property property;
      property.Name = name;
      handleAvailability(property.Availability, info);

      // FIXME: No way to represent "not audited for nullability".
      if (auto nullability = info.getNullability()) {
        property.Nullability = *nullability;
      }

      auto known = knownContexts[contextID.Value];
      if (known.second)
        TheModule.Protocols[known.first].Properties.push_back(property);
      else
        TheModule.Classes[known.first].Properties.push_back(property);
    }

    virtual void visitGlobalFunction(StringRef name,
                                     const GlobalFunctionInfo &info) {
      Function function;
      function.Name = name;
      handleAvailability(function.Availability, info);
      if (info.NumAdjustedNullable > 0)
        handleNullability(function.Nullability, function.NullabilityOfRet,
                          info, info.NumAdjustedNullable-1);

      TheModule.Functions.push_back(function);
    }

    virtual void visitGlobalVariable(StringRef name,
                                     const GlobalVariableInfo &info) {
      GlobalVariable global;
      global.Name = name;
      handleAvailability(global.Availability, info);

      // FIXME: No way to represent "not audited for nullability".
      if (auto nullability = info.getNullability()) {
        global.Nullability = *nullability;
      }

      TheModule.Globals.push_back(global);
    }

    /// Retrieve the module.
    Module &getModule() { return TheModule; }
  } decompileVisitor;

  reader->visit(decompileVisitor);

  // Sort the data in the module, because the API notes reader doesn't preserve
  // order.
  auto &module = decompileVisitor.getModule();

  // Set module name.
  module.Name = reader->getModuleName();

  // Sort classes.
  std::sort(module.Classes.begin(), module.Classes.end(),
            [](const Class &lhs, const Class &rhs) -> bool {
              return lhs.Name < rhs.Name;
            });

  // Sort protocols.
  std::sort(module.Protocols.begin(), module.Protocols.end(),
            [](const Class &lhs, const Class &rhs) -> bool {
              return lhs.Name < rhs.Name;
            });

  // Sort methods and properties within each class and protocol.
  auto sortMembers = [](Class &record) {
    // Sort properties.
    std::sort(record.Properties.begin(), record.Properties.end(),
              [](const Property &lhs, const Property &rhs) -> bool {
                return lhs.Name < rhs.Name;
              });

    // Sort methods.
    std::sort(record.Methods.begin(), record.Methods.end(),
              [](const Method &lhs, const Method &rhs) -> bool {
                return lhs.Selector < rhs.Selector ||
                       (lhs.Selector == rhs.Selector &&
                        static_cast<unsigned>(lhs.Kind)
                          < static_cast<unsigned>(rhs.Kind));
              });
  };
  std::for_each(module.Classes.begin(), module.Classes.end(), sortMembers);
  std::for_each(module.Protocols.begin(), module.Protocols.end(), sortMembers);

  // Sort functions.
  std::sort(module.Functions.begin(), module.Functions.end(),
            [](const Function &lhs, const Function &rhs) -> bool {
              return lhs.Name < rhs.Name;
            });

  // Sort global variables.
  std::sort(module.Globals.begin(), module.Globals.end(),
            [](const GlobalVariable &lhs, const GlobalVariable &rhs) -> bool {
              return lhs.Name < rhs.Name;
            });

  // Output the YAML representation.
  Output yout(os);
  yout << module;

  return false;
}

