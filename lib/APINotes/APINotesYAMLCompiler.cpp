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
#include "swift/APINotes/Types.h"
#include "swift/APINotes/APINotesWriter.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/YAMLTraits.h"

/*
 
 YAML Format specification.

 Nullability should be expressed using one of the following values:
 O - Optional (or Nullable)
 N - Not Optional
 U - Unknown
 S - Scalar

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

       FactoryAsInit: false           # Optional: Specifies if this method is a
                                      # factory initializer (false/true)
       DesignatedInit: false          # Optional: Specifies if this method is a
                                      # designated initializer (false/true)
   Properties:
     - Name: window

       Nullability: O

       Availability: OSX

       AvailabilityMessage: ""

 The protocol definition format is the same as the class definition.

 Each functuon definition is of the following form:

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
    APIAvailability Mode;
    StringRef Msg;
    AvailabilityItem() : Mode(APIAvailability::Available), Msg("") {}
  };

  static api_notes::NullableKind UnknownNullability =
    api_notes::NullableKind::Unknown;
  static api_notes::NullableKind DefaultNullability =
    api_notes::NullableKind::NonNullable;
  typedef std::vector<swift::api_notes::NullableKind> NullabilitySeq;

  struct Method {
    StringRef Selector;
    MethodKind Kind;
    NullabilitySeq Nullability;
    swift::api_notes::NullableKind NullabilityOfRet;
    AvailabilityItem Availability;
    bool FactoryAsInit;
    bool DesignatedInit;
  };
  typedef std::vector<Method> MethodsSeq;

  struct Property {
    StringRef Name;
    swift::api_notes::NullableKind Nullability;
    AvailabilityItem Availability;
  };
  typedef std::vector<Property> PropertiesSeq;

  struct Class {
    StringRef Name;
    bool AuditedForNullability;
    AvailabilityItem Availability;
    MethodsSeq Methods;
    PropertiesSeq Properties;
  };
  typedef std::vector<Class> ClassesSeq;

  struct Function {
    StringRef Name;
    NullabilitySeq Nullability;
    swift::api_notes::NullableKind NullabilityOfRet;
    AvailabilityItem Availability;
  };
  typedef std::vector<Function> FunctionsSeq;

  struct GlobalVariable {
    StringRef Name;
    swift::api_notes::NullableKind Nullability;
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
                                          UnknownNullability);
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
                                            UnknownNullability);
        io.mapOptional("Availability",    m.Availability.Mode);
        io.mapOptional("AvailabilityMsg", m.Availability.Msg);
        io.mapOptional("FactoryAsInit",   m.FactoryAsInit, false);
        io.mapOptional("DesignatedInit",  m.DesignatedInit, false);
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
                                           UnknownNullability);
        io.mapOptional("Availability",     f.Availability.Mode);
        io.mapOptional("AvailabilityMsg",  f.Availability.Msg);
      }
    };

    template <>
    struct MappingTraits<GlobalVariable> {
      static void mapping(IO &io, GlobalVariable& v) {
        io.mapRequired("Name",            v.Name);
        io.mapOptional("Nullability",     v.Nullability,
                                          UnknownNullability);
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

static bool translateAvailability(const AvailabilityItem &in,
                                  api_notes::CommonEntityInfo &outInfo,
                                  llvm::StringRef apiName) {
  using namespace api_notes;
  // TODO: handle more availability kinds here.
  outInfo.Unavailable = (in.Mode == APIAvailability::None);
  if (in.Mode != APIAvailability::Available) {
    outInfo.UnavailableMsg = in.Msg;
  } else {
    if (!in.Msg.empty()) {
      llvm::errs() << "Availability message for available class '" << apiName
                   << "' will not be used.";
      return true;
    }
  }
  return false;
}

// Translate from Method into ObjCMethodInfo and write it out.
static bool writeMethod(api_notes::APINotesWriter &writer,
                        const Method &meth,
                        api_notes::ContextID classID, StringRef className) {
  using namespace api_notes;
  ObjCMethodInfo mInfo;

  // Check if the selector ends with ':' to determine if it takes arguments.
  bool takesArguments = meth.Selector.endswith(":");

  // Split the selector into pieces.
  llvm::SmallVector<StringRef, 4> a;
  meth.Selector.split(a, ":", /*MaxSplit*/ -1, /*KeepEmpty*/ false);
  if (!takesArguments && a.size() > 1 ) {
    llvm::errs() << "Selector " << meth.Selector
                 << "is missing a ':' at the end\n";
    return true;
  }

  // Construct ObjCSelectorRef.
  api_notes::ObjCSelectorRef selectorRef;
  selectorRef.NumPieces = !takesArguments ? 0 : a.size();
  selectorRef.Identifiers = llvm::ArrayRef<StringRef>(a);

  // Translate the initializer info.
  mInfo.DesignatedInit = meth.DesignatedInit;
  // TODO: We should be able to express more in the YAML and/or need to
  // rename the yaml entry.
  if (meth.FactoryAsInit)
    mInfo.setFactoryAsInitKind(FactoryAsInitKind::AsClassMethod);

  // Translate availability info.
  if (translateAvailability(meth.Availability, mInfo, meth.Selector))
    return true;

  // Translate nullability info.
  if (meth.NullabilityOfRet != UnknownNullability) {
    mInfo.addTypeInfo(0, meth.NullabilityOfRet);
  }
  if (meth.Nullability.size() > ObjCMethodInfo::getMaxNullabilityIndex()) {
    llvm::errs() << "Nullability info for "
                 << className << meth.Selector << " does not fit.";
    return true;
  }
  unsigned int idx = 1;
  for (auto i = meth.Nullability.begin(),
       e = meth.Nullability.end(); i != e; ++i, ++idx) {
    mInfo.addTypeInfo(idx, *i);
  }

  // Write it.
  writer.addObjCMethod(classID, selectorRef,
                       meth.Kind == MethodKind::Instance,
                       mInfo);
  return false;
}

static bool compile(const Module &module, llvm::raw_ostream &os){
  using namespace api_notes;
  APINotesWriter writer;

  // Write all classes.
  for (auto iCl = module.Classes.begin(), eCl = module.Classes.end();
       iCl != eCl; ++iCl) {
    Class cl = *iCl;

    // Write the class.
    ObjCContextInfo cInfo;
    if (cl.AuditedForNullability)
      cInfo.setDefaultNullability(DefaultNullability);
    if (translateAvailability(cl.Availability, cInfo, cl.Name))
      return true;
    ContextID clID = writer.addObjCClass(cl.Name, cInfo);

    // Write all methods.
    for (auto iMeth = cl.Methods.begin(), eMeth = cl.Methods.end();
         iMeth != eMeth; ++iMeth) {
      writeMethod(writer, *iMeth, clID, cl.Name);
    }

    // Write all properties.
    for (auto iProp = cl.Properties.begin(), eProp = cl.Properties.end();
         iProp != eProp; ++iProp) {
      // Translate from Property into ObjCPropertyInfo.
      Property prop = *iProp;
      ObjCPropertyInfo pInfo;
      pInfo.setNullabilityAudited(prop.Nullability);
      if (translateAvailability(cl.Availability, cInfo, cl.Name))
        return true;
      writer.addObjCProperty(clID, prop.Name, pInfo);
    }
  }

  writer.writeToStream(os);

  return false;
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
                                llvm::raw_ostream &os) {
  Module module;

  if (parseAPINotes(yamlInput, module))
    return true;

  return compile(module, os);
}
