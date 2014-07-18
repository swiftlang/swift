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

#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/YAMLTraits.h"

using llvm::StringRef;

namespace {
  enum class APIAvailability {
    OSX,
    IOS,
    None,
  };

  struct Method {
    StringRef Selector;
  };
  typedef std::vector<Method> MethodsSeq;

  struct Property {
    StringRef Name;
  };
  typedef std::vector<Property> PropertiesSeq;


  struct Class {
    StringRef Name;
    bool AuditedForNullability;
    APIAvailability Availability;
    StringRef AvailabilityMsg;
    MethodsSeq Methods;
    PropertiesSeq Properties;
  };
  typedef std::vector<Class> ClassesSeq;

  struct Framework {
    StringRef Name;
    StringRef Availability;
    StringRef AvailabilityMsg;
    ClassesSeq Classes;
  };
};

LLVM_YAML_IS_SEQUENCE_VECTOR(Method);
LLVM_YAML_IS_SEQUENCE_VECTOR(Property);
LLVM_YAML_IS_SEQUENCE_VECTOR(Class);
LLVM_YAML_IS_SEQUENCE_VECTOR(Framework);

namespace llvm {
  namespace yaml {

    template <>
    struct ScalarEnumerationTraits<APIAvailability> {
      static void enumeration(IO &io, APIAvailability &value) {
        io.enumCase(value, "OSX",  APIAvailability::OSX);
        io.enumCase(value, "iOS",  APIAvailability::IOS);
        io.enumCase(value, "none", APIAvailability::None);
      }
    };

    template <>
    struct MappingTraits<Property> {
      static void mapping(IO &io, Property& p) {
        io.mapRequired("Name",             p.Name);
      }
    };

    template <>
    struct MappingTraits<Method> {
      static void mapping(IO &io, Method& m) {
        io.mapRequired("Selector",             m.Selector);
      }
    };

    template <>
    struct MappingTraits<Class> {
      static void mapping(IO &io, Class& c) {
        io.mapRequired("ClassName",             c.Name);
        io.mapOptional("AuditedForNullability", c.AuditedForNullability);
        io.mapOptional("Availability",          c.Availability);
        io.mapOptional("AvailabilityMessage",   c.AvailabilityMsg);
        io.mapOptional("Methods",               c.Methods);
        io.mapOptional("Properties",            c.Properties);
      }
    };

    template <>
    struct MappingTraits<Framework> {
      static void mapping(IO &io, Framework& f) {
        io.mapRequired("FrameworkName",         f.Name);
        io.mapOptional("FrameworkAvailability", f.Availability);
        io.mapOptional("AvailabilityMsg",       f.AvailabilityMsg);
        io.mapRequired("FrameworkClasses",      f.Classes);
      }
    };
  }
}

using llvm::yaml::Input;

namespace swift {
namespace api_notes {

void compileAPINotes(llvm::StringRef fromFileName, llvm::raw_ostream &os) {
  using ::llvm::ErrorOr;
  using ::llvm::MemoryBuffer;
  ErrorOr<std::unique_ptr<MemoryBuffer>> FileBufOrErr =
    MemoryBuffer::getFile(fromFileName);
  if (std::error_code EC = FileBufOrErr.getError()) {
    llvm::errs() << "\n Could not open input file: " + EC.message() << '\n';
    return;
  }

  Framework f;
  Input yin(FileBufOrErr.get()->getBuffer());
  yin >> f;

  llvm::outs() << f.Name;

  if (std::error_code ec = yin.error()) {
    llvm::errs() << "\n Could not parse the input file: " << ec.message() << '\n';
    return;
  }
}

} // end namespace api_notes
} // end namespace swift
