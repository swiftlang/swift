//===--- ObjectFile.h - Object File Related Information ------*- C++ -*-===//
//
// Object File related data structures.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ABI_OBJECTFILE_H
#define SWIFT_ABI_OBJECTFILE_H

#include "llvm/Support/ErrorHandling.h"
#include "llvm/ADT/StringRef.h"

namespace swift {

/// Represents the eight reflection sections used by Swift
enum ReflectionSectionKind : uint8_t {
  fieldmd,
  assocty,
  builtin,
  capture,
  typeref,
  reflstr,
  conform,
  protocs
};

/// Abstract base class responsible for providing the correct reflection section
/// string identifier for a given object file type (Mach-O, ELF, COFF).
class SwiftObjectFileFormat {
public:
  virtual ~SwiftObjectFileFormat() {}
  virtual llvm::StringRef getSectionName(ReflectionSectionKind section) = 0;
  virtual llvm::Optional<llvm::StringRef> getSegmentName() {
    return {};
  }
  /// Predicate to identify if the named section can contain reflection data.
  virtual bool sectionContainsReflectionData(llvm::StringRef sectionName) = 0;
};

/// Responsible for providing the Mach-O reflection section identifiers.
class SwiftObjectFileFormatMachO : public SwiftObjectFileFormat {
public:
  llvm::StringRef getSectionName(ReflectionSectionKind section) override {
    switch (section) {
    case fieldmd:
      return "__swift5_fieldmd";
    case assocty:
      return "__swift5_assocty";
    case builtin:
      return "__swift5_builtin";
    case capture:
      return "__swift5_capture";
    case typeref:
      return "__swift5_typeref";
    case reflstr:
      return "__swift5_reflstr";
    case conform:
      return "__swift5_proto";
    case protocs:
      return "__swift5_protos";
    }
    llvm_unreachable("Section type not found.");
  }
  llvm::Optional<llvm::StringRef> getSegmentName() override {
    return {"__TEXT"};
  }

  bool sectionContainsReflectionData(llvm::StringRef sectionName) override {
    return sectionName.startswith("__swift5_") || sectionName == "__const";
  }
};

/// Responsible for providing the ELF reflection section identifiers.
class SwiftObjectFileFormatELF : public SwiftObjectFileFormat {
public:
  llvm::StringRef getSectionName(ReflectionSectionKind section) override {
    switch (section) {
    case fieldmd:
      return "swift5_fieldmd";
    case assocty:
      return "swift5_assocty";
    case builtin:
      return "swift5_builtin";
    case capture:
      return "swift5_capture";
    case typeref:
      return "swift5_typeref";
    case reflstr:
      return "swift5_reflstr";
    case conform:
      return "swift5_protocol_conformances";
    case protocs:
      return "swift5_protocols";
    }
    llvm_unreachable("Section type not found.");
  }

  bool sectionContainsReflectionData(llvm::StringRef sectionName) override {
    return sectionName.startswith("swift5_");
  }
};

/// Responsible for providing the COFF reflection section identifiers
class SwiftObjectFileFormatCOFF : public SwiftObjectFileFormat {
public:
  llvm::StringRef getSectionName(ReflectionSectionKind section) override {
    switch (section) {
    case fieldmd:
      return ".sw5flmd";
    case assocty:
      return ".sw5asty";
    case builtin:
      return ".sw5bltn";
    case capture:
      return ".sw5cptr";
    case typeref:
      return ".sw5tyrf";
    case reflstr:
      return ".sw5rfst";
    case conform:
      return ".sw5prtc$B";
    case protocs:
      return ".sw5prt$B";
    }
    llvm_unreachable("Section  not found.");
  }

  bool sectionContainsReflectionData(llvm::StringRef sectionName) override {
    return sectionName.startswith(".sw5");
  }
};
} // namespace swift
#endif // SWIFT_ABI_OBJECTFILE_H
