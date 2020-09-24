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

/// Represents the six reflection sections used by Swift
enum ReflectionSectionKind : uint8_t {
  fieldmd,
  assocty,
  builtin,
  capture,
  typeref,
  reflstr
};

/// Abstract base class responsible for providing the correct reflection section
/// string identifier for a given object file type (Mach-O, ELF, COFF).
class SwiftObjectFileFormat {
public:
  virtual ~SwiftObjectFileFormat() {}
  virtual llvm::StringRef getSectionName(ReflectionSectionKind section) = 0;
};

/// Responsible for providing the Mach-O reflection section identifiers.
class SwiftObjectFileFormatMachO : SwiftObjectFileFormat {
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
    }
    llvm_unreachable("Section type not found.");
  }
};

/// Responsible for providing the ELF reflection section identifiers.
class SwiftObjectFileFormatELF : SwiftObjectFileFormat {
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
    }
    llvm_unreachable("Section type not found.");
  }
};

/// Responsible for providing the COFF reflection section identifiers
class SwiftObjectFileFormatCOFF : SwiftObjectFileFormat {
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
    }
    llvm_unreachable("Section  not found.");
  }
};
} // namespace swift
#endif // SWIFT_ABI_OBJECTFILE_H
