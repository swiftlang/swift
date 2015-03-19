//===--- Validation.h - Validation / errors for serialization ---*- c++ -*-===//
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

#ifndef SWIFT_SERIALIZATION_VALIDATION_H
#define SWIFT_SERIALIZATION_VALIDATION_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"

namespace swift {
namespace serialization {

  /// Describes whether a serialized module can be used by this compiler.
  enum class Status {
    /// The module is valid.
    Valid,

    /// The module file format is too old to be used by this version of the
    /// compiler.
    FormatTooOld,

    /// The module file format is too new to be used by this version of the
    /// compiler.
    FormatTooNew,

    /// The module file depends on another module that can't be loaded.
    MissingDependency,

    /// The module file is an overlay for a Clang module, which can't be found.
    MissingShadowedModule,

    /// The module file is malformed in some way.
    Malformed,

    /// The module documentation file is malformed in some way.
    MalformedDocumentation,

    /// The module file's name does not match the module it is being loaded
    /// into.
    NameMismatch,

    /// The module file was built for a different target platform.
    TargetIncompatible,

    /// The module file was built for a target newer than the current target.
    TargetTooNew
  };

  /// Returns true if the data looks like it contains a serialized AST.
  bool isSerializedAST(StringRef data);

  /// \see validateSerializedAST()
  struct ValidationInfo {
    StringRef name = {};
    StringRef targetTriple = {};
    size_t bytes = 0;
    Status status = Status::Malformed;
  };

  /// A collection of options that can be used to set up a new AST context
  /// before it has been created.
  ///
  /// Note that this is intended to be a transient data structure; as such,
  /// <strong>none of the string values added to it are copied</strong>.
  ///
  /// \sa validateSerializedAST()
  class ExtendedValidationInfo {
    SmallVector<StringRef, 4> ExtraClangImporterOpts;
    StringRef SDKPath;
    struct {
      unsigned IsSIB : 1;
      unsigned IsTestable : 1;
    } Bits;
  public:
    ExtendedValidationInfo() : Bits() {}

    StringRef getSDKPath() const { return SDKPath; }
    void setSDKPath(StringRef path) {
      assert(SDKPath.empty());
      SDKPath = path;
    }

    ArrayRef<StringRef> getExtraClangImporterOptions() const {
      return ExtraClangImporterOpts;
    }
    void addExtraClangImporterOption(StringRef option) {
      ExtraClangImporterOpts.push_back(option);
    }

    bool isSIB() const { return Bits.IsSIB; }
    void setIsSIB(bool val) {
        Bits.IsSIB = val;
    }
    bool isTestable() const { return Bits.IsTestable; }
    void setIsTestable(bool val) {
      Bits.IsTestable = val;
    }
  };

  /// Returns info about the serialized AST in the given data.
  ///
  /// If the returned status is anything but Status::Valid, the serialized data
  /// cannot be loaded by this version of the compiler. If the returned size is
  /// non-zero, it's possible to skip over this module's data, in case there is
  /// more data in the buffer. The returned name, which may be empty, directly
  /// points into the given data buffer.
  ///
  /// Note that this does not actually try to load the module or validate any
  /// of its dependencies; it only checks that it /can/ be loaded.
  ///
  /// \param data A buffer containing the serialized AST. Result information
  /// refers directly into this buffer.
  /// \param[out] extendedInfo If present, will be populated with additional
  /// compilation options serialized into the AST at build time that may be
  /// necessary to load it properly.
  ValidationInfo
  validateSerializedAST(StringRef data,
                        ExtendedValidationInfo *extendedInfo = nullptr);

} // end namespace serialization
} // end namespace swift

#endif
