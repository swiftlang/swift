//===--- APINotesReader.h - API Notes Reader ----------------------*- C++ -*-===//
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
// This file defines the \c APINotesReader class that reads source
// API notes data providing additional information about source code as
// a separate input, such as the non-nil/nilable annotations for
// method parameters.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_API_NOTES_READER_H
#define SWIFT_API_NOTES_READER_H

#include "swift/APINotes/Types.h"
#include "swift/Basic/Optional.h"
#include "llvm/Support/MemoryBuffer.h"
#include <memory>

namespace swift {
namespace api_notes {

/// A class that reads API notes data from a binary file that was written by
/// the \c APINotesWriter.
class APINotesReader {
  class Implementation;

  Implementation &Impl;

  APINotesReader(std::unique_ptr<llvm::MemoryBuffer> inputBuffer, bool &failed);

public:
  /// Create a new API notes reader from the given member buffer, which
  /// contains the contents of a binary API notes file.
  ///
  /// \returns the new API notes reader, or null if an error occurred.
  static std::unique_ptr<APINotesReader> 
  get(std::unique_ptr<llvm::MemoryBuffer> inputBuffer);

  ~APINotesReader();

  APINotesReader(const APINotesReader &) = delete;
  APINotesReader &operator=(const APINotesReader &) = delete;

  /// Retrieve the name of the module for which this reader is providing API
  /// notes.
  StringRef getModuleName() const;

  /// Look for information regarding the given Objective-C class.
  ///
  /// \param name The name of the class we're looking for.
  ///
  /// \returns The ID and information about the class, if known.
  Optional<std::pair<ContextID, ObjCContextInfo>>
  lookupObjCClass(StringRef name);

  /// Look for information regarding the given Objective-C protocol.
  ///
  /// \param name The name of the protocol we're looking for.
  ///
  /// \returns The ID and information about the protocol, if known.
  Optional<std::pair<ContextID, ObjCContextInfo>>
  lookupObjCProtocol(StringRef name);

  /// Look for information regarding the given Objective-C property in
  /// the given context.
  ///
  /// \param contextID The ID that references the context we are looking for.
  /// \param name The name of the property we're looking for.
  ///
  /// \returns Information about the property, if known.
  Optional<ObjCPropertyInfo> lookupObjCProperty(ContextID contextID,
                                                StringRef name);

  /// Look for information regarding the given Objective-C method in
  /// the given context.
  ///
  /// \param contextID The ID that references the context we are looking for.
  /// \param selector The selector naming the method we're looking for.
  /// \param isInstanceMethod Whether we are looking for an instance method.
  ///
  /// \returns Information about the method, if known.
  Optional<ObjCMethodInfo> lookupObjCMethod(ContextID contextID,
                                            ObjCSelectorRef selector,
                                            bool isInstanceMethod);

  /// Look for information regarding the given global variable.
  ///
  /// \param name The name of the global variable.
  ///
  /// \returns information about the global variable, if known.
  Optional<GlobalVariableInfo> lookupGlobalVariable(StringRef name);

  /// Look for information regarding the given global function.
  ///
  /// \param name The name of the global function.
  ///
  /// \returns information about the global function, if known.
  Optional<GlobalFunctionInfo> lookupGlobalFunction(StringRef name);

  /// Visitor used when walking the contents of the API notes file.
  class Visitor {
  public:
    virtual ~Visitor();

    /// Visit an Objective-C class.
    virtual void visitObjCClass(ContextID contextID, StringRef name,
                                const ObjCContextInfo &info);

    /// Visit an Objective-C protocol.
    virtual void visitObjCProtocol(ContextID contextID, StringRef name,
                                   const ObjCContextInfo &info);

    /// Visit an Objective-C method.
    virtual void visitObjCMethod(ContextID contextID, StringRef selector,
                                 bool isInstanceMethod,
                                 const ObjCMethodInfo &info);

    /// Visit an Objective-C property.
    virtual void visitObjCProperty(ContextID contextID, StringRef name,
                                   const ObjCPropertyInfo &info);

    /// Visit a global variable.
    virtual void visitGlobalVariable(StringRef name,
                                     const GlobalVariableInfo &info);

    /// Visit a global function.
    virtual void visitGlobalFunction(StringRef name,
                                     const GlobalFunctionInfo &info);
  };

  /// Visit the contents of the API notes file, passing each entity to the
  /// given visitor.
  void visit(Visitor &visitor);
};

} // end namespace api_notes
} // end namespace swift

#endif // LLVM_SWIFT_API_NOTES_READER_H
