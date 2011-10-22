//===- DiagnosticEngine.h - Diagnostic Display Engine -----------*- C++ -*-===//
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
//  This file declares the DiagnosticEngine class, which manages any diagnostics
//  emitted by Swift.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_DIAGNOSTICENGINE_H
#define SWIFT_BASIC_DIAGNOSTICENGINE_H

#include "swift/AST/LLVM.h"
#include "swift/AST/Identifier.h"    // FIXME: Layering violation.
#include "swift/AST/Type.h"          // FIXME: Layering violation.
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include <string>
#include <utility>

namespace llvm {
  class SourceMgr;
}

namespace swift {
  using llvm::ArrayRef;
  using llvm::StringRef;
  
  /// \brief Enumeration describing all of possible diagnostics.
  ///
  /// Each of the diagnostics described in Diagnostics.def has an entry in
  /// this enumeration type that uniquely identifies it.
  enum class DiagID : unsigned {
#define DIAG(KIND,ID,Category,Options,Text,Signature) ID,
  #include "Diagnostics.def"
  };

  /// \brief Describes a diagnostic along with its argument types.
  ///
  /// The diagnostics header introduces instances of this type for each 
  /// diagnostic, which provide both the set of argument types (used to
  /// check/convert the arguments at each call site) and the diagnostic ID
  /// (for other information about the diagnostic).
  template<typename ...ArgTypes>
  struct Diag {
    /// \brief The diagnostic ID corresponding to this diagnostic.
    DiagID ID;
  };

  namespace detail {
    /// \brief Describes how to pass a diagnostic argument of the given type.
    ///
    /// By default, diagnostic arguments are passed by value, because they
    /// tend to be small. Larger diagnostic arguments
    /// need to specialize this class template to pass by reference.
    template<typename T>
    struct PassArgument {
      typedef T type;
    };
  }
  
  /// \brief Describes the kind of diagnostic.
  ///
  enum class DiagnosticKind {
    Error,
    Warning,
    Note
  };
  
  /// \brief Describes the kind of diagnostic argument we're storing.
  ///
  enum class DiagnosticArgumentKind {
    String,
    Integer,
    Unsigned,
    Identifier,
    Type
  };

  /// \brief Variant type that holds a single diagnostic argument of a known
  /// type.
  ///
  /// All diagnostic arguments are converted to an instance of this class.
  class DiagnosticArgument {
    DiagnosticArgumentKind Kind;
    union {
      int IntegerVal;
      unsigned UnsignedVal;
      StringRef StringVal;
      Identifier IdentifierVal;
      Type TypeVal;
    };
    
  public:
    DiagnosticArgument(StringRef S)
      : Kind(DiagnosticArgumentKind::String), StringVal(S) {
    }

    DiagnosticArgument(int I) 
      : Kind(DiagnosticArgumentKind::Integer), IntegerVal(I) {
    }

    DiagnosticArgument(unsigned I) 
      : Kind(DiagnosticArgumentKind::Unsigned), UnsignedVal(I) {
    }

    DiagnosticArgument(Identifier I)
      : Kind(DiagnosticArgumentKind::Identifier), IdentifierVal(I) {
    }

    DiagnosticArgument(Type T)
      : Kind(DiagnosticArgumentKind::Type), TypeVal(T) {
    }

    DiagnosticArgumentKind getKind() const { return Kind; }

    StringRef getAsString() const {
      assert(Kind == DiagnosticArgumentKind::String);
      return StringVal;
    }

    int getAsInteger() const {
      assert(Kind == DiagnosticArgumentKind::Integer);
      return IntegerVal;
    }

    unsigned getAsUnsigned() const {
      assert(Kind == DiagnosticArgumentKind::Unsigned);
      return UnsignedVal;
    }

    Identifier getAsIdentifier() const {
      assert(Kind == DiagnosticArgumentKind::Identifier);
      return IdentifierVal;
    }

    Type getAsType() const {
      assert(Kind == DiagnosticArgumentKind::Type);
      return TypeVal;
    }
  };
  
  /// Diagnostic - This is a specific instance of a diagnostic along with all of
  /// the DiagnosticArguments that it requires. 
  class Diagnostic {
    DiagID ID;
    SmallVector<DiagnosticArgument, 3> Args;
  public:
    // All constructors are intentionally implicit.
    Diagnostic(Diag<> ID) : ID(ID.ID) {}
        
    template<typename ...ArgTypes>
    Diagnostic(Diag<ArgTypes...> ID,
               typename detail::PassArgument<ArgTypes>::type... VArgs)
      : ID(ID.ID) {
      DiagnosticArgument DiagArgs[] = { VArgs... };
      Args.append(DiagArgs, DiagArgs+sizeof(DiagArgs)/sizeof(DiagArgs[0]));
    }

    /*implicit*/Diagnostic(DiagID ID, ArrayRef<DiagnosticArgument> Args)
      : ID(ID), Args(Args.begin(), Args.end()) {}
    
    // Accessors.
    DiagID getID() const { return ID; }
    ArrayRef<DiagnosticArgument> getArgs() const { return Args; }
  };
  
  /// \brief Class responsible for formatting diagnostics and presenting them
  /// to the user.
  class DiagnosticEngine {
    /// \brief The source manager used to interpret source locations and
    /// display diagnostics.
    llvm::SourceMgr &SourceMgr;

    /// HadAnyError - True if any error diagnostics have been emitted.
    bool HadAnyError;

  public:
    explicit DiagnosticEngine(llvm::SourceMgr &SourceMgr)
      : SourceMgr(SourceMgr), HadAnyError(false) { }

    /// hadAnyError - return true if any *error* diagnostics have been emitted.
    bool hadAnyError() const {
      return HadAnyError;
    }
    
    /// \brief Emit a diagnostic using a preformatted array of diagnostic
    /// arguments.
    ///
    /// \param Loc The location to which the diagnostic refers in the source
    /// code.
    ///
    /// \param ID The diagnostic ID.
    ///
    /// \param Args The preformatted set of diagnostic arguments. The caller
    /// must ensure that the diagnostic arguments have the appropriate type.
    void diagnose(SourceLoc Loc, DiagID ID, ArrayRef<DiagnosticArgument> Args);

    void diagnose(SourceLoc Loc, const Diagnostic &D) {
      diagnose(Loc, D.getID(), D.getArgs());
    }
    
    /// \brief Emit a diagnostic with no arguments.
    ///
    /// \param Loc The location to which the diagnostic refers in the source
    /// code.
    ///
    /// \param ID The diagnostic to be emitted.
    void diagnose(SourceLoc Loc, Diag<> ID) {
      diagnose(Loc, ID.ID, ArrayRef<DiagnosticArgument>());
    }    

    /// \brief Emit a diagnostic with the given set of diagnostic arguments.
    ///
    /// \param Loc The location to which the diagnostic refers in the source
    /// code.
    ///
    /// \param ID The diagnostic to be emitted.
    ///
    /// \param Args The diagnostic arguments, which will be converted to
    /// the types expected by the diagnostic \p ID.
    template<typename ...ArgTypes>
    void diagnose(SourceLoc Loc, Diag<ArgTypes...> ID,
                  typename detail::PassArgument<ArgTypes>::type... Args) {
      DiagnosticArgument DiagArgs[] = { Args... };
      diagnose(Loc, ID.ID, 
               ArrayRef<DiagnosticArgument>(DiagArgs, sizeof...(Args)));
    }    
  };
} // end namespace swift

#endif
