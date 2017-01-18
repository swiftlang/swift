//===--- ParameterList.h - Functions & closures parameter lists -*- C++ -*-===//
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
//
// This file defines the ParameterList class and support logic.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_PARAMETERLIST_H
#define SWIFT_AST_PARAMETERLIST_H

#include "swift/AST/Decl.h"
#include "swift/Basic/OptionSet.h"
#include "llvm/Support/TrailingObjects.h"

namespace swift {

/// This describes a list of parameters.  Each parameter descriptor is tail
/// allocated onto this list.
class alignas(ParamDecl *) ParameterList final :
    private llvm::TrailingObjects<ParameterList, ParamDecl *> {
  friend TrailingObjects;

  void *operator new(size_t Bytes) throw() = delete;
  void operator delete(void *Data) throw() = delete;
  void *operator new(size_t Bytes, void *Mem) throw() = delete;
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = 8);

  SourceLoc LParenLoc, RParenLoc;
  size_t numParameters;

  ParameterList(SourceLoc LParenLoc, size_t numParameters, SourceLoc RParenLoc)
    : LParenLoc(LParenLoc), RParenLoc(RParenLoc), numParameters(numParameters){}
  void operator=(const ParameterList&) = delete;
public:
  /// Create a parameter list with the specified parameters.
  static ParameterList *create(const ASTContext &C, SourceLoc LParenLoc,
                               ArrayRef<ParamDecl*> params,
                               SourceLoc RParenLoc);

  /// Create a parameter list with the specified parameters, with no location
  /// info for the parens.
  static ParameterList *create(const ASTContext &C,
                               ArrayRef<ParamDecl*> params) {
    return create(C, SourceLoc(), params, SourceLoc());
  }
 
  /// Create an empty parameter list.
  static ParameterList *createEmpty(const ASTContext &C,
                                    SourceLoc LParenLoc = SourceLoc(),
                                    SourceLoc RParenLoc = SourceLoc()) {
    return create(C, LParenLoc, {}, RParenLoc);
  }
  
  /// Create a parameter list for a single parameter lacking location info.
  static ParameterList *createWithoutLoc(ParamDecl *decl) {
    return create(decl->getASTContext(), decl);
  }
  
  /// Create an implicit 'self' decl for a method in the specified decl context.
  /// If 'static' is true, then this is self for a static method in the type.
  ///
  /// Note that this decl is created, but it is returned with an incorrect
  /// DeclContext that needs to be set correctly.  This is automatically handled
  /// when a function is created with this as part of its argument list.
  ///
  static ParameterList *createUnboundSelf(SourceLoc loc, DeclContext *DC);

  /// Create an implicit 'self' decl for a method in the specified decl context.
  /// If 'static' is true, then this is self for a static method in the type.
  ///
  /// Note that this decl is created, but it is returned with an incorrect
  /// DeclContext that needs to be set correctly.  This is automatically handled
  /// when a function is created with this as part of its argument list.
  static ParameterList *createSelf(SourceLoc loc, DeclContext *DC,
                                   bool isStatic = false,
                                   bool isInOut = false);

  SourceLoc getLParenLoc() const { return LParenLoc; }
  SourceLoc getRParenLoc() const { return RParenLoc; }
  
  typedef MutableArrayRef<ParamDecl*>::iterator iterator;
  typedef ArrayRef<ParamDecl*>::iterator const_iterator;
  iterator begin() { return getArray().begin(); }
  iterator end() { return getArray().end(); }
  const_iterator begin() const { return getArray().begin(); }
  const_iterator end() const { return getArray().end(); }
  
  MutableArrayRef<ParamDecl*> getArray() {
    return {getTrailingObjects<ParamDecl*>(), numParameters};
  }
  ArrayRef<ParamDecl*> getArray() const {
    return {getTrailingObjects<ParamDecl*>(), numParameters};
  }

  size_t size() const {
    return numParameters;
  }
  
  const ParamDecl *get(unsigned i) const {
    return getArray()[i];
  }
  
  ParamDecl *&get(unsigned i) {
    return getArray()[i];
  }

  const ParamDecl *operator[](unsigned i) const { return get(i); }
  ParamDecl *&operator[](unsigned i) { return get(i); }
  
  /// Change the DeclContext of any contained parameters to the specified
  /// DeclContext.
  void setDeclContextOfParamDecls(DeclContext *DC);
  
  
  /// Flags used to indicate how ParameterList cloning should operate.
  enum CloneFlags {
    /// The cloned ParamDecls should be marked implicit.
    Implicit = 0x01,
    /// The cloned pattern is for an inherited constructor; mark default
    /// arguments as inherited, and mark unnamed arguments as named.
    Inherited = 0x02
  };
  
  /// Make a duplicate copy of this parameter list.  This allocates copies of
  /// the ParamDecls, so they can be reparented into a new DeclContext.
  ParameterList *clone(const ASTContext &C,
                       OptionSet<CloneFlags> options = None) const;

  /// Return a TupleType or ParenType for this parameter list, written in terms
  /// of contextual archetypes.
  Type getType(const ASTContext &C) const;

  /// Return a TupleType or ParenType for this parameter list, written in terms
  /// of interface types.
  Type getInterfaceType(const ASTContext &C) const;

  /// Return the full function type for a set of curried parameter lists that
  /// returns the specified result type written in terms of interface types.
  static Type getFullInterfaceType(Type resultType, ArrayRef<ParameterList*> PL,
                                   const ASTContext &C);


  /// Return the full source range of this parameter.
  SourceRange getSourceRange() const;
  SourceLoc getStartLoc() const { return getSourceRange().Start; }
  SourceLoc getEndLoc() const { return getSourceRange().End; }

  void dump() const;
  void dump(raw_ostream &OS, unsigned Indent = 0) const;
  
  //  void print(raw_ostream &OS) const;
};

} // end namespace swift

#endif
