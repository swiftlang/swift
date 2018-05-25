//===--- TypeCheckRequest.h - Type Checking Request -------------*- C++ -*-===//
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
//  This file defines the TypeCheckRequest type, which describes a request
//  to the type checker to compute certain information.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_TYPE_CHECK_REQUEST_H
#define SWIFT_SEMA_TYPE_CHECK_REQUEST_H

#include "swift/AST/Identifier.h"
#include "swift/AST/Type.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/ErrorHandling.h"
#include <cassert>
#include <utility>

namespace swift {

class ClassDecl;
class DeclContext;
class EnumDecl;
class ExtensionDecl;
class ProtocolDecl;
class TypeDecl;
class TypeRepr;

/// Describes the information needed to perform name lookup into a
/// declaration context.
struct DeclContextLookupInfo {
  DeclContext *DC;
  DeclName Name;
  SourceLoc Loc;

  friend bool operator==(const DeclContextLookupInfo &x,
                         const DeclContextLookupInfo &y) {
    return x.DC == y.DC && x.Name == y.Name;
  }

  friend bool operator!=(const DeclContextLookupInfo &x,
                         const DeclContextLookupInfo &y) {
    return !(x == y);
  }
};

/// A request to the type checker to compute some particular kind of
/// information.
class TypeCheckRequest {
public:
  /// Describes the kind of request.
  enum Kind : uint8_t {
#define TYPE_CHECK_REQUEST(Request,Payload) \
    Request,

#include "swift/Sema/TypeCheckRequestKinds.def"
  } TheKind;

private:
  /// The payload of the request, which differs based on the request kind.
  union PayloadType {
    PayloadType() { }

#define TYPE_CHECK_REQUEST_PAYLOAD(PayloadName,...) \
    __VA_ARGS__ PayloadName;

#include "swift/Sema/TypeCheckRequestPayloads.def"
  } Payload;

  /// Describes the kind of payload expected.
  ///
  /// The enumerators in this enumeration type are expected to map 1-1 to the
  /// fields of the \c Payload union.
  enum class PayloadKind {
#define TYPE_CHECK_REQUEST_PAYLOAD(PayloadName,...)     \
    PayloadName,

#include "swift/Sema/TypeCheckRequestPayloads.def"
  };

  /// Determine the payload kind for the given type check request kind.
  static PayloadKind getPayloadKind(Kind kind) {
    switch (kind) {
#define TYPE_CHECK_REQUEST(Request,PayloadName) \
    case Request:                               \
      return PayloadKind::PayloadName;

#include "swift/Sema/TypeCheckRequestKinds.def"
    }

    llvm_unreachable("Unhandled PayloadKind in switch.");
  }

public:
  // The payload types.
#define TYPE_CHECK_REQUEST_PAYLOAD(PayloadName,...)     \
  using PayloadName##PayloadType = __VA_ARGS__;

#include "swift/Sema/TypeCheckRequestPayloads.def"
  
  // Constructors.
#define TYPE_CHECK_REQUEST_PAYLOAD(PayloadName,...)                     \
  TypeCheckRequest(Kind kind, __VA_ARGS__ payload) : TheKind(kind) {    \
    assert(getPayloadKind(kind) == PayloadKind::PayloadName);           \
    Payload.PayloadName = payload;                                      \
  }

#include "swift/Sema/TypeCheckRequestPayloads.def"

  TypeCheckRequest(const TypeCheckRequest &T) { *this = T; }

  TypeCheckRequest& operator=(const TypeCheckRequest &T) {
    TheKind = T.getKind();
    switch (getPayloadKind(TheKind)) {
    case PayloadKind::Class:
      Payload.Class = T.Payload.Class;
      break;
    case PayloadKind::Enum:
      Payload.Enum = T.Payload.Enum;
      break;
    case PayloadKind::InheritedClauseEntry:
      new (&Payload.InheritedClauseEntry)
        std::pair<llvm::PointerUnion<TypeDecl *, ExtensionDecl *>, unsigned>();
      Payload.InheritedClauseEntry = T.Payload.InheritedClauseEntry;
      break;
    case PayloadKind::Protocol:
      Payload.Protocol = T.Payload.Protocol;
      break;
    case PayloadKind::DeclContextLookup:
      new (&Payload.DeclContextLookup) DeclContextLookupInfo();
      Payload.DeclContextLookup = T.Payload.DeclContextLookup;
      break;
    case PayloadKind::TypeResolution:
      new (&Payload.InheritedClauseEntry)
        std::tuple<TypeRepr *, DeclContext *, unsigned>();
      Payload.TypeResolution = T.Payload.TypeResolution;
      break;
    case PayloadKind::TypeDeclResolution:
      Payload.TypeDeclResolution = T.Payload.TypeDeclResolution;
      break;
    }
    return *this;
  }

  /// Determine the kind of type check request.
  Kind getKind() const { return TheKind; }

  // Payload retrieval.
#define TYPE_CHECK_REQUEST_PAYLOAD(PayloadName,...)                     \
  __VA_ARGS__ get##PayloadName##Payload() const {                       \
    assert(getPayloadKind(TheKind) == PayloadKind::PayloadName);        \
    return Payload.PayloadName;                                         \
  }

#include "swift/Sema/TypeCheckRequestPayloads.def"

  /// Retrieve the location at which this request was initiated.
  SourceLoc getLoc() const;

  /// Retrieve the declaration to which this request was anchored.
  ///
  /// A request is anchored on a declaration if the request is
  /// specifically about that declaration, e.g., the superclass of a
  /// class or some inheritance clause entry of a type.
  Decl *getAnchor() const;

  friend bool operator==(const TypeCheckRequest &x, const TypeCheckRequest &y);
};

/// A callback used to check whether a particular dependency of this
/// operation has been satisfied. If so, it returns \c false. If not,
/// the dependency will be recorded and this operation returns \c true.
using UnsatisfiedDependency = llvm::function_ref<bool(TypeCheckRequest)>;

// Create requestXXX functions to more easily form type check requests
// of the appropriate type.
#define TYPE_CHECK_REQUEST(Request,PayloadName)                                \
inline TypeCheckRequest request##Request(                                      \
                          TypeCheckRequest::PayloadName##PayloadType payload) {\
  return TypeCheckRequest(TypeCheckRequest::Request, payload);                 \
}
#include "swift/Sema/TypeCheckRequestKinds.def"

/// Compare two type checking requests for equality.
bool operator==(const TypeCheckRequest &x, const TypeCheckRequest &y);
inline bool operator!=(const TypeCheckRequest &x, const TypeCheckRequest &y) {
  return !(x == y);
}

}

#endif /* SWIFT_SEMA_TYPE_CHECK_REQUEST_H */
