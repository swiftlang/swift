//===--- Outlining.h - Value operation outlining ----------------*- C++ -*-===//
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
// This file defines interfaces for outlined value witnesses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_OUTLINING_H
#define SWIFT_IRGEN_OUTLINING_H

#include "IRGen.h"
#include "LocalTypeDataKind.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/TaggedUnion.h"
#include "swift/IRGen/GenericRequirement.h"
#include "swift/SIL/SILType.h"
#include "llvm/ADT/MapVector.h"

namespace llvm {
  class Value;
  class Type;
}

namespace swift {
class CanGenericSignature;
class CanType;
enum IsInitialization_t : bool;
enum IsTake_t : bool;
class SILType;
class NominalTypeDecl;

namespace irgen {
class Address;
class Explosion;
class IRGenFunction;
class IRGenModule;
class TypeInfo;

enum LayoutIsNeeded_t : bool {
  LayoutIsNotNeeded = false,
  LayoutIsNeeded = true
};

enum DeinitIsNeeded_t : bool {
  DeinitIsNotNeeded = false,
  DeinitIsNeeded = true
};

/// Emit outlined value operations.
///
/// The typical use-pattern is:
/// - construct it
///   OutliningMetadataCollector::OutliningMetadataCollector
/// - collect polymorphic values required for outlining
///   TypeInfo::collectMetadataForOutlining
/// - materialize the arguments for those values in the caller
///   OutliningMetadataCollector::materialize
/// - emit the call to the outlined value function
///   OutliningMetadataCollector::emitCallToOutlined(Copy|Destroy|Release)
///
/// For custom outlined functions (e.g. the outlined consume function for enums)
/// the use-pattern is:
/// - construct it
///   OutliningMetadataCollector::OutliningMetadataCollector
/// - collect polymorphic values required for outlining
///   TypeInfo::collectMetadataForOutlining
/// - materialize the arguments for those values in the caller
///   OutliningMetadataCollector::materialize
/// - add the polymorphic arguments to the list of arguments to be passed
///   OutliningMetadataCollector::addPolymorphicArguments
/// - when creating the outlined function, add the polymorphic parameters to its
///   signature
///   OutliningMetadataCollector::addPolymorphicParameterTypes
/// - when emitting the outlined function, after binding the custom parameters,
///   bind the polymorphic parameters
///   OutliningMetadataCollector::bindPolymorphicParameters
class OutliningMetadataCollector {
public:
  SILType T;
  IRGenFunction &IGF;
  const unsigned needsLayout : 1;
  const unsigned needsDeinit : 1;

  OutliningMetadataCollector(SILType T, IRGenFunction &IGF,
                             LayoutIsNeeded_t needsLayout,
                             DeinitIsNeeded_t needsDeinitTypes);
  unsigned size() const;

  // If any local type data is needed for \p type, add it.
  //
  // NOTE: To be called from TypeData instances.
  void collectTypeMetadata(SILType type);

  void emitCallToOutlinedCopy(Address dest, Address src,
                              SILType T, const TypeInfo &ti,
                              IsInitialization_t isInit, IsTake_t isTake) const;
  void emitCallToOutlinedDestroy(Address addr, SILType T,
                                 const TypeInfo &ti) const;
  void emitCallToOutlinedRelease(Address addr, SILType T, const TypeInfo &ti,
                                 Atomicity atomicity) const;

  void addPolymorphicArguments(SmallVectorImpl<llvm::Value *> &args) const;
  void
  addPolymorphicParameterTypes(SmallVectorImpl<llvm::Type *> &paramTys) const;
  void bindPolymorphicParameters(IRGenFunction &helperIGF,
                                 Explosion &params) const;
  void materialize();

private:
  void collectTypeMetadataForLayout(SILType ty);
  void collectTypeMetadataForDeinit(SILType ty);

  /// Emulates the following enum with associated values:
  ///
  /// enum State {
  ///   case empty
  ///   enum Collecting {
  ///     enum Element {
  ///       case metadataForFormal(CanType)
  ///       case metadataForRepresentation(SILType)
  ///     }
  ///     case elements([Element])
  ///     case environment
  ///   }
  ///   case collecting(Collecting)
  ///   enum Collected {
  ///     case elements([LocalTypeDataKey : llvm::Value *])
  ///     case environment(SubstitutionMap [GenericRequirement : llvm::Value *])
  ///   }
  ///   case collected(Collected)
  /// }
  class State {
  public:
    enum class CollectionKind {
      Elements,
      Environment,
    };
    enum class ElementKind {
      MetadataForFormal,
      MetadataForRepresentation,
    };

  private:
    struct Empty {};
    class Collecting {
      struct Elements {
        class Element {
          struct MetadataForFormal {
            CanType ty;
          };
          struct MetadataForRepresentation {
            SILType ty;
          };
          using Payload =
              TaggedUnion<MetadataForFormal, MetadataForRepresentation>;
          Payload payload;
          Element(Payload payload) : payload(payload) {}

        public:
          using Kind = ElementKind;
          operator Kind() {
            if (payload.isa<MetadataForFormal>())
              return Kind::MetadataForFormal;
            return Kind::MetadataForRepresentation;
          }
          static Element metadataForFormal(CanType ty) {
            return {MetadataForFormal{ty}};
          }
          static Element metadataForRepresentation(SILType ty) {
            return {MetadataForRepresentation{ty}};
          }
          CanType getFormalType() {
            return payload.get<MetadataForFormal>().ty;
          }
          SILType getRepresentationType() {
            return payload.get<MetadataForRepresentation>().ty;
          }
        };
        llvm::SmallVector<Element, 4> elements;
      };
      struct Environment {};
      using Payload = TaggedUnion<Elements, Environment>;
      Payload payload;

    public:
      Collecting() : payload(Elements{}) {}
      using Kind = CollectionKind;
      operator Kind() const {
        if (payload.isa<Elements>()) {
          return Kind::Elements;
        }
        return Kind::Environment;
      }
      void addRepresentationTypeMetadata(SILType ty) {
        if (*this == Kind::Environment)
          return;
        payload.get<Elements>().elements.push_back(
            Elements::Element::metadataForRepresentation(ty));
      }
      void addFormalTypeMetadata(CanType ty) {
        if (*this == Kind::Environment)
          return;
        payload.get<Elements>().elements.push_back(
            Elements::Element::metadataForFormal(ty));
      }
      void addValueTypeWithDeinit(SILType ty) {
        if (*this == Kind::Environment)
          return;
        payload = {Environment{}};
      }
      Elements &getElements() { return payload.get<Elements>(); };
    };

  public:
    class Collected {
    public:
      struct Elements {
        llvm::MapVector<LocalTypeDataKey, llvm::Value *> Values;
      };
      struct Environment {
        llvm::MapVector<GenericRequirement, llvm::Value *> Requirements;
        SubstitutionMap Subs;
      };

    private:
      using Payload = TaggedUnion<Elements, Environment>;
      Payload payload;
      Collected(Payload payload) : payload(payload) {}

    public:
      using Kind = CollectionKind;
      operator Kind() const {
        if (payload.isa<Elements>())
          return Kind::Elements;
        return Kind::Environment;
      }
      static Collected elements() { return {Elements{}}; };
      static Collected environment(SubstitutionMap subs) {
        return {Environment{{}, subs}};
      }
      Elements &getElements() { return payload.get<Elements>(); }
      Elements const &getElements() const { return payload.get<Elements>(); }
      Environment &getEnvironment() { return payload.get<Environment>(); }
      Environment const &getEnvironment() const {
        return payload.get<Environment>();
      }
      unsigned size() const {
        switch (*this) {
        case Kind::Elements:
          return getElements().Values.size();
        case Kind::Environment:
          return getEnvironment().Requirements.size();
        }
      }
    };

  private:
    using Payload = TaggedUnion<Empty, Collecting, Collected>;
    Payload payload;

  public:
    State() : payload(Empty{}) {}
    enum class Kind {
      Empty,
      Collecting,
      Collected,
    };
    operator Kind() const {
      if (payload.isa<Empty>())
        return Kind::Empty;
      if (payload.isa<Collecting>())
        return Kind::Collecting;
      return Kind::Collected;
    }
    Collecting &getCollecting() {
      if (payload.isa<Empty>())
        payload = Collecting{};
      return payload.get<Collecting>();
    }
    Collected const &getCollected() const { return payload.get<Collected>(); }
    Collected::Elements &setCollectedElements() {
      assert(*this == Kind::Collecting);
      payload = Collected::elements();
      return payload.get<Collected>().getElements();
    }
    Collected::Environment &setCollectedEnvironment(SubstitutionMap subs) {
      assert(*this == Kind::Collecting);
      payload = Collected::environment(subs);
      return payload.get<Collected>().getEnvironment();
    }
  };
  State state;
  bool hasFinished() const {
    return state == State::Kind::Empty || state == State::Kind::Collected;
  }

  void materializeFormalTypeMetadata(CanType ty,
                                     State::Collected::Elements &into);
  void materializeRepresentationTypeMetadata(SILType ty,
                                             State::Collected::Elements &into);

  friend class IRGenModule;
};

inline unsigned OutliningMetadataCollector::size() const {
  return state.getCollected().size();
}

std::pair<CanType, CanGenericSignature>
getTypeAndGenericSignatureForManglingOutlineFunction(SILType type);


}
}

#endif
