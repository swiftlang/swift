//===--- TypeCheckSwitchStmt.cpp - Switch Exhaustiveness and Type Checks --===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file contains an algorithm for checking the exhaustiveness of switches.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/Pattern.h"

#include <llvm/ADT/APInt.h>
#include <llvm/ADT/APFloat.h>

#include <numeric>
#include <forward_list>

using namespace swift;

#define DEBUG_TYPE "TypeCheckSwitchStmt"

namespace {
  struct DenseMapAPIntKeyInfo {
    static inline APInt getEmptyKey() { return APInt(); }

    static inline APInt getTombstoneKey() {
      return APInt::getAllOnesValue(/*bitwidth*/1);
    }

    static unsigned getHashValue(const APInt &Key) {
      return static_cast<unsigned>(hash_value(Key));
    }

    static bool isEqual(const APInt &LHS, const APInt &RHS) {
      return LHS.getBitWidth() == RHS.getBitWidth() && LHS == RHS;
    }
  };

  struct DenseMapAPFloatKeyInfo {
    static inline APFloat getEmptyKey() { return APFloat(APFloat::Bogus(), 1); }
    static inline APFloat getTombstoneKey() { return APFloat(APFloat::Bogus(), 2); }
    
    static unsigned getHashValue(const APFloat &Key) {
      return static_cast<unsigned>(hash_value(Key));
    }
    
    static bool isEqual(const APFloat &LHS, const APFloat &RHS) {
      return LHS.bitwiseIsEqual(RHS);
    }
  };
}

namespace {

  /// The SpaceEngine encapsulates an algorithm for computing the exhaustiveness
  /// of a switch statement using an algebra of spaces described by Fengyun Liu
  /// and an algorithm for computing warnings for pattern matching by
  /// Luc Maranget.
  ///
  /// The main algorithm centers around the computation of the difference and
  /// the containment of the "Spaces" given in each case, which reduces the
  /// definition of exhaustiveness to checking if the difference of the space
  /// 'S' of the user's written patterns and the space 'T' of the pattern
  /// condition is empty.
  struct SpaceEngine {
    enum class SpaceKind : uint8_t {
      Empty,
      Type,
      Constructor,
      Disjunct,
      BooleanConstant,
      UnknownCase,
    };

    // The order of cases is used for disjunctions: a disjunction's
    // DowngradeToWarning condition is the std::min of its spaces'.
    enum class DowngradeToWarning {
      No,
      ForUnknownCase,
      ForSwift3Case,

      LAST = ForSwift3Case
    };

    enum UnknownCase_t {
      UnknownCase
    };

    /// A data structure for conveniently pattern-matching on the kinds of
    /// two spaces.
    struct PairSwitch {
    public:
      constexpr PairSwitch(SpaceKind pair1, SpaceKind pair2)
        : Data(static_cast<uint8_t>(pair1) | (static_cast<uint8_t>(pair2) << 8))
        {}

      constexpr bool operator==(const PairSwitch other) const {
        return Data == other.Data;
      }

      constexpr operator int() const {
        return Data;
      }

    private:
      uint16_t Data;

      PairSwitch (const PairSwitch&) = delete;
      PairSwitch& operator= (const PairSwitch&) = delete;
    };

  #define PAIRCASE(XS, YS) case PairSwitch(XS, YS)
    
    class Space final : public RelationalOperationsBase<Space> {
    private:
      SpaceKind Kind;
      llvm::PointerIntPair<Type, 1, bool> TypeAndVal;

      // In type space, we reuse HEAD to help us print meaningful name, e.g.,
      // tuple element name in fixits.
      Identifier Head;
      std::forward_list<Space> Spaces;

      // NB: This constant is arbitrary.  Anecdotally, the Space Engine is
      // capable of efficiently handling Spaces of around size 200, but it would
      // potentially push an enormous fixit on the user.
      static const size_t MAX_SPACE_SIZE = 128;

      size_t computeSize(TypeChecker &TC, const DeclContext *DC,
                         SmallPtrSetImpl<TypeBase *> &cache) const {
        switch (getKind()) {
        case SpaceKind::Empty:
          return 0;
        case SpaceKind::BooleanConstant:
          return 1;
        case SpaceKind::UnknownCase:
          return isAllowedButNotRequired() ? 0 : 1;
        case SpaceKind::Type: {
          if (!canDecompose(getType(), DC)) {
            return 1;
          }
          cache.insert(getType().getPointer());

          SmallVector<Space, 4> spaces;
          decompose(TC, DC, getType(), spaces);
          size_t acc = 0;
          for (auto &sp : spaces) {
            // Decomposed pattern spaces grow with the sum of the subspaces.
            acc += sp.computeSize(TC, DC, cache);
          }
          
          cache.erase(getType().getPointer());
          return acc;
        }
        case SpaceKind::Constructor: {
          size_t acc = 1;
          for (auto &sp : getSpaces()) {
            // Break self-recursive references among enum arguments.
            if (sp.getKind() == SpaceKind::Type
                  && cache.count(sp.getType().getPointer())) {
              continue;
            }
            
            // Constructor spaces grow with the product of their arguments.
            acc *= sp.computeSize(TC, DC, cache);
          }
          return acc;
        }
        case SpaceKind::Disjunct: {
          size_t acc = 0;
          for (auto &sp : getSpaces()) {
            // Disjoint grow with the sum of the subspaces.
            acc += sp.computeSize(TC, DC, cache);
          }
          return acc;
        }
        }
      }

      explicit Space(Type T, Identifier NameForPrinting)
        : Kind(SpaceKind::Type), TypeAndVal(T),
          Head(NameForPrinting), Spaces({}){}
      explicit Space(UnknownCase_t, bool allowedButNotRequired)
        : Kind(SpaceKind::UnknownCase),
          TypeAndVal(Type(), allowedButNotRequired), Head(Identifier()),
          Spaces({}) {}
      explicit Space(Type T, Identifier H, bool downgrade,
                     ArrayRef<Space> SP)
        : Kind(SpaceKind::Constructor), TypeAndVal(T, downgrade), Head(H),
          Spaces(SP.begin(), SP.end()) {}
      explicit Space(Type T, Identifier H, bool downgrade,
                     std::forward_list<Space> SP)
        : Kind(SpaceKind::Constructor), TypeAndVal(T, downgrade), Head(H),
          Spaces(SP) {}
      explicit Space(ArrayRef<Space> SP)
        : Kind(SpaceKind::Disjunct), TypeAndVal(Type()),
          Head(Identifier()), Spaces(SP.begin(), SP.end()) {}
      explicit Space(bool C)
        : Kind(SpaceKind::BooleanConstant), TypeAndVal(Type(), C),
          Head(Identifier()), Spaces({}) {}
    public:
      explicit Space()
        : Kind(SpaceKind::Empty), TypeAndVal(Type()), Head(Identifier()),
          Spaces({}) {}

      static Space forType(Type T, Identifier NameForPrinting) {
        if (T->isStructurallyUninhabited())
          return Space();
        return Space(T, NameForPrinting);
      }
      static Space forUnknown(bool allowedButNotRequired) {
        return Space(UnknownCase, allowedButNotRequired);
      }
      static Space forConstructor(Type T, Identifier H, bool downgrade,
                                  ArrayRef<Space> SP) {
        if (llvm::any_of(SP, std::mem_fn(&Space::isEmpty))) {
          // A constructor with an unconstructible parameter can never actually
          // be used.
          return Space();
        }
        return Space(T, H, downgrade, SP);
      }
      static Space forConstructor(Type T, Identifier H, bool downgrade,
                                  std::forward_list<Space> SP) {
        // No need to filter SP here; this is only used to copy other
        // Constructor spaces.
        return Space(T, H, downgrade, SP);
      }
      static Space forBool(bool C) {
        return Space(C);
      }
      static Space forDisjunct(ArrayRef<Space> SP) {
        SmallVector<Space, 4> spaces(SP.begin(), SP.end());
        spaces.erase(
            std::remove_if(spaces.begin(), spaces.end(),
                           [](const Space &space) { return space.isEmpty(); }),
            spaces.end());

        if (spaces.empty())
          return Space();
        if (spaces.size() == 1)
          return spaces.front();

        return Space(spaces);
      }

      bool operator==(const Space &other) const {
        return Kind == other.Kind && TypeAndVal == other.TypeAndVal &&
               Head == other.Head && Spaces == other.Spaces;
      }

      SpaceKind getKind() const { return Kind; }

      void dump() const LLVM_ATTRIBUTE_USED;

      size_t getSize(TypeChecker &TC, const DeclContext *DC) const {
        SmallPtrSet<TypeBase *, 4> cache;
        return computeSize(TC, DC, cache);
      }

      static size_t getMaximumSize() {
        return MAX_SPACE_SIZE;
      }

      bool isEmpty() const { return getKind() == SpaceKind::Empty; }
      
      bool canDowngradeToWarning() const {
        assert((getKind() == SpaceKind::Type
                || getKind() == SpaceKind::Constructor)
               && "Wrong kind of space tried to access downgrade");
        return TypeAndVal.getInt();
      }

      bool isAllowedButNotRequired() const {
        assert(getKind() == SpaceKind::UnknownCase
               && "Wrong kind of space tried to access not-required flag");
        return TypeAndVal.getInt();
      }

      Type getType() const {
        assert((getKind() == SpaceKind::Type
                || getKind() == SpaceKind::Constructor)
               && "Wrong kind of space tried to access space type");
        return TypeAndVal.getPointer();
      }

      Identifier getHead() const {
        assert(getKind() == SpaceKind::Constructor
               && "Wrong kind of space tried to access head");
        return Head;
      }

      Identifier getPrintingName() const {
        assert(getKind() == SpaceKind::Type
               && "Wrong kind of space tried to access printing name");
        return Head;
      }

      const std::forward_list<Space> &getSpaces() const {
        assert((getKind() == SpaceKind::Constructor
                || getKind() == SpaceKind::Disjunct)
               && "Wrong kind of space tried to access subspace list");
        return Spaces;
      }

      bool getBoolValue() const {
        assert(getKind() == SpaceKind::BooleanConstant
                && "Wrong kind of space tried to access bool value");
        return TypeAndVal.getInt();
      }

      // An optimization that computes if the difference of this space and
      // another space is empty.
      bool isSubspace(const Space &other, TypeChecker &TC,
                      const DeclContext *DC) const {
        if (this->isEmpty()) {
          return true;
        }

        if (other.isEmpty()) {
          return false;
        }

        switch (PairSwitch(getKind(), other.getKind())) {
        PAIRCASE (SpaceKind::Disjunct, SpaceKind::Empty):
        PAIRCASE (SpaceKind::Disjunct, SpaceKind::Type):
        PAIRCASE (SpaceKind::Disjunct, SpaceKind::Constructor):
        PAIRCASE (SpaceKind::Disjunct, SpaceKind::Disjunct):
        PAIRCASE (SpaceKind::Disjunct, SpaceKind::BooleanConstant):
        PAIRCASE (SpaceKind::Disjunct, SpaceKind::UnknownCase): {
          // (S1 | ... | Sn) <= S iff (S1 <= S) && ... && (Sn <= S)
          for (auto &space : this->getSpaces()) {
            if (!space.isSubspace(other, TC, DC)) {
              return false;
            }
          }
          return true;
        }
        PAIRCASE (SpaceKind::Type, SpaceKind::Type): {
          // Optimization: Are the types equal?  If so, the space is covered.
          if (this->getType()->isEqual(other.getType())) {
            return true;
          }

          // (_ : Ty1) <= (_ : Ty2) iff D(Ty1) == D(Ty2)
          if (canDecompose(this->getType(), DC)) {
            Space or1Space = decompose(TC, DC, this->getType());
            if (or1Space.isSubspace(other, TC, DC)) {
              return true;
            }
          }

          if (canDecompose(other.getType(), DC)) {
            Space or2Space = decompose(TC, DC, other.getType());
            return this->isSubspace(or2Space, TC, DC);
          }

          return true;
        }
        PAIRCASE (SpaceKind::Type, SpaceKind::Disjunct): {
          // (_ : Ty1) <= (S1 | ... | Sn) iff (S1 <= S) || ... || (Sn <= S)
          for (auto &dis : other.getSpaces()) {
            if (this->isSubspace(dis, TC, DC)) {
              return true;
            }
          }

          // (_ : Ty1) <= (S1 | ... | Sn) iff D(Ty1) <= (S1 | ... | Sn)
          if (!canDecompose(this->getType(), DC)) {
            return false;
          }
          Space or1Space = decompose(TC, DC, this->getType());
          return or1Space.isSubspace(other, TC, DC);
        }
        PAIRCASE (SpaceKind::Type, SpaceKind::Constructor): {
          // (_ : Ty1) <= H(p1 | ... | pn) iff D(Ty1) <= H(p1 | ... | pn)
          if (canDecompose(this->getType(), DC)) {
            Space or1Space = decompose(TC, DC, this->getType());
            return or1Space.isSubspace(other, TC, DC);
          }
          // An undecomposable type is always larger than its constructor space.
          return false;
        }
        PAIRCASE (SpaceKind::Type, SpaceKind::UnknownCase):
          return false;

        PAIRCASE (SpaceKind::Constructor, SpaceKind::Type):
          // Typechecking guaranteed this constructor is a subspace of the type.
          return true;
        PAIRCASE (SpaceKind::UnknownCase, SpaceKind::Type):
          return true;
        PAIRCASE (SpaceKind::BooleanConstant, SpaceKind::Type):
          return other.getType()->isBool();
        PAIRCASE (SpaceKind::Constructor, SpaceKind::Constructor): {
          // Optimization: If the constructor heads don't match, subspace is
          // impossible.
          if (this->Head != other.Head) {
            return false;
          }
          
          // Special Case: Short-circuit comparisons with payload-less
          // constructors.
          if (other.getSpaces().empty()) {
            return true;
          }

          // H(a1, ..., an) <= H(b1, ..., bn) iff a1 <= b1 && ... && an <= bn
          auto i = this->getSpaces().begin();
          auto j = other.getSpaces().begin();
          for (; i != this->getSpaces().end() && j != other.getSpaces().end();
               ++i, ++j) {
            if (!(*i).isSubspace(*j, TC, DC)) {
              return false;
            }
          }
          return true;
        }
        PAIRCASE (SpaceKind::Constructor, SpaceKind::UnknownCase):
          for (auto &param : this->getSpaces()) {
            if (param.isSubspace(other, TC, DC)) {
              return true;
            }
          }
          return false;

        PAIRCASE (SpaceKind::Constructor, SpaceKind::Disjunct):
        PAIRCASE (SpaceKind::BooleanConstant, SpaceKind::Disjunct):
        PAIRCASE (SpaceKind::UnknownCase, SpaceKind::Disjunct): {
          // S <= (S1 | ... | Sn) <= S iff (S <= S1) || ... || (S <= Sn)
          for (auto &param : other.getSpaces()) {
            if (this->isSubspace(param, TC, DC)) {
              return true;
            }
          }
          return false;
        }

        PAIRCASE (SpaceKind::BooleanConstant, SpaceKind::BooleanConstant):
          return this->getBoolValue() == other.getBoolValue();

        PAIRCASE (SpaceKind::BooleanConstant, SpaceKind::UnknownCase):
          return false;

        PAIRCASE (SpaceKind::Empty, SpaceKind::BooleanConstant):
        PAIRCASE (SpaceKind::Constructor, SpaceKind::BooleanConstant):
        PAIRCASE (SpaceKind::Type, SpaceKind::BooleanConstant):
        PAIRCASE (SpaceKind::UnknownCase, SpaceKind::BooleanConstant):
          return false;

        PAIRCASE (SpaceKind::UnknownCase, SpaceKind::Constructor):
          return false;

        PAIRCASE (SpaceKind::UnknownCase, SpaceKind::UnknownCase):
          if (other.isAllowedButNotRequired())
            return this->isAllowedButNotRequired();
          return true;

        default:
          llvm_unreachable("Uncovered pair found while computing subspaces?");
        }
      }

      // Returns the result of subtracting the other space from this space.  The
      // result is empty if the other space completely covers this space, or
      // non-empty if there were any uncovered cases.  The difference of spaces
      // is the smallest uncovered set of cases. The result is absent if the
      // computation had to be abandoned.
      //
      // \p minusCount is an optional pointer counting the number of
      // times minus has run.
      // Returns None if the computation "timed out".
      Optional<Space> minus(const Space &other, TypeChecker &TC,
                            const DeclContext *DC, unsigned *minusCount) const {

        if (minusCount && TC.getSwitchCheckingInvocationThreshold() &&
            (*minusCount)++ >= TC.getSwitchCheckingInvocationThreshold())
          return None;
        
        if (this->isEmpty()) {
          return Space();
        }

        if (other.isEmpty()) {
          return *this;
        }

        switch (PairSwitch(this->getKind(), other.getKind())) {
        PAIRCASE (SpaceKind::Type, SpaceKind::Type): {
          if (this->getType()->isEqual(other.getType())) {
            return Space();
          }
          return *this;
        }
        PAIRCASE (SpaceKind::Type, SpaceKind::Constructor): {
          if (canDecompose(this->getType(), DC)) {
            auto decomposition = decompose(TC, DC, this->getType());
            return decomposition.minus(other, TC, DC, minusCount);
          } else {
            return *this;
          }
        }
        PAIRCASE (SpaceKind::Type, SpaceKind::UnknownCase):
          // Note: This is not technically correct for decomposable types, but
          // you'd only get "typeSpace - unknownCaseSpace" if you haven't tried
          // to match any of the decompositions of the space yet. In that case,
          // we'd rather not expand the type, because it might be infinitely
          // decomposable.
          return *this;

        PAIRCASE (SpaceKind::Empty, SpaceKind::Disjunct):
        PAIRCASE (SpaceKind::Type, SpaceKind::Disjunct):
        PAIRCASE (SpaceKind::Constructor, SpaceKind::Disjunct):
        PAIRCASE (SpaceKind::Disjunct, SpaceKind::Disjunct):
        PAIRCASE (SpaceKind::BooleanConstant, SpaceKind::Disjunct):
        PAIRCASE (SpaceKind::UnknownCase, SpaceKind::Disjunct): {
          Space tot = *this;
          for (auto s : other.getSpaces()) {
            if (auto diff = tot.minus(s, TC, DC, minusCount))
              tot = *diff;
            else
              return None;
          }
          return tot;
        }

        PAIRCASE (SpaceKind::Disjunct, SpaceKind::Empty):
        PAIRCASE (SpaceKind::Disjunct, SpaceKind::Type):
        PAIRCASE (SpaceKind::Disjunct, SpaceKind::Constructor):
        PAIRCASE (SpaceKind::Disjunct, SpaceKind::BooleanConstant):
        PAIRCASE (SpaceKind::Disjunct, SpaceKind::UnknownCase): {
          SmallVector<Space, 4> smallSpaces;
          for (auto s : this->getSpaces()) {
            if (auto diff = s.minus(other, TC, DC, minusCount))
              smallSpaces.push_back(*diff);
            else
              return None;
          }
          return Space::forDisjunct(smallSpaces);
        }
        PAIRCASE (SpaceKind::Constructor, SpaceKind::Type):
          return Space();
        PAIRCASE (SpaceKind::Constructor, SpaceKind::UnknownCase): {
          SmallVector<Space, 4> newSubSpaces;
          for (auto subSpace : this->getSpaces()) {
            auto nextSpace = subSpace.minus(other, TC, DC, minusCount);
            if (!nextSpace)
              return None;
            if (nextSpace.getValue().isEmpty())
              return Space();
            newSubSpaces.push_back(nextSpace.getValue());
          }
          return Space::forConstructor(this->getType(), this->getHead(),
                                       this->canDowngradeToWarning(),
                                       newSubSpaces);
        }

        PAIRCASE (SpaceKind::Constructor, SpaceKind::Constructor): {
          // Optimization: If the heads of the constructors don't match then
          // the two are disjoint and their difference is the first space.
          if (this->Head != other.Head) {
            return *this;
          }

          // Special Case: Short circuit patterns without payloads.  Their
          // difference is empty.
          if (other.getSpaces().empty()) {
            return Space();
          }
          
          SmallVector<Space, 4> constrSpaces;
          bool foundBad = false;
          auto i = this->getSpaces().begin();
          auto j = other.getSpaces().begin();
          for (auto idx = 0;
               i != this->getSpaces().end() && j != other.getSpaces().end();
               ++i, ++j, ++idx) {
            auto &s1 = *i;
            auto &s2 = *j;

            // If one constructor parameter doesn't cover the other then we've
            // got to report the uncovered cases in a user-friendly way.
            if (!s1.isSubspace(s2, TC, DC)) {
              foundBad = true;
            }
            // Copy the params and replace the parameter at each index with the
            // difference of the two spaces.  This unpacks one constructor head
            // into each parameter.
            SmallVector<Space, 4> copyParams(this->getSpaces().begin(),
                                             this->getSpaces().end());

            auto reducedSpaceOrNone = s1.minus(s2, TC, DC, minusCount);
            if (!reducedSpaceOrNone)
              return None;
            auto reducedSpace = *reducedSpaceOrNone;
            
            // If one of the constructor parameters is empty it means
            // the whole constructor space is empty as well, so we can
            // safely skip it.
            if (reducedSpace.isEmpty())
              continue;

            // If reduced produced the same space as original one, we
            // should return it directly instead of trying to create
            // a disjunction of its sub-spaces because nothing got reduced.
            // This is especially helpful when dealing with `unknown` case
            // in parameter positions.
            if (s1 == reducedSpace)
              return *this;

            copyParams[idx] = reducedSpace;
            Space CS = Space::forConstructor(this->getType(), this->getHead(),
                                             this->canDowngradeToWarning(),
                                             copyParams);
            constrSpaces.push_back(CS);
          }

          if (foundBad) {
            return Space::forDisjunct(constrSpaces);
          }
          return Space();
        }
        PAIRCASE (SpaceKind::BooleanConstant, SpaceKind::BooleanConstant): {
          // The difference of boolean constants depends on their values.
          if (this->getBoolValue() == other.getBoolValue()) {
            return Space();
          } else {
            return *this;
          }
        }
        PAIRCASE (SpaceKind::BooleanConstant, SpaceKind::Type): {
          if (other.getType()->isBool()) {
            return (getKind() == SpaceKind::BooleanConstant)  ? Space() : *this;
          }

          if (canDecompose(other.getType(), DC)) {
            auto decomposition = decompose(TC, DC, other.getType());
            return this->minus(decomposition, TC, DC, minusCount);
          }
          return *this;
        }
        PAIRCASE (SpaceKind::BooleanConstant, SpaceKind::Empty):
        PAIRCASE (SpaceKind::BooleanConstant, SpaceKind::Constructor):
        PAIRCASE (SpaceKind::BooleanConstant, SpaceKind::UnknownCase):
          return *this;

        PAIRCASE (SpaceKind::Type, SpaceKind::BooleanConstant): {
          if (canDecompose(this->getType(), DC)) {
            auto orSpace = decompose(TC, DC, this->getType());
            return orSpace.minus(other, TC, DC, minusCount);
          } else {
            return *this;
          }
        }

        PAIRCASE (SpaceKind::UnknownCase, SpaceKind::Type):
          return Space();
        PAIRCASE (SpaceKind::UnknownCase, SpaceKind::Constructor):
          return *this;
        PAIRCASE (SpaceKind::UnknownCase, SpaceKind::UnknownCase):
          if (other.isAllowedButNotRequired() &&
              !this->isAllowedButNotRequired()) {
            return *this;
          }
          return Space();

        PAIRCASE (SpaceKind::Empty, SpaceKind::BooleanConstant):
        PAIRCASE (SpaceKind::Constructor, SpaceKind::BooleanConstant):
        PAIRCASE (SpaceKind::UnknownCase, SpaceKind::BooleanConstant):
          return *this;
        default:
          llvm_unreachable("Uncovered pair found while computing difference?");
        }
      }

      void show(llvm::raw_ostream &buffer, bool forDisplay = true) const {
        switch (getKind()) {
        case SpaceKind::Empty:
          if (forDisplay) {
            buffer << "_";
          } else {
            buffer << "[EMPTY]";
          }
          break;
        case SpaceKind::Disjunct: {
          if (forDisplay) {
            llvm_unreachable("Attempted to display disjunct to user!");
          } else {
            buffer << "DISJOIN(";
            for (auto &sp : Spaces) {
              buffer << "\n";
              sp.show(buffer, forDisplay);
              buffer << " |";
            }
            buffer << ")";
          }
        }
          break;
        case SpaceKind::BooleanConstant:
          buffer << (getBoolValue() ? "true" : "false");
          break;
        case SpaceKind::Constructor: {
          if (!Head.empty()) {
            buffer << ".";
            buffer << Head.str();
          }

          if (Spaces.empty()) {
            return;
          }

          buffer << "(";
          bool first = true;
          for (auto &param : Spaces) {
            if (!first) {
              buffer << ", ";
            }
            param.show(buffer, forDisplay);
            if (first) {
              first = false;
            }
          }
          buffer << ")";
        }
          break;
        case SpaceKind::Type: {
          Identifier Name = getPrintingName();
          if (Name.empty())
            buffer << "_";
          else
            buffer << tok::kw_let << " " << Name.str();
          if (!forDisplay) {
            buffer << ": ";
            getType()->print(buffer);
          }
        }
          break;
        case SpaceKind::UnknownCase:
          if (forDisplay) {
            // We special-case this to use "@unknown default" at the top level.
            buffer << "_";
          } else {
            buffer << "UNKNOWN";
            if (isAllowedButNotRequired())
              buffer << "(not_required)";
          }
          break;
        }
      }

      static bool isSwift3DowngradeExhaustivityCase(TypeChecker &TC,
                                                    const EnumElementDecl *eed){
        if (TC.getLangOpts().isSwiftVersionAtLeast(4))
          return false;
        return eed->getAttrs().hasAttribute<DowngradeExhaustivityCheckAttr>();
      }

      // Decompose a type into its component spaces.
      static void decompose(TypeChecker &TC, const DeclContext *DC, Type tp,
                            SmallVectorImpl<Space> &arr) {
        assert(canDecompose(tp, DC) && "Non-decomposable type?");

        if (tp->isBool()) {
          arr.push_back(Space::forBool(true));
          arr.push_back(Space::forBool(false));
        } else if (auto *E = tp->getEnumOrBoundGenericEnum()) {
          // Look into each case of the enum and decompose it in turn.
          auto children = E->getAllElements();
          std::transform(children.begin(), children.end(),
                         std::back_inserter(arr), [&](EnumElementDecl *eed) {
            SmallVector<Space, 4> constElemSpaces;

            // We need the interface type of this enum case but it may
            // not have been computed.
            if (!eed->hasInterfaceType()) {
              TC.validateDecl(eed);
            }

            // If there's still no interface type after validation then there's
            // not much else we can do here.
            if (!eed->hasInterfaceType()) {
              return Space();
            }

            // Don't force people to match unavailable cases; they can't even
            // write them.
            if (AvailableAttr::isUnavailable(eed)) {
              return Space();
            }

            auto eedTy = tp->getCanonicalType()
                           ->getTypeOfMember(E->getModuleContext(), eed,
                                             eed->getArgumentInterfaceType());
            if (eedTy) {
              if (auto *TTy = eedTy->getAs<TupleType>()) {
                // Decompose the payload tuple into its component type spaces.
                llvm::transform(TTy->getElements(),
                                std::back_inserter(constElemSpaces),
                                [&](TupleTypeElt elt) {
                  return Space::forType(elt.getType(), elt.getName());
                });
              } else if (auto *TTy = dyn_cast<ParenType>(eedTy.getPointer())) {
                constElemSpaces.push_back(
                    Space::forType(TTy->getUnderlyingType(), Identifier()));
              }
            }
            bool canDowngrade = isSwift3DowngradeExhaustivityCase(TC, eed);
            return Space::forConstructor(tp, eed->getName(), canDowngrade,
                                         constElemSpaces);
          });

          if (!E->isFormallyExhaustive(DC)) {
            arr.push_back(Space::forUnknown(/*allowedButNotRequired*/false));
          } else if (!E->getAttrs().hasAttribute<FrozenAttr>()) {
            arr.push_back(Space::forUnknown(/*allowedButNotRequired*/true));
          }

        } else if (auto *TTy = tp->castTo<TupleType>()) {
          // Decompose each of the elements into its component type space.
          SmallVector<Space, 4> constElemSpaces;
          llvm::transform(TTy->getElements(),
                          std::back_inserter(constElemSpaces),
                          [&](TupleTypeElt elt) {
            return Space::forType(elt.getType(), elt.getName());
          });
          // Create an empty constructor head for the tuple space.
          arr.push_back(Space::forConstructor(tp, Identifier(),
                                              /*canDowngrade*/false,
                                              constElemSpaces));
        } else {
          llvm_unreachable("Can't decompose type?");
        }
      }

      static Space decompose(TypeChecker &TC, const DeclContext *DC,
                             Type type) {
        SmallVector<Space, 4> spaces;
        decompose(TC, DC, type, spaces);
        return Space::forDisjunct(spaces);
      }

      static bool canDecompose(Type tp, const DeclContext *DC) {
        return tp->is<TupleType>() || tp->isBool() ||
               tp->getEnumOrBoundGenericEnum();
      }

      // HACK: Search the space for any remaining cases that were labelled
      // @_downgrade_exhaustivity_check, or 'exhaustive' enums in Swift 4 mode.
      DowngradeToWarning checkDowngradeToWarning() const {
        switch (getKind()) {
        case SpaceKind::Type:
        case SpaceKind::BooleanConstant:
        case SpaceKind::Empty:
          return DowngradeToWarning::No;
        case SpaceKind::UnknownCase:
          return DowngradeToWarning::ForUnknownCase;
        case SpaceKind::Constructor: {
          auto result = DowngradeToWarning::No;
          if (canDowngradeToWarning())
            result = DowngradeToWarning::ForSwift3Case;
          // Traverse the constructor and its subspaces.
          for (const Space &space : getSpaces())
            result = std::max(result, space.checkDowngradeToWarning());
          return result;
        }
        case SpaceKind::Disjunct: {
          if (getSpaces().empty())
            return DowngradeToWarning::No;
          // Traverse the disjunct's subspaces.
          auto result = DowngradeToWarning::LAST;
          for (const Space &space : getSpaces())
            result = std::min(result, space.checkDowngradeToWarning());
          return result;
        }
        }
      }
    };

    TypeChecker &TC;
    const SwitchStmt *Switch;
    const DeclContext *DC;
    llvm::DenseMap<APInt, Expr *, ::DenseMapAPIntKeyInfo> IntLiteralCache;
    llvm::DenseMap<APFloat, Expr *, ::DenseMapAPFloatKeyInfo> FloatLiteralCache;
    llvm::DenseMap<StringRef, Expr *> StringLiteralCache;
    
    SpaceEngine(TypeChecker &C, const SwitchStmt *SS, const DeclContext *DC)
        : TC(C), Switch(SS), DC(DC) {}

    bool checkRedundantLiteral(const Pattern *Pat, Expr *&PrevPattern) {
      if (Pat->getKind() != PatternKind::Expr) {
          return false;
      }
      auto *ExprPat = cast<ExprPattern>(Pat);
      auto *MatchExpr = ExprPat->getSubExpr();
      if (!MatchExpr || !isa<LiteralExpr>(MatchExpr)) {
          return false;
      }
      auto *EL = cast<LiteralExpr>(MatchExpr);
      switch (EL->getKind()) {
      case ExprKind::StringLiteral: {
        auto *SLE = cast<StringLiteralExpr>(EL);
        auto cacheVal =
            StringLiteralCache.insert({SLE->getValue(), SLE});
        PrevPattern = (cacheVal.first != StringLiteralCache.end())
                    ? cacheVal.first->getSecond()
                    : nullptr;
        return !cacheVal.second;
      }
      case ExprKind::IntegerLiteral: {
        // FIXME: The magic number 128 is bad and we should actually figure out
        // the bitwidth.  But it's too early in Sema to get it.
        auto *ILE = cast<IntegerLiteralExpr>(EL);
        auto cacheVal =
            IntLiteralCache.insert(
                {ILE->getValue(ILE->getDigitsText(), 128, ILE->isNegative()), ILE});
        PrevPattern = (cacheVal.first != IntLiteralCache.end())
                    ? cacheVal.first->getSecond()
                    : nullptr;
        return !cacheVal.second;
      }
      case ExprKind::FloatLiteral: {
        // FIXME: Pessimistically using IEEEquad here is bad and we should
        // actually figure out the bitwidth.  But it's too early in Sema.
        auto *FLE = cast<FloatLiteralExpr>(EL);
        auto cacheVal =
            FloatLiteralCache.insert(
                   {FLE->getValue(FLE->getDigitsText(),
                                  APFloat::IEEEquad(), FLE->isNegative()), FLE});
        PrevPattern = (cacheVal.first != FloatLiteralCache.end())
                    ? cacheVal.first->getSecond()
                    : nullptr;
        return !cacheVal.second;
      }
      default:
        return false;
      }
    }

    /// Estimate how big is the search space that exhaustivity
    /// checker needs to cover, based on the total space and information
    /// from the `switch` statement itself. Some of the easy situations
    /// like `case .foo(let bar)` don't really contribute to the complexity
    /// of the search so their sub-space sizes could be excluded from
    /// consideration.
    ///
    /// \param total The total space to check.
    /// \param covered The space covered by the `case` statements in the switch.
    ///
    /// \returns The size of the search space exhastivity checker has to check.
    size_t estimateSearchSpaceSize(const Space &total, const Space &covered) {
      switch (PairSwitch(total.getKind(), covered.getKind())) {
      PAIRCASE(SpaceKind::Type, SpaceKind::Type): {
        return total.getType()->isEqual(covered.getType())
                    ? 0
                    : total.getSize(TC, DC);
      }
      PAIRCASE(SpaceKind::Type, SpaceKind::Disjunct):
      PAIRCASE(SpaceKind::Type, SpaceKind::Constructor): {
        if (!Space::canDecompose(total.getType(), DC))
          break;

        auto decomposition = Space::decompose(TC, DC, total.getType());
        return estimateSearchSpaceSize(decomposition, covered);
      }

      PAIRCASE(SpaceKind::Disjunct, SpaceKind::Disjunct):
      PAIRCASE(SpaceKind::Disjunct, SpaceKind::Constructor): {
        auto &spaces = total.getSpaces();
        return std::accumulate(spaces.begin(), spaces.end(), 0,
                               [&](size_t totalSize, const Space &space) {
                                 return totalSize + estimateSearchSpaceSize(
                                                        space, covered);
                               });
      }

      // Search space size computation is not commutative, because it
      // tries to check if space on right-hand side is covering any
      // portion of the "total" space on the left.
      PAIRCASE(SpaceKind::Constructor, SpaceKind::Disjunct): {
        for (const auto &space : covered.getSpaces()) {
          // enum E { case foo }
          // func bar(_ lhs: E, _ rhs: E) {
          //   switch (lhs, rhs) {
          //     case (_, _): break
          // }
          if (total == space)
            return 0;

          if (!space.isSubspace(total, TC, DC))
            continue;

          if (estimateSearchSpaceSize(total, space) == 0)
            return 0;
        }
        break;
      }

      PAIRCASE(SpaceKind::Constructor, SpaceKind::Constructor): {
        if (total.getHead() != covered.getHead())
          break;

        auto &lhs = total.getSpaces();
        auto &rhs = covered.getSpaces();

        if (std::distance(lhs.begin(), lhs.end()) !=
            std::distance(rhs.begin(), rhs.end()))
          return total.getSize(TC, DC);

        auto i = lhs.begin();
        auto j = rhs.begin();

        size_t totalSize = 0;
        for (; i != lhs.end() && j != rhs.end(); ++i, ++j) {
          // The only light-weight checking we can do
          // is when sub-spaces on both sides are types
          // otherwise we'd have to decompose, which
          // is too heavy, so let's just return total
          // space size if such situation is detected.
          if (i->getKind() != SpaceKind::Type ||
              j->getKind() != SpaceKind::Type)
            return total.getSize(TC, DC);

          totalSize += estimateSearchSpaceSize(*i, *j);
        }

        return totalSize;
      }

      default:
        break;
      }

      return total.getSize(TC, DC);
    }

    void checkExhaustiveness(bool limitedChecking) {
      // If the type of the scrutinee is uninhabited, we're already dead.
      // Allow any well-typed patterns through.
      auto subjectType = Switch->getSubjectExpr()->getType();
      if (subjectType && subjectType->isStructurallyUninhabited()) {
        return;
      }

      // If the switch body fails to typecheck, end analysis here.
      if (limitedChecking) {
        // Reject switch statements with empty blocks.
        if (Switch->getCases().empty())
          diagnoseMissingCases(RequiresDefault::EmptySwitchBody, Space());
        return;
      }

      bool sawDowngradablePattern = false;
      bool sawRedundantPattern = false;
      const CaseStmt *unknownCase = nullptr;
      SmallVector<Space, 4> spaces;
      for (auto *caseBlock : Switch->getCases()) {
        if (caseBlock->hasUnknownAttr()) {
          assert(unknownCase == nullptr && "multiple unknown cases");
          unknownCase = caseBlock;
          continue;
        }

        for (auto &caseItem : caseBlock->getCaseLabelItems()) {
          // 'where'-clauses on cases mean the case does not contribute to
          // the exhaustiveness of the pattern.
          if (caseItem.getGuardExpr())
            continue;

          // Space is trivially covered with a default clause.
          if (caseItem.isDefault())
            return;

          Space projection = projectPattern(TC, caseItem.getPattern(),
                                            sawDowngradablePattern);

          if (!projection.isEmpty() &&
              projection.isSubspace(Space::forDisjunct(spaces), TC, DC)) {
            sawRedundantPattern |= true;

            TC.diagnose(caseItem.getStartLoc(),
                          diag::redundant_particular_case)
              .highlight(caseItem.getSourceRange());
            continue;
          } else {
            Expr *cachedExpr = nullptr;
            if (checkRedundantLiteral(caseItem.getPattern(), cachedExpr)) {
              assert(cachedExpr && "Cache found hit but no expr?");
              TC.diagnose(caseItem.getStartLoc(),
                          diag::redundant_particular_literal_case)
                .highlight(caseItem.getSourceRange());
              TC.diagnose(cachedExpr->getLoc(),
                          diag::redundant_particular_literal_case_here)
                .highlight(cachedExpr->getSourceRange());
              continue;
            }
          }
          spaces.push_back(projection);
        }
      }

      Space totalSpace = Space::forType(subjectType, Identifier());
      Space coveredSpace = Space::forDisjunct(spaces);

      const size_t searchSpaceSizeEstimate =
          estimateSearchSpaceSize(totalSpace, coveredSpace);
      if (searchSpaceSizeEstimate > Space::getMaximumSize()) {
        diagnoseCannotCheck(sawRedundantPattern, totalSpace, coveredSpace,
                            unknownCase);
        return;
      }
      unsigned minusCount = 0;
      auto diff = totalSpace.minus(coveredSpace, TC, DC, &minusCount);
      if (!diff) {
        diagnoseCannotCheck(sawRedundantPattern, totalSpace, coveredSpace,
                            unknownCase);
        return;
      }
      
      auto uncovered = diff.getValue();
      if (unknownCase && uncovered.isEmpty()) {
        TC.diagnose(unknownCase->getLoc(), diag::redundant_particular_case)
          .highlight(unknownCase->getSourceRange());
      }

      // Account for unknown cases. If the developer wrote `unknown`, they're
      // all handled; otherwise, we ignore the ones that were added for enums
      // that are implicitly frozen.
      uncovered = *uncovered.minus(Space::forUnknown(unknownCase == nullptr),
                                   TC, DC, /*&minusCount*/ nullptr);

      if (uncovered.isEmpty())
        return;

      // If the entire space is left uncovered we have two choices: We can
      // decompose the type space and offer them as fixits, or simply offer
      // to insert a `default` clause.
      if (uncovered.getKind() == SpaceKind::Type) {
        if (Space::canDecompose(uncovered.getType(), DC)) {
          SmallVector<Space, 4> spaces;
          Space::decompose(TC, DC, uncovered.getType(), spaces);
          diagnoseMissingCases(RequiresDefault::No, Space::forDisjunct(spaces),
                               unknownCase, /*sawDowngradablePattern*/false);
        } else {
          diagnoseMissingCases(Switch->getCases().empty()
                                 ? RequiresDefault::EmptySwitchBody
                                 : RequiresDefault::UncoveredSwitch,
                               uncovered, unknownCase,
                               /*sawDowngradablePattern*/false);
        }
        return;
      }

      diagnoseMissingCases(RequiresDefault::No, uncovered, unknownCase,
                           sawDowngradablePattern);
    }
    
    enum class RequiresDefault {
      No,
      EmptySwitchBody,
      UncoveredSwitch,
      SpaceTooLarge,
    };
    
    void diagnoseCannotCheck(const bool sawRedundantPattern,
                             const Space &totalSpace,
                             const Space &coveredSpace,
                             const CaseStmt *unknownCase) {
      // Because the space is large or the check is too slow,
      // we have to extend the size
      // heuristic to compensate for actually exhaustively pattern matching
      // over enormous spaces.  In this case, if the covered space covers
      // as much as the total space, and there were no duplicates, then we
      // can assume the user did the right thing and that they don't need
      // a 'default' to be inserted.
      // FIXME: Do something sensible for non-frozen enums.
      if (!sawRedundantPattern &&
          coveredSpace.getSize(TC, DC) >= totalSpace.getSize(TC, DC))
        return;
      diagnoseMissingCases(RequiresDefault::SpaceTooLarge, Space(),
                           unknownCase);
    }
    
    void diagnoseMissingCases(RequiresDefault defaultReason, Space uncovered,
                              const CaseStmt *unknownCase = nullptr,
                              bool sawDowngradablePattern = false) {
      SourceLoc startLoc = Switch->getStartLoc();
      SourceLoc insertLoc;
      if (unknownCase)
        insertLoc = unknownCase->getStartLoc();
      else
        insertLoc = Switch->getEndLoc();
      StringRef placeholder = getCodePlaceholder();
      llvm::SmallString<128> buffer;
      llvm::raw_svector_ostream OS(buffer);

      bool InEditor = TC.Context.LangOpts.DiagnosticsEditorMode;

      // Decide whether we want an error or a warning.
      auto mainDiagType = diag::non_exhaustive_switch;
      if (unknownCase) {
        switch (defaultReason) {
        case RequiresDefault::EmptySwitchBody:
          llvm_unreachable("there's an @unknown case; the body can't be empty");
        case RequiresDefault::No:
          if (!uncovered.isEmpty())
            mainDiagType = diag::non_exhaustive_switch_warn;
          break;
        case RequiresDefault::UncoveredSwitch:
        case RequiresDefault::SpaceTooLarge: {
          auto diagnostic = defaultReason == RequiresDefault::UncoveredSwitch
                                ? diag::non_exhaustive_switch
                                : diag::possibly_non_exhaustive_switch;
          TC.diagnose(startLoc, diagnostic);
          TC.diagnose(unknownCase->getLoc(),
                      diag::non_exhaustive_switch_drop_unknown)
            .fixItRemoveChars(unknownCase->getStartLoc(),
                              unknownCase->getLoc());
          return;
        }
        }
      }

      switch (uncovered.checkDowngradeToWarning()) {
      case DowngradeToWarning::No:
        break;
      case DowngradeToWarning::ForSwift3Case:
        // If someone's used one of the cases introduced in the Swift 4
        // timeframe, force them to handle all of them.
        if (!sawDowngradablePattern)
          mainDiagType = diag::non_exhaustive_switch_warn;
        break;
      case DowngradeToWarning::ForUnknownCase:
        if (TC.Context.LangOpts.DebuggerSupport ||
            TC.Context.LangOpts.Playground ||
            !TC.getLangOpts().EnableNonFrozenEnumExhaustivityDiagnostics) {
          // Don't require covering unknown cases in the debugger or in
          // playgrounds.
          return;
        }
        // Missing '@unknown' is just a warning.
        mainDiagType = diag::non_exhaustive_switch_warn;
        break;
      }

      switch (defaultReason) {
      case RequiresDefault::No:
        break;
      case RequiresDefault::EmptySwitchBody: {
        OS << tok::kw_default << ":\n" << placeholder << "\n";
        TC.diagnose(startLoc, diag::empty_switch_stmt)
          .fixItInsert(insertLoc, buffer.str());
      }
        return;
      case RequiresDefault::UncoveredSwitch: {
        OS << tok::kw_default << ":\n" << placeholder << "\n";
        TC.diagnose(startLoc, mainDiagType);
        TC.diagnose(startLoc, diag::missing_several_cases, /*default*/true)
          .fixItInsert(insertLoc, buffer.str());
      }
        return;
      case RequiresDefault::SpaceTooLarge: {
        OS << tok::kw_default << ":\n" << "<#fatalError()#>" << "\n";
        TC.diagnose(startLoc, diag::possibly_non_exhaustive_switch);
        TC.diagnose(startLoc, diag::missing_several_cases, /*default*/true)
          .fixItInsert(insertLoc, buffer.str());
      }
        return;
      }

      // If there's nothing else to diagnose, bail.
      if (uncovered.isEmpty()) return;

      TC.diagnose(startLoc, mainDiagType);

      // Add notes to explain what's missing.
      auto processUncoveredSpaces =
          [&](llvm::function_ref<void(const Space &space,
                                      bool onlyOneUncoveredSpace)> process) {

        // Flatten away all disjunctions.
        SmallVector<Space, 4> flats;
        flatten(uncovered, flats);

        // Then figure out which of the remaining spaces are interesting.
        // To do this, we sort by size, largest spaces first...
        SmallVector<const Space *, 4> flatsSortedBySize;
        flatsSortedBySize.reserve(flats.size());
        for (const Space &space : flats)
          flatsSortedBySize.push_back(&space);
        std::stable_sort(flatsSortedBySize.begin(), flatsSortedBySize.end(),
                         [&](const Space *left, const Space *right) {
          return left->getSize(TC, DC) > right->getSize(TC, DC);
        });

        // ...and then remove any of the later spaces that are contained
        // entirely in an earlier one.
        SmallPtrSet<const Space *, 4> flatsToEmit;
        for (const Space *space : flatsSortedBySize) {
          bool alreadyHandled =
              llvm::any_of(flatsToEmit, [&](const Space *previousSpace) {
            return space->isSubspace(*previousSpace, TC, DC);
          });
          if (alreadyHandled)
            continue;
          flatsToEmit.insert(space);
        }

        // Finally we can iterate over the flat spaces in their original order,
        // but only emit the interesting ones.
        for (const Space &flat : flats) {
          if (!flatsToEmit.count(&flat))
            continue;

          if (flat.getKind() == SpaceKind::UnknownCase) {
            assert(&flat == &flats.back() && "unknown case must be last");
            if (unknownCase) {
              // This can occur if the /only/ case in the switch is 'unknown'.
              // In that case we won't do any analysis on the input space, but
              // will later decompose the space into cases.
              continue;
            }
            if (!TC.getLangOpts().EnableNonFrozenEnumExhaustivityDiagnostics)
              continue;
          }

          process(flat, flats.size() == 1);
        }
      };

      // If editing is enabled, emit a formatted error of the form:
      //
      // switch must be exhaustive, do you want to add missing cases?
      //     case (.none, .some(_)):
      //       <#code#>
      //     case (.some(_), .none):
      //       <#code#>
      //
      // else:
      //
      // switch must be exhaustive, consider adding missing cases:
      //
      // missing case '(.none, .some(_))'
      // missing case '(.some(_), .none)'
      if (InEditor) {
        buffer.clear();

        bool alreadyEmittedSomething = false;
        processUncoveredSpaces([&](const Space &space,
                                   bool onlyOneUncoveredSpace) {
          if (space.getKind() == SpaceKind::UnknownCase) {
            OS << "@unknown " << tok::kw_default;
            if (onlyOneUncoveredSpace) {
              OS << ":\n<#fatalError()#>\n";
              TC.diagnose(startLoc, diag::missing_unknown_case)
                .fixItInsert(insertLoc, buffer.str());
              alreadyEmittedSomething = true;
              return;
            }
          } else {
            OS << tok::kw_case << " ";
            space.show(OS);
          }
          OS << ":\n" << placeholder << "\n";
        });

        if (!alreadyEmittedSomething) {
          TC.diagnose(startLoc, diag::missing_several_cases, false)
            .fixItInsert(insertLoc, buffer.str());
        }

      } else {
        processUncoveredSpaces([&](const Space &space,
                                   bool onlyOneUncoveredSpace) {
          if (space.getKind() == SpaceKind::UnknownCase) {
            auto note = TC.diagnose(startLoc, diag::missing_unknown_case);
            if (onlyOneUncoveredSpace)
              note.fixItInsert(insertLoc, "@unknown default:\n<#fatalError#>()\n");
            return;
          }

          buffer.clear();
          space.show(OS);
          TC.diagnose(startLoc, diag::missing_particular_case, buffer.str());
        });
      }
    }

  private:
    // Recursively unpacks a space of disjunctions or constructor parameters
    // into its component parts such that the resulting array of flattened
    // spaces contains no further disjunctions.  The resulting flattened array
    // will never be empty.
    static void flatten(const Space space, SmallVectorImpl<Space> &flats) {
      switch (space.getKind()) {
      case SpaceKind::Constructor: {
        // Optimization: If this space is just a constructor head, it is already
        // flat.
        if (space.getSpaces().empty()) {
          flats.push_back(space);
          return;
        }
        
        // To recursively recover a pattern matrix from a bunch of disjuncts:
        // 1) Unpack the arguments to the constructor under scrutiny.
        // 2) Traverse each argument in turn.
        // 3) Flatten the argument space into a column vector.
        // 4) Extend the existing pattern matrix by a factor of the size of
        //    the column vector and copy each previous component.
        // 5) Extend the expanded matrix with multiples of the column vector's
        //    components until filled.
        // 6) Wrap each matrix row in the constructor under scrutiny.
        size_t multiplier = 1;
        SmallVector<SmallVector<Space, 4>, 2> matrix;
        for (auto &subspace : space.getSpaces()) {
          SmallVector<Space, 4> columnVect;
          flatten(subspace, columnVect);

          size_t startSize = matrix.size();
          if (!matrix.empty() && columnVect.size() > 1) {
            size_t oldCount = matrix.size();
            matrix.reserve(oldCount * columnVect.size());
            // Indexing starts at 1, we already have 'startSize'-many elements
            // in the matrix; multiplies by 1 are no-ops.
            for (size_t i = 1; i < columnVect.size(); ++i) {
              std::copy_n(matrix.begin(), oldCount, std::back_inserter(matrix));
            }
          }

          if (matrix.empty()) {
            // Get the empty matrix setup with its starting row vectors.
            for (auto &vspace : columnVect) {
              matrix.push_back({});
              matrix.back().push_back(vspace);
            }
          } else {
            // Given a matrix of 'n' rows and '(m-1)*k' columns, to make a
            // matrix of size 'n' by 'm*k' we need to copy each element of the
            // column vector into a row 'm' times - as many times as there were
            // elements of the original matrix before multiplication.
            size_t stride = multiplier;
            if (startSize == 1) {
              // Special case: If the column vector is bigger than the matrix
              // before multiplication, we need to index it linearly
              stride = 1;
            } else if (columnVect.size() == 1) {
              // Special case: If the column vector has size 1 then we needn't
              // stride at all.
              stride = matrix.size();
            }

            for (size_t rowIdx = 0, colIdx = 0; rowIdx < matrix.size(); ++rowIdx) {
              if (rowIdx != 0 && (rowIdx % stride) == 0) {
                colIdx++;
              }

              matrix[rowIdx].push_back(columnVect[colIdx]);
            }
          }
          
          // Pattern matrices grow quasi-factorially in the size of the
          // input space.
          multiplier *= columnVect.size();
        }

        // Wrap the matrix rows into this constructor.
        for (auto &row : matrix) {
          flats.push_back(Space::forConstructor(space.getType(),
                                                space.getHead(),
                                                space.canDowngradeToWarning(),
                                                row));
        }
      }
        break;
      case SpaceKind::Disjunct: {
        for (auto &subspace : space.getSpaces()) {
          SmallVector<Space, 4> buf;
          flatten(subspace, buf);
          flats.append(buf.begin(), buf.end());
        }
      }
        break;
      default:
        flats.push_back(space);
        break;
      }
    }

    /// Recursively project a pattern into a Space.
    ///
    /// The resulting Space does not mark any subspaces as downgradable.
    /// Instead, whether or not a Swift 3 downgradable pattern was seen is
    /// recorded in \p sawDowngradablePattern. (This does not include
    /// downgradable warnings for exhaustive enums in Swift 4.)
    static Space projectPattern(TypeChecker &TC, const Pattern *item,
                                bool &sawDowngradablePattern) {
      switch (item->getKind()) {
      case PatternKind::Any:
        return Space::forType(item->getType(), Identifier());
      case PatternKind::Named:
        return Space::forType(item->getType(),
                              cast<NamedPattern>(item)->getBoundName());
      case PatternKind::Bool:
        return Space::forBool(cast<BoolPattern>(item)->getValue());
      case PatternKind::Is: {
        auto *IP = cast<IsPattern>(item);
        switch (IP->getCastKind()) {
        case CheckedCastKind::Coercion:
        case CheckedCastKind::BridgingCoercion: {
          if (auto *subPattern = IP->getSubPattern()) {
            // Project the cast target's subpattern.
            Space castSubSpace = projectPattern(TC, subPattern,
                                                sawDowngradablePattern);
            // If we recieved a type space from a named pattern or a wildcard
            // we have to re-project with the cast's target type to maintain
            // consistency with the scrutinee's type.
            if (castSubSpace.getKind() == SpaceKind::Type) {

              return Space::forType(IP->getType(),
                                    castSubSpace.getPrintingName());
            }
            return castSubSpace;
          }

          // With no subpattern coercions are irrefutable.  Project with the
          // original type instead of the cast's target type to maintain
          // consistency with the scrutinee's type.
          return Space::forType(IP->getType(), Identifier());
        }
        case CheckedCastKind::Unresolved:
        case CheckedCastKind::ValueCast:
        case CheckedCastKind::ArrayDowncast:
        case CheckedCastKind::DictionaryDowncast:
        case CheckedCastKind::SetDowncast:
            return Space();
        }
      }
      case PatternKind::Typed:
      case PatternKind::Expr:
        return Space();
      case PatternKind::Var: {
        auto *VP = cast<VarPattern>(item);
        return projectPattern(TC, VP->getSubPattern(), sawDowngradablePattern);
      }
      case PatternKind::Paren: {
        auto *PP = cast<ParenPattern>(item);
        return projectPattern(TC, PP->getSubPattern(), sawDowngradablePattern);
      }
      case PatternKind::OptionalSome: {
        auto *OSP = cast<OptionalSomePattern>(item);
        Identifier name = TC.Context.getOptionalSomeDecl()->getName();

        auto subSpace = projectPattern(TC, OSP->getSubPattern(),
                                       sawDowngradablePattern);
        // To match patterns like (_, _, ...)?, we must rewrite the underlying
        // tuple pattern to .some(_, _, ...) first.
        if (subSpace.getKind() == SpaceKind::Constructor
            && subSpace.getHead().empty()) {
          return Space::forConstructor(item->getType(), name,
                                       /*canDowngrade*/false,
                                       std::move(subSpace.getSpaces()));
        }
        return Space::forConstructor(item->getType(), name,
                                     /*canDowngrade*/false, subSpace);
      }
      case PatternKind::EnumElement: {
        auto *VP = cast<EnumElementPattern>(item);
        TC.validateDecl(item->getType()->getEnumOrBoundGenericEnum());
        
        if (auto *eed = VP->getElementDecl()) {
          if (Space::isSwift3DowngradeExhaustivityCase(TC, eed)) {
            sawDowngradablePattern |= true;
          }
        }
        
        auto *SP = VP->getSubPattern();
        if (!SP) {
          // If there's no sub-pattern then there's no further recursive
          // structure here.  Yield the constructor space.
          return Space::forConstructor(item->getType(), VP->getName(),
                                       /*canDowngrade*/false, None);
        }

        SmallVector<Space, 4> conArgSpace;
        switch (SP->getKind()) {
        case PatternKind::Tuple: {
          auto *TP = dyn_cast<TuplePattern>(SP);
          std::transform(TP->getElements().begin(), TP->getElements().end(),
                         std::back_inserter(conArgSpace),
                         [&](TuplePatternElt pate) {
                           return projectPattern(TC, pate.getPattern(),
                                                 sawDowngradablePattern);
                         });
          return Space::forConstructor(item->getType(), VP->getName(),
                                       /*canDowngrade*/false, conArgSpace);
        }
        case PatternKind::Paren: {
          auto *PP = dyn_cast<ParenPattern>(SP);
          auto *SP = PP->getSemanticsProvidingPattern();

          // Special Case: A constructor pattern may have all of its payload
          // matched by a single var pattern.  Project it like the tuple it
          // really is.
          //
          // FIXME: SE-0155 makes this case unreachable.
          if (SP->getKind() == PatternKind::Named
              || SP->getKind() == PatternKind::Any) {
            if (auto *TTy = SP->getType()->getAs<TupleType>()) {
              for (auto ty : TTy->getElements()) {
                conArgSpace.push_back(Space::forType(ty.getType(),
                                                     ty.getName()));
              }
            } else {
              conArgSpace.push_back(projectPattern(TC, SP,
                                                   sawDowngradablePattern));
            }
          } else if (SP->getKind() == PatternKind::Tuple) {
            Space argTupleSpace = projectPattern(TC, SP,
                                                 sawDowngradablePattern);
            assert(argTupleSpace.getKind() == SpaceKind::Constructor);
            conArgSpace.insert(conArgSpace.end(),
                               argTupleSpace.getSpaces().begin(),
                               argTupleSpace.getSpaces().end());
          } else {
            conArgSpace.push_back(projectPattern(TC, SP,
                                                 sawDowngradablePattern));
          }
          return Space::forConstructor(item->getType(), VP->getName(),
                                       /*canDowngrade*/false, conArgSpace);
        }
        default:
          return projectPattern(TC, SP, sawDowngradablePattern);
        }
      }
      case PatternKind::Tuple: {
        auto *TP = cast<TuplePattern>(item);
        SmallVector<Space, 4> conArgSpace;
        std::transform(TP->getElements().begin(), TP->getElements().end(),
                       std::back_inserter(conArgSpace),
                       [&](TuplePatternElt pate) {
          return projectPattern(TC, pate.getPattern(), sawDowngradablePattern);
        });
        return Space::forConstructor(item->getType(), Identifier(),
                                     /*canDowngrade*/false, conArgSpace);
      }
      }
    }
  };
} // end anonymous namespace

void TypeChecker::checkSwitchExhaustiveness(const SwitchStmt *stmt,
                                            const DeclContext *DC,
                                            bool limited) {
  SpaceEngine(*this, stmt, DC).checkExhaustiveness(limited);
}

void SpaceEngine::Space::dump() const {
  this->show(llvm::errs(), /*normalize*/ false);
  llvm::errs() << '\n';
}
