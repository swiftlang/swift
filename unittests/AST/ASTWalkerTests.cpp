//===--- ASTWalkerTests.cpp - Tests for ASTWalker -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "TestContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/TypeRepr.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::unittest;

namespace {
namespace internal {

enum class WalkCheckpoint { Pre, Post };

struct WalkAction {
  class Payload {
    bool isRepr;
    union {
      llvm::StringRef name;
      TypeRepr *repr;
    };

  public:
    /*implicit*/ Payload(TypeRepr *repr) : isRepr(true), repr(repr) {
      assert(repr);
    }

    /*implicit*/ Payload(llvm::StringRef name) : isRepr(false), name(name) {
      assert(!name.empty());
    }

    llvm::StringRef getName() const { return isRepr ? StringRef() : name; }

    TypeRepr *getTypeRepr() const { return isRepr ? repr : nullptr; }

    bool matches(const TypeRepr *otherRepr) const {
      assert(otherRepr);

      if (isRepr) {
        if (getTypeRepr() == otherRepr) {
          return true;
        }
      } else if (auto *declRefTR = dyn_cast<DeclRefTypeRepr>(otherRepr)) {
        if (declRefTR->getNameRef().getBaseIdentifier().str() == getName()) {
          return true;
        }
      }

      return false;
    }
  };

  enum {
    SkipChildren,
    SkipChildrenAndPostWalk,
    StopPreWalk,
    StopPostWalk,
  };

private:
  using _Action = decltype(SkipChildren);

  _Action action;
  Payload payload;

  WalkAction(_Action action, Payload payload)
      : action(action), payload(payload) {}

public:
  static WalkAction SkipChildrenOf(Payload payload) {
    return {SkipChildren, payload};
  }

  static WalkAction SkipChildrenAndPostWalkOf(Payload payload) {
    return {SkipChildrenAndPostWalk, payload};
  }

  static WalkAction StopPreWalkAt(Payload payload) {
    return {StopPreWalk, payload};
  }

  static WalkAction StopPostWalkAt(Payload payload) {
    return {StopPostWalk, payload};
  }

  const Payload &getPayload() const { return payload; }

  operator _Action() const { return action; }
};

/// A walker that invokes a callback on every `walkToTypeReprPre` and
/// `walkToTypeReprPost` checkpoint.
class Walker : public ASTWalker {
  using Callback = std::function<void(TypeRepr *, WalkCheckpoint)>;

  QualifiedIdentTypeReprWalkingScheme scheme;
  std::optional<WalkAction> action;
  Callback callback;

public:
  Walker(QualifiedIdentTypeReprWalkingScheme scheme,
         std::optional<WalkAction> action, Callback callback)
      : scheme(scheme), action(action), callback(callback) {}

  QualifiedIdentTypeReprWalkingScheme
  getQualifiedIdentTypeReprWalkingScheme() const override {
    return scheme;
  }

  PreWalkAction walkToTypeReprPre(TypeRepr *repr) override {
    this->callback(repr, WalkCheckpoint::Pre);

    if (this->action && this->action.value().getPayload().matches(repr)) {
      switch (this->action.value()) {
      case WalkAction::StopPostWalk:
        return Action::Continue();
      case WalkAction::SkipChildren:
        return Action::SkipChildren();
      case WalkAction::SkipChildrenAndPostWalk:
        return Action::SkipNode();
      case WalkAction::StopPreWalk:
        return Action::Stop();
      }
    }

    return Action::Continue();
  }

  PostWalkAction walkToTypeReprPost(TypeRepr *repr) override {
    this->callback(repr, WalkCheckpoint::Post);

    if (this->action && this->action.value().getPayload().matches(repr)) {
      switch (this->action.value()) {
      case WalkAction::SkipChildren:
      case WalkAction::SkipChildrenAndPostWalk:
      case WalkAction::StopPreWalk:
        return Action::Continue();
      case WalkAction::StopPostWalk:
        return Action::Stop();
      }
    }

    return Action::Continue();
  }
};

} // end namespace internal

class WalkVerifier {
  /// Represents a substring of the format `( 'pr' | 'po' ) '(' x ')'`,
  /// where `x` is the argument. For example, `pr(T)`.
  class Component {
    internal::WalkCheckpoint checkpoint;
    size_t argumentStartIndex;
    size_t endIndex;

    Component(internal::WalkCheckpoint checkpoint, size_t argumentStartIndex,
              size_t endIndex)
        : checkpoint(checkpoint), argumentStartIndex(argumentStartIndex),
          endIndex(endIndex) {}

  public:
    internal::WalkCheckpoint getCheckpoint() const { return checkpoint; }
    size_t getArgumentStartIndex() const { return argumentStartIndex; }
    size_t getEndIndex() const { return endIndex; }
    size_t getArgumentEndIndex() const { return endIndex - 1; }

    static Component consume(llvm::raw_svector_ostream &os, TypeRepr *repr,
                             internal::WalkCheckpoint checkpoint) {
      size_t argumentStartIndex = os.str().size() + 3;
      print(os, repr, checkpoint);
      size_t endIndex = os.str().size();

      return Component(checkpoint, argumentStartIndex, endIndex);
    }

    static void print(llvm::raw_ostream &os, TypeRepr *repr,
                      internal::WalkCheckpoint checkpoint) {
      switch (checkpoint) {
      case internal::WalkCheckpoint::Pre:
        os << "pr";
        break;
      case internal::WalkCheckpoint::Post:
        os << "po";
        break;
      }

      os << "(";
      printArgument(os, repr);
      os << ")";
    }

    static std::string getArgumentString(TypeRepr *repr) {
      std::string result;
      llvm::raw_string_ostream os(result);
      printArgument(os, repr);
      return result;
    }

  private:
    static void printArgument(llvm::raw_ostream &os, TypeRepr *repr) {
      if (auto *declRefTR = dyn_cast<DeclRefTypeRepr>(repr)) {
        os << declRefTR->getNameRef().getBaseIdentifier();
      } else {
        repr->print(os);
      }
    }
  };

  TypeRepr *repr;
  QualifiedIdentTypeReprWalkingScheme scheme;

  /// A contiguous series of `Component` substrings.
  llvm::SmallString<128> walkStr;
  llvm::SmallVector<Component, 20> components;

public:
  WalkVerifier(TypeRepr *repr, QualifiedIdentTypeReprWalkingScheme scheme)
      : repr(repr), scheme(scheme) {
    llvm::raw_svector_ostream os(this->walkStr);
    internal::Walker walker(
        scheme, std::nullopt,
        [&](TypeRepr *repr, internal::WalkCheckpoint checkpoint) {
          this->components.push_back(Component::consume(os, repr, checkpoint));
        });

    repr->walk(walker);
  }

  llvm::StringRef getWalkString() const { return walkStr; }

  /// Verify that walking controls such as skipping children or aborting work
  /// correctly on the given child node.
  void testWalkActionsOn(TypeRepr *repr) const {
    for (auto &action : {
             internal::WalkAction::SkipChildrenOf(repr),
             internal::WalkAction::SkipChildrenAndPostWalkOf(repr),
             internal::WalkAction::StopPreWalkAt(repr),
             internal::WalkAction::StopPostWalkAt(repr),
         }) {
      testWalkAction(action);
    }
  }

  /// Verify that walking controls such as skipping children or aborting work
  /// correctly on a child `DeclRefTypeRepr` node with the given name.
  void testWalkActionsOn(llvm::StringRef name) const {
    for (auto &action : {
             internal::WalkAction::SkipChildrenOf(name),
             internal::WalkAction::SkipChildrenAndPostWalkOf(name),
             internal::WalkAction::StopPreWalkAt(name),
             internal::WalkAction::StopPostWalkAt(name),
         }) {
      testWalkAction(action);
    }
  }

private:
  void testWalkAction(internal::WalkAction action) const {
    llvm::SmallString<128> actualStr;
    {
      llvm::raw_svector_ostream os(actualStr);
      internal::Walker walker(
          scheme, action,
          [&](TypeRepr *repr, internal::WalkCheckpoint checkpoint) {
            Component::print(os, repr, checkpoint);
          });

      this->repr->walk(walker);
    }

    std::string argumentStr;
    if (auto *payloadRepr = action.getPayload().getTypeRepr()) {
      argumentStr = Component::getArgumentString(payloadRepr);
    } else {
      argumentStr = action.getPayload().getName().str();
    }

    switch (action) {
    case internal::WalkAction::SkipChildren: {
      auto expectedStr = getStringForSkipChildrenAction(
          argumentStr, /*includePostComponent=*/true);
      EXPECT_EQ(actualStr.str(), StringRef(expectedStr));
      break;
    }
    case internal::WalkAction::SkipChildrenAndPostWalk: {
      auto expectedStr = getStringForSkipChildrenAction(
          argumentStr, /*includePostComponent=*/false);
      EXPECT_EQ(actualStr.str(), StringRef(expectedStr));
      break;
    }
    case internal::WalkAction::StopPreWalk: {
      auto expectedStr =
          getStringForStopAction(internal::WalkCheckpoint::Pre, argumentStr);
      EXPECT_EQ(actualStr.str(), expectedStr);
      break;
    }
    case internal::WalkAction::StopPostWalk: {
      auto expectedStr =
          getStringForStopAction(internal::WalkCheckpoint::Post, argumentStr);
      EXPECT_EQ(actualStr.str(), expectedStr);
      break;
    }
    }
  }

  llvm::StringRef getStringForStopAction(internal::WalkCheckpoint checkpoint,
                                         llvm::StringRef argumentStr) const {
    auto index = getComponentIndex(checkpoint, argumentStr);
    if (index) {
      return getWalkString().take_front(this->components[*index].getEndIndex());
    }

    return StringRef();
  }

  std::string getStringForSkipChildrenAction(llvm::StringRef argumentStr,
                                             bool includePostComponent) const {
    std::string str;
    auto preIndex =
        getComponentIndex(internal::WalkCheckpoint::Pre, argumentStr);
    if (!preIndex)
      return str;

    auto postIndex =
        getComponentIndex(internal::WalkCheckpoint::Post, argumentStr);
    if (!postIndex) {
      return str;
    }

    if (includePostComponent) {
      --(*postIndex);
    }

    str = getWalkString()
              .take_front(this->components[*preIndex].getEndIndex())
              .str();
    str += getWalkString().take_back(
        getWalkString().size() - this->components[*postIndex].getEndIndex());

    return str;
  }

  std::optional<unsigned> getComponentIndex(internal::WalkCheckpoint checkpoint,
                                            llvm::StringRef argumentStr) const {
    for (auto index : indices(this->components)) {
      auto &component = this->components[index];
      if (component.getCheckpoint() == checkpoint) {
        auto arg = this->walkStr.str().slice(component.getArgumentStartIndex(),
                                             component.getArgumentEndIndex());
        if (arg == argumentStr) {
          return index;
        }
      }
    }

    return std::nullopt;
  }
};

} // end namespace

TEST(ASTWalker, QualifiedIdentTypeReprWalkingScheme) {
  TestContext C;
  auto &ctx = C.Ctx;

  const auto dummyLoc = DeclNameLoc(SourceLoc());
  const auto makeDeclNameRef = [&ctx](llvm::StringRef str) {
    return DeclNameRef(ctx.getIdentifier(str));
  };
  const auto TypeRepr_getString = [](TypeRepr *repr) {
    std::string result;
    llvm::raw_string_ostream os(result);
    repr->print(os);
    return result;
  };

  {
    auto *repr = QualifiedIdentTypeRepr::create(
        ctx,
        UnqualifiedIdentTypeRepr::create(ctx, dummyLoc, makeDeclNameRef("A")),
        dummyLoc, makeDeclNameRef("B"));

    EXPECT_EQ(TypeRepr_getString(repr), "A.B");

    const auto verify = [](WalkVerifier &verifier) {
      verifier.testWalkActionsOn("A");
      verifier.testWalkActionsOn("B");
    };

    {
      WalkVerifier verifier(
          repr, QualifiedIdentTypeReprWalkingScheme::ASTOrderRecursive);
      EXPECT_EQ(verifier.getWalkString(), "pr(B)pr(A)po(A)po(B)");
      verify(verifier);
    }
    {
      WalkVerifier verifier(
          repr, QualifiedIdentTypeReprWalkingScheme::SourceOrderRecursive);
      EXPECT_EQ(verifier.getWalkString(), "pr(A)pr(B)po(B)po(A)");
      verify(verifier);
    }
  }

  {
    auto *repr = QualifiedIdentTypeRepr::create(
        ctx,
        QualifiedIdentTypeRepr::create(ctx,
                                       UnqualifiedIdentTypeRepr::create(
                                           ctx, dummyLoc, makeDeclNameRef("A")),
                                       dummyLoc, makeDeclNameRef("B")),
        dummyLoc, makeDeclNameRef("C"));

    EXPECT_EQ(TypeRepr_getString(repr), "A.B.C");

    const auto verify = [](WalkVerifier &verifier) {
      verifier.testWalkActionsOn("A");
      verifier.testWalkActionsOn("B");
      verifier.testWalkActionsOn("C");
    };

    {
      WalkVerifier verifier(
          repr, QualifiedIdentTypeReprWalkingScheme::ASTOrderRecursive);
      EXPECT_EQ(verifier.getWalkString(), "pr(C)pr(B)pr(A)po(A)po(B)po(C)");
      verify(verifier);
    }

    {
      WalkVerifier verifier(
          repr, QualifiedIdentTypeReprWalkingScheme::SourceOrderRecursive);
      EXPECT_EQ(verifier.getWalkString(), "pr(A)pr(B)pr(C)po(C)po(B)po(A)");
      verify(verifier);
    }
  }

  {
    auto *array = new (ctx) ArrayTypeRepr(
        UnqualifiedIdentTypeRepr::create(ctx, dummyLoc, makeDeclNameRef("A")),
        SourceRange());

    auto *repr = QualifiedIdentTypeRepr::create(
        ctx,
        QualifiedIdentTypeRepr::create(ctx, array, dummyLoc,
                                       makeDeclNameRef("B")),
        dummyLoc, makeDeclNameRef("C"));

    EXPECT_EQ(TypeRepr_getString(repr), "[A].B.C");

    const auto verify = [array](WalkVerifier &verifier) {
      verifier.testWalkActionsOn(array);
      verifier.testWalkActionsOn("A");
      verifier.testWalkActionsOn("B");
      verifier.testWalkActionsOn("C");
    };

    {
      WalkVerifier verifier(
          repr, QualifiedIdentTypeReprWalkingScheme::ASTOrderRecursive);
      EXPECT_EQ(verifier.getWalkString(),
                "pr(C)pr(B)pr([A])pr(A)po(A)po([A])po(B)po(C)");
      verify(verifier);
    }

    {
      WalkVerifier verifier(
          repr, QualifiedIdentTypeReprWalkingScheme::SourceOrderRecursive);
      EXPECT_EQ(verifier.getWalkString(),
                "pr([A])pr(A)po(A)pr(B)pr(C)po(C)po(B)po([A])");
      verify(verifier);
    }
  }

  {
    auto *repr = QualifiedIdentTypeRepr::create(
        ctx,
        QualifiedIdentTypeRepr::create(
            ctx,
            UnqualifiedIdentTypeRepr::create(
                ctx, dummyLoc, makeDeclNameRef("A"),
                {UnqualifiedIdentTypeRepr::create(ctx, dummyLoc,
                                                  makeDeclNameRef("W"))},
                SourceRange()),
            dummyLoc, makeDeclNameRef("B"),
            {UnqualifiedIdentTypeRepr::create(ctx, dummyLoc,
                                              makeDeclNameRef("X")),
             UnqualifiedIdentTypeRepr::create(ctx, dummyLoc,
                                              makeDeclNameRef("Y"))},
            SourceRange()),
        dummyLoc, makeDeclNameRef("C"),
        {UnqualifiedIdentTypeRepr::create(ctx, dummyLoc, makeDeclNameRef("Z"))},
        SourceRange());

    EXPECT_EQ(TypeRepr_getString(repr), "A<W>.B<X, Y>.C<Z>");

    const auto verify = [](WalkVerifier &verifier) {
      verifier.testWalkActionsOn("A");
      verifier.testWalkActionsOn("W");
      verifier.testWalkActionsOn("B");
      verifier.testWalkActionsOn("X");
      verifier.testWalkActionsOn("Y");
      verifier.testWalkActionsOn("C");
      verifier.testWalkActionsOn("Z");
    };

    {
      WalkVerifier verifier(
          repr, QualifiedIdentTypeReprWalkingScheme::ASTOrderRecursive);
      // clang-format off
      EXPECT_EQ(verifier.getWalkString(),
                "pr(C)pr(B)pr(A)pr(W)po(W)po(A)pr(X)po(X)pr(Y)po(Y)po(B)pr(Z)po(Z)po(C)");
      verify(verifier);
      // clang-format on
    }

    {
      WalkVerifier verifier(
          repr, QualifiedIdentTypeReprWalkingScheme::SourceOrderRecursive);
      // clang-format off
      EXPECT_EQ(verifier.getWalkString(),
                "pr(A)pr(W)po(W)pr(B)pr(X)po(X)pr(Y)po(Y)pr(C)pr(Z)po(Z)po(C)po(B)po(A)");
      // clang-format on
      verify(verifier);
    }
  }

  {
    auto *array = new (ctx) ArrayTypeRepr(
        QualifiedIdentTypeRepr::create(ctx,
                                       UnqualifiedIdentTypeRepr::create(
                                           ctx, dummyLoc, makeDeclNameRef("Y")),
                                       dummyLoc, makeDeclNameRef("Z")),
        SourceRange());

    auto *repr = QualifiedIdentTypeRepr::create(
        ctx,
        QualifiedIdentTypeRepr::create(
            ctx,
            UnqualifiedIdentTypeRepr::create(
                ctx, dummyLoc, makeDeclNameRef("A"),
                {QualifiedIdentTypeRepr::create(
                     ctx,
                     QualifiedIdentTypeRepr::create(
                         ctx,
                         UnqualifiedIdentTypeRepr::create(ctx, dummyLoc,
                                                          makeDeclNameRef("V")),
                         dummyLoc, makeDeclNameRef("W")),
                     dummyLoc, makeDeclNameRef("X")),
                 array},
                SourceRange()),
            dummyLoc, makeDeclNameRef("B")),
        dummyLoc, makeDeclNameRef("C"));

    EXPECT_EQ(TypeRepr_getString(repr), "A<V.W.X, [Y.Z]>.B.C");

    const auto verify = [array](WalkVerifier &verifier) {
      verifier.testWalkActionsOn("A");
      verifier.testWalkActionsOn("V");
      verifier.testWalkActionsOn("W");
      verifier.testWalkActionsOn("X");
      verifier.testWalkActionsOn(array);
      verifier.testWalkActionsOn("Y");
      verifier.testWalkActionsOn("Z");
      verifier.testWalkActionsOn("B");
      verifier.testWalkActionsOn("C");
    };

    {
      WalkVerifier verifier(
          repr, QualifiedIdentTypeReprWalkingScheme::ASTOrderRecursive);
      // clang-format off
      EXPECT_EQ(verifier.getWalkString(),
                "pr(C)pr(B)pr(A)pr(X)pr(W)pr(V)po(V)po(W)po(X)pr([Y.Z])pr(Z)pr(Y)po(Y)po(Z)po([Y.Z])po(A)po(B)po(C)");
      verify(verifier);
      // clang-format on
    }

    {
      WalkVerifier verifier(
          repr, QualifiedIdentTypeReprWalkingScheme::SourceOrderRecursive);
      // clang-format off
      EXPECT_EQ(verifier.getWalkString(),
                "pr(A)pr(V)pr(W)pr(X)po(X)po(W)po(V)pr([Y.Z])pr(Y)pr(Z)po(Z)po(Y)po([Y.Z])pr(B)pr(C)po(C)po(B)po(A)");
      // clang-format on
      verify(verifier);
    }
  }
}
