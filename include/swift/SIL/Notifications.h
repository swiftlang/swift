//===--- Notifications.h - SIL Undef Value Representation -------*- C++ -*-===//
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

#ifndef SWIFT_SIL_NOTIFICATIONS_H
#define SWIFT_SIL_NOTIFICATIONS_H

#include "swift/Basic/LLVM.h"
#include "swift/Basic/STLExtras.h"
#include "swift/SIL/SILMoveOnlyDeinit.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include <memory>

namespace swift {

class SILNode;
class ModuleDecl;
class SILFunction;
class SILWitnessTable;
class SILDefaultOverrideTable;
class SILDefaultWitnessTable;
class SILGlobalVariable;
class SILVTable;

/// An abstract class for handling SIL deserialization notifications.
///
/// This is an interface that should be implemented by clients that wish to
/// maintain a list of notification handlers. In contrast, handler
/// implementations should instead subclass DeserializationNotificationHandler
/// so that default no-op implementations can be inherited and so that it can be
/// passed into DeserializationNotificationHandlerSet.
class DeserializationNotificationHandlerBase {
public:
  /// Observe that we deserialized a function declaration.
  virtual void didDeserialize(ModuleDecl *mod, SILFunction *fn) = 0;

  /// Observe that we successfully deserialized a function body.
  virtual void didDeserializeFunctionBody(ModuleDecl *mod, SILFunction *fn) = 0;

  /// Observe that we successfully deserialized a witness table's entries.
  virtual void didDeserializeWitnessTableEntries(ModuleDecl *mod,
                                                 SILWitnessTable *wt) = 0;

  /// Observe that we successfully deserialized a default override table's
  /// entries.
  virtual void
  didDeserializeDefaultOverrideTableEntries(ModuleDecl *mod,
                                            SILDefaultOverrideTable *ot) = 0;

  /// Observe that we successfully deserialized a default witness table's
  /// entries.
  virtual void
  didDeserializeDefaultWitnessTableEntries(ModuleDecl *mod,
                                           SILDefaultWitnessTable *wt) = 0;

  /// Observe that we deserialized a global variable declaration.
  virtual void didDeserialize(ModuleDecl *mod, SILGlobalVariable *var) = 0;

  /// Observe that we deserialized a v-table declaration.
  virtual void didDeserialize(ModuleDecl *mod, SILVTable *vtable) = 0;

  /// Observe that we deserialized a witness-table declaration.
  virtual void didDeserialize(ModuleDecl *mod, SILWitnessTable *wtable) = 0;

  /// Observe that we deserialized a default witness-table declaration.
  virtual void didDeserialize(ModuleDecl *mod,
                              SILDefaultWitnessTable *wtable) = 0;

  /// Observe that we deserialized a default override table declaration.
  virtual void didDeserialize(ModuleDecl *mod,
                              SILDefaultOverrideTable *otable) = 0;

  /// Observe that we deserialized a move only deinit declaration.
  virtual void didDeserialize(ModuleDecl *mod, SILMoveOnlyDeinit *deinit) = 0;

  virtual ~DeserializationNotificationHandlerBase() = default;
};

/// A no-op implementation of DeserializationNotificationHandlerBase. Intended
/// to allow for users to implement only one of the relevant methods and have
/// all other methods be no-ops.
class DeserializationNotificationHandler
    : public DeserializationNotificationHandlerBase {
public:
  /// Observe that we deserialized a function declaration.
  virtual void didDeserialize(ModuleDecl *mod, SILFunction *fn) override {}

  /// Observe that we successfully deserialized a function body.
  virtual void didDeserializeFunctionBody(ModuleDecl *mod,
                                          SILFunction *fn) override {}

  /// Observe that we successfully deserialized a witness table's entries.
  virtual void didDeserializeWitnessTableEntries(ModuleDecl *mod,
                                                 SILWitnessTable *wt) override {
  }

  /// Observe that we successfully deserialized a override table's entries.
  virtual void didDeserializeDefaultOverrideTableEntries(
      ModuleDecl *mod, SILDefaultOverrideTable *ot) override {}

  /// Observe that we successfully deserialized a default witness table's
  /// entries.
  virtual void didDeserializeDefaultWitnessTableEntries(
      ModuleDecl *mod, SILDefaultWitnessTable *wt) override {}

  /// Observe that we deserialized a global variable declaration.
  virtual void didDeserialize(ModuleDecl *mod,
                              SILGlobalVariable *var) override {}

  /// Observe that we deserialized a v-table declaration.
  virtual void didDeserialize(ModuleDecl *mod, SILVTable *vtable) override {}

  /// Observe that we deserialized a witness-table declaration.
  virtual void didDeserialize(ModuleDecl *mod,
                              SILWitnessTable *wtable) override {}

  /// Observe that we deserialized a default witness-table declaration.
  virtual void didDeserialize(ModuleDecl *mod,
                              SILDefaultWitnessTable *wtable) override {}

  /// Observe that we deserialized a default override table declaration.
  virtual void didDeserialize(ModuleDecl *mod,
                              SILDefaultOverrideTable *otable) override {}

  /// Observe that we deserialized a move only deinit declaration.
  virtual void didDeserialize(ModuleDecl *mod,
                              SILMoveOnlyDeinit *deinit) override {}

  virtual StringRef getName() const = 0;

  virtual ~DeserializationNotificationHandler() = default;
};

/// A notification handler that only overrides didDeserializeFunctionBody and
/// calls the passed in function pointer.
class FunctionBodyDeserializationNotificationHandler final
    : public DeserializationNotificationHandler {
public:
  using Handler = void (*)(ModuleDecl *, SILFunction *);

private:
  Handler handler;

public:
  FunctionBodyDeserializationNotificationHandler(Handler handler)
      : handler(handler) {}
  virtual ~FunctionBodyDeserializationNotificationHandler() {}

  void didDeserializeFunctionBody(ModuleDecl *mod, SILFunction *fn) override {
    (*handler)(mod, fn);
  }

  StringRef getName() const override {
    return "FunctionBodyDeserializationNotificationHandler";
  }
};

/// A type that contains a set of unique DeserializationNotificationHandlers and
/// implements DeserializationNotificationHandlerBase by iterating over the
/// stored handlers and calling each handler's implementation.
class DeserializationNotificationHandlerSet final
    : public DeserializationNotificationHandlerBase {
public:
  using NotificationUniquePtr =
    std::unique_ptr<DeserializationNotificationHandler>;

private:
  /// A list of deserialization callbacks that update the SILModule and other
  /// parts of SIL as deserialization occurs.
  ///
  /// We use 3 here since that is the most that will ever be used today in the
  /// compiler. If that changed, that number should be changed as well. The
  /// specific users are:
  ///
  /// 1. SILModule's serialization callback.
  /// 2. SILPassManager notifications.
  /// 3. Access Enforcement Stripping notification.
  SmallVector<NotificationUniquePtr, 3> handlerSet;

public:
  DeserializationNotificationHandlerSet() = default;
  ~DeserializationNotificationHandlerSet() = default;

  bool erase(DeserializationNotificationHandler *handler) {
    auto iter = find_if(handlerSet, [&](const NotificationUniquePtr &h) {
      return handler == h.get();
    });
    if (iter == handlerSet.end())
      return false;
    handlerSet.erase(iter);
    return true;
  }

  void add(NotificationUniquePtr &&handler) {
    // Since we store unique_ptrs and are accepting a movable rvalue here, we
    // should never have a case where we have a notification added twice. But
    // just to be careful, lets use an assert.
    assert(!count_if(handlerSet, [&](const NotificationUniquePtr &h) {
             return handler.get() == h.get();
           }) && "Two unique ptrs pointing at the same memory?!");
    handlerSet.emplace_back(std::move(handler));
  }

  static DeserializationNotificationHandler *
  getUnderlyingHandler(const NotificationUniquePtr &h) {
    return h.get();
  }

  /// An iterator into the notification set that returns a bare
  /// 'DeserializationNotificationHandler *' projected from one of the
  /// underlying std::unique_ptr<DeserializationNotificationHandler>.
  using iterator = llvm::mapped_iterator<
      decltype(handlerSet)::const_iterator,
      decltype(&DeserializationNotificationHandlerSet::getUnderlyingHandler)>;
  using range = iterator_range<iterator>;

  /// Returns an iterator to the first element of the handler set.
  ///
  /// NOTE: This iterator has a value_type of
  /// 'DeserializationNotificationHandler'.
  iterator begin() const {
    auto *fptr = &DeserializationNotificationHandlerSet::getUnderlyingHandler;
    return llvm::map_iterator(handlerSet.begin(), fptr);
  }

  /// Returns an iterator to the end of the handler set.
  ///
  /// NOTE: This iterator has a value_type of
  /// 'DeserializationNotificationHandler'.
  iterator end() const {
    auto *fptr = &DeserializationNotificationHandlerSet::getUnderlyingHandler;
    return llvm::map_iterator(handlerSet.end(), fptr);
  }

  /// Returns a range that iterates over bare pointers projected from the
  /// internal owned memory pointers in the handlerSet.
  ///
  /// NOTE: The underlying iterator value_type here is
  /// 'DeserializationNotificationHandler *'.
  auto getRange() const -> range { return llvm::make_range(begin(), end()); }

  //===--------------------------------------------------------------------===//
  // DeserializationNotificationHandler implementation via chaining to the
  // handlers we contain.
  //===--------------------------------------------------------------------===//

  void didDeserialize(ModuleDecl *mod, SILFunction *fn) override;
  void didDeserializeFunctionBody(ModuleDecl *mod, SILFunction *fn) override;
  void didDeserializeWitnessTableEntries(ModuleDecl *mod,
                                         SILWitnessTable *wt) override;
  void
  didDeserializeDefaultWitnessTableEntries(ModuleDecl *mod,
                                           SILDefaultWitnessTable *wt) override;
  void didDeserializeDefaultOverrideTableEntries(
      ModuleDecl *mod, SILDefaultOverrideTable *ot) override;
  void didDeserialize(ModuleDecl *mod, SILGlobalVariable *var) override;
  void didDeserialize(ModuleDecl *mod, SILVTable *vtable) override;
  void didDeserialize(ModuleDecl *mod, SILWitnessTable *wtable) override;
  void didDeserialize(ModuleDecl *mod,
                      SILDefaultWitnessTable *wtable) override;
  void didDeserialize(ModuleDecl *mod,
                      SILDefaultOverrideTable *otable) override;
  void didDeserialize(ModuleDecl *mod, SILMoveOnlyDeinit *deinit) override;
};
} // namespace swift

#endif
