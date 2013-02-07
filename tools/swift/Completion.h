//===-- Completion.h - Completion engine for swift immediate mode -* C++ *-===//
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
// This module provides completions to the immediate mode environment.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Allocator.h"
#include <memory>
#include <vector>

namespace swift {
  class DeclContext;

/// State of a completion operation.
enum class CompletionState {
  /// An invalid completion set.
  Invalid,
  /// No matching completions were found. Subsequent <TAB>s just beep.
  Empty,
  /// A unique completion was found. Subsequent <TAB>s just beep.
  Unique,
  /// Completions have been looked up and the root has been completed. This is
  /// the state after the first <TAB> event. The next <TAB> will display
  /// available completions.
  CompletedRoot,
  /// Displayed the list of available completions. Subsequent <TAB> events will
  /// cycle through completing different stems.
  DisplayedCompletionList
};
  
/// Represents a completion set and maintains state for navigating through
/// a set of completions.
class Completions {
  CompletionState state;
  std::unique_ptr<llvm::BumpPtrAllocator> strings;
  
  std::vector<llvm::StringRef> completions;
  size_t enteredLength;
  size_t rootLength;
  size_t currentStem;
  
  llvm::StringRef allocateCopy(llvm::StringRef s);
  
public:
  /// Create an invalid completion set.
  Completions() : state(CompletionState::Invalid) {}
  
  /// Create a completion set containing completions appropriate to the given
  /// string.
  Completions(DeclContext *dc,
              llvm::StringRef prefix);
  
  /// Returns true if this is a valid completion set.
  explicit operator bool() const { return state != CompletionState::Invalid; }
  bool isValid() const { return state != CompletionState::Invalid; }
  
  // True if no completions were found.
  bool isEmpty() const { return state == CompletionState::Empty; }
  
  /// True if the completion is unique.
  bool isUnique() const { return state == CompletionState::Unique; }
  
  /// Returns the current state of the completion.
  CompletionState getState() const { return state; }
  /// Sets the state of the completion.
  void setState(CompletionState s) { state = s; }
  
  /// Returns the common root of all the found completions (or the entire
  /// completion for a unique completion).
  llvm::StringRef getRoot() const;
  
  /// Returns the stem (if any) returned by the previous getNextStem() call.
  llvm::StringRef getPreviousStem() const;
  
  /// Returns the next completion stem. Cycles to the beginning of the list if
  /// the end was reached.
  llvm::StringRef getNextStem();
  
  /// Returns a list of all complete names for which completions were found.
  llvm::ArrayRef<llvm::StringRef> getCompletionList() const {
    return completions;
  }
  
  /// Reset the completion set to an invalid state.
  void reset();
};

}
