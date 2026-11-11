//===--- LinearLifetimeChecker.h ------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_LINEARLIFETIMECHECKER_H
#define SWIFT_SIL_LINEARLIFETIMECHECKER_H

#include "swift/Basic/LLVM.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/BasicBlockUtils.h"

namespace swift {

class SILBasicBlock;
class SILInstruction;
class SILModule;
class SILValue;
class SILOwnershipVerifier;
class SILValueOwnershipChecker;

/// A class used to validate linear lifetime with respect to an SSA-like
/// definition.
///
/// This class is able to both validate that a linear lifetime has been properly
/// constructed (for verification and safety purposes) as well as return to the
/// caller upon failure, what the failure was. In certain cases (for instance if
/// there exists a path without a non-consuming use), the class will report back
/// the specific insertion points needed to insert these compensating releases.
///
/// DISCUSSION: A linear lifetime consists of a starting block or instruction
/// and a list of non-consuming uses and a set of consuming uses. The consuming
/// uses must not be reachable from each other and jointly post-dominate all
/// consuming uses as well as the defining block/instruction.
class LinearLifetimeChecker {
  /// Set by checkValue if a consuming use was found to be reachable from another
  /// consuming use.
  bool foundOverConsume = false;

  friend class GuaranteedPhiVerifier;
  friend class SILOwnershipVerifier;
  friend class SILValueOwnershipChecker;

public:
  /// Returns true that \p value forms a linear lifetime with consuming uses \p
  /// consumingUses, non consuming uses \p nonConsumingUses. Returns false
  /// otherwise.
  bool validateLifetime(SILValue value, ArrayRef<Operand *> consumingUses,
                        ArrayRef<Operand *> nonConsumingUses);

  /// Given a value and a consuming use of that value, compute a non-unique
  /// minimal set of insertion points that together with \p consumingUse
  /// post-dominate and end the lifetime of \p value.
  ///
  /// Returns true if we completed the consuming use set and discovered that \p
  /// consumingUse is not strongly control equivalent to value (meaning
  /// consumingUse is not in the same loop in the loop nest as value).
  bool completeConsumingUseSet(
      SILValue value, Operand *consumingUse,
      function_ref<void(SILBasicBlock::iterator insertPt)> visitor);

private:
  /// Checks the linear lifetime of \p value.
  ///
  /// Returns true if no error was found, i.e. if:
  ///
  /// 1. No consuming uses are reachable from any other consuming use, from any
  /// non-consuming uses, or from the producer instruction.
  /// 2. The consuming use set jointly post dominates producers and all non
  /// consuming uses.
  ///
  /// Returns false if an error was found. This is independent of \p printError:
  /// also if the error was printed and the ownership verifier is in testing
  /// mode, false is returned.
  ///
  /// If a consuming use is reachable from another consuming use,
  /// `foundOverConsume` is set.
  ///
  /// \p value The value whose lifetime we are checking.
  /// \p consumingUses the array of users that destroy or consume a value.
  /// \p nonConsumingUses regular uses
  /// \p printError If true, errors are printed and the compiler aborts (unless
  /// the ownership verifier is in testing mode). If false, errors are only
  /// returned.
  /// \p leakingBlocks If non-null a list of blocks where the value was detected
  /// to leak. Can be used to insert missing destroys.
  bool checkValue(SILValue value, ArrayRef<Operand *> consumingUses,
                  ArrayRef<Operand *> nonConsumingUses, bool printError);

  bool checkValue(SILValue value, ArrayRef<Operand *> consumingUses,
                  ArrayRef<Operand *> nonConsumingUses, bool printError,
                  function_ref<void(SILBasicBlock *)> leakingBlockCallback);

  /// The implementation of checkValue. Returns true if no error was found.
  ///
  /// \p nonConsumingUsesOutsideLifetimeCallback If not None, called for all
  /// non-consuming uses which are outside of the lifetime of \p value.
  bool checkValueImpl(
      SILValue value, ArrayRef<Operand *> consumingUses,
      ArrayRef<Operand *> nonConsumingUses, bool printError,
      std::optional<function_ref<void(SILBasicBlock *)>> leakingBlockCallback,
      std::optional<function_ref<void(Operand *)>>
          nonConsumingUsesOutsideLifetimeCallback);
};

/// Prints an ownership error in \p function, using \p messagePrinterFunc to
/// print the details of the error.
///
/// Aborts the compiler, unless the ownership verifier is in testing mode.
///
/// Always returns false (if it returns at all), meaning "verification failed".
/// This lets callers directly return the result of this function in their
/// error path.
bool printOwnershipError(const SILFunction *function,
                         llvm::function_ref<void()> messagePrinterFunc);

} // namespace swift

#endif
