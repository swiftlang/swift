//===- BottomUpIPAnalysis.h - Bottom-up IP analysis base-class --*- C++ -*-===//
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
// This file contains a base class for bottom-up interprocedural analysis.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_BOTTOMUPIPANALYSIS_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_BOTTOMUPIPANALYSIS_H

#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SIL/SILInstruction.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

/// An abstract base class for interprocedural analysis which are computed in
/// bottom-up order of the call-graph.
/// It provides utilities for automatic invalidation and updating the analysis.
class BottomUpIPAnalysis : public SILAnalysis {

  /// Each update cycle gets a unique ID.
  /// It is incremented for each recomputation of the analysis.
  /// It is mainly used to check for invalid CallerEntries in
  /// FunctionInfoBase::Callers (for details see there).
  int CurrentUpdateID = 0;

protected:

  template<typename FunctionInfo> class FunctionInfoWorkList;

  /// An abstract base class for a \p FunctionInfo class, which stores the
  /// analysis information for a function.
  /// This base class stores the administrative information needed for
  /// invalidation and updating the analysis.
  /// In the following "this function" refers to the associated function.
  template<typename FunctionInfo> class FunctionInfoBase {
  public:

    /// An entry in the caller list.
    struct CallerEntry {
      /// The caller function.
      FunctionInfo *Caller;

      /// The function apply-site.
      /// It can be an apply instruction or an instruction,
      /// which results in a call, e.g. strong_release results in
      /// a destructor call.
      SILInstruction *FAS;

      /// Returns false, if the caller was invalidated since this entry had been
      /// created.
      bool isValid() const { return Caller->UpdateID == UpdateID; }

    private:
      /// Used to check if the caller and its apply-site is still valid.
      int UpdateID;

      CallerEntry(FunctionInfo *Caller, SILInstruction *FAS, int UpdateID) :
        Caller(Caller), FAS(FAS), UpdateID(UpdateID) { }

      friend class FunctionInfoBase<FunctionInfo>;
    };

  private:

    /// The list of callers which must be invalidated if this function gets
    /// invalidated. Note that the list may contain invalid entries for already
    /// invalidated callers. Those entries are removed lazily in
    /// removeInvalidCallers().
    /// The lazy removal of invalid entries avoids that we additionally need to
    /// store a callee-set. Also it reduces the number of times we need to
    /// iterate through the Callers (in case multiple caller functions are
    /// invalidated at the same time).
    /// Note that Callers can't be a pure hashed set-like container because the
    /// iteration order must be deterministic.
    llvm::SmallVector<CallerEntry, 8> Callers;

    /// In which update-cycle the analysis was computed for this function.
    /// A 0-value means that the analysis for this function was not computed
    /// yet or got invalidated.
    int UpdateID = 0;

    /// Special states for StateAndPosition.
    enum {
      NotVisited = -2,
      Visited = -1,
      FirstPosition = 0
    };

    /// The position in the function schedule, or if not scheduled yet, any of
    /// the special states.
    /// This field is only used during recomputation.
    int StateAndPosition = NotVisited;

    /// How many callees are not in the function schedule yet.
    /// This field is only used during recomputation.
    int numUnscheduledCallees = 0;


    /// Removes invalid caller entries.
    void removeInvalidCallers() {
      Callers.erase(std::remove_if(Callers.begin(), Callers.end(),
                                   [](const CallerEntry &E) {
                                     return !E.isValid();
                                   }), Callers.end());
    }

    friend class FunctionInfoWorkList<FunctionInfo>;
    friend class BottomUpIPAnalysis;

  public:

    /// Returns true if the analysis data for this function is computed and
    /// up-to-date.
    bool isValid() const { return UpdateID != 0; }

    /// Returns true if this function was already visited during recomputation.
    bool isVisited() const { return StateAndPosition >= Visited; }

    /// Returns true if this function is already scheduled in the bottom-up
    /// order during recomputation.
    bool isScheduled() const { return StateAndPosition >= FirstPosition; }

    /// Returns true if this function is scheduled after \p RHS in the bottom-up
    /// order, i.e. is considered as a "caller" of \p RHS.
    bool isScheduledAfter(const FunctionInfo *RHS) const {
      assert(isScheduled() && RHS->isScheduled() &&
             "operating with unscheduled functions?");
      return StateAndPosition > RHS->StateAndPosition;
    }

    /// Returns the list of caller-entries.
    ArrayRef<CallerEntry> getCallers() const { return Callers; }

    /// Adds a caller for this function.
    void addCaller(FunctionInfo *CallerInfo, SILInstruction *FAS) {
      Callers.push_back(CallerEntry(CallerInfo, FAS, CallerInfo->UpdateID));
      if (!isScheduled())
        ++CallerInfo->numUnscheduledCallees;
    }

    void addCaller(FunctionInfo *CallerInfo, FullApplySite FAS) {
      addCaller(CallerInfo, FAS.getInstruction());
    }
  };

  /// Computes and stores a bottom-up function order.
  template<typename FunctionInfo> class BottomUpFunctionOrder {

    typedef llvm::SmallVector<FunctionInfo *, 8> FunctionInfoList;

    /// The final bottom-up function order.
    FunctionInfoList Scheduled;

    /// Functions which are not initially scheduled in the bottom-up order.
    FunctionInfoList InitiallyUnscheduled;

    /// The number of visited functions.
    unsigned numVisited = 0;

    /// The BottomUpIPAnalysis::CurrentUpdateID.
    int CurrentUpdateID;

  public:

    /// The constructor. The \p CurrentUpdateID is the
    /// BottomUpIPAnalysis::CurrentUpdateID.
    BottomUpFunctionOrder(int CurrentUpdateID) :
        CurrentUpdateID(CurrentUpdateID) {
      assert(CurrentUpdateID > 0 && "did not allocate an UpdateID");
    }

    ~BottomUpFunctionOrder() {
      assert(InitiallyUnscheduled.empty() &&
             "not finished scheduling");
      assert(Scheduled.size() == numVisited &&
             "missed some functions to schedule");
      for (FunctionInfo *FInfo : Scheduled) {
        assert(FInfo->isScheduled() &&
               "function not scheduled at the end of recomputation");
        assert(FInfo->numUnscheduledCallees == 0 &&
               "something basic is broken in scheduling");
        assert(FInfo->isValid() &&
               "function not recomputed at the end of recomputation");
        FInfo->StateAndPosition = FunctionInfoBase<FunctionInfo>::NotVisited;
      }
    }

    /// Returns the begin of the bottom-up function order.
    typename FunctionInfoList::const_iterator begin() const {
      return Scheduled.begin();
    }

    /// Returns the end of the bottom-up function order.
    typename FunctionInfoList::const_iterator end() const {
      return Scheduled.end();
    }

    /// Returns true if the analysis for \p FInfo was recomputed during the
    /// current update-cycle.
    bool wasRecomputedWithCurrentUpdateID(FunctionInfo *FInfo) {
      return FInfo->UpdateID == CurrentUpdateID;
    }

    /// Should be called when first visiting \p FInfo during recomputation.
    /// Returns true if the analysis is still valid for \p FInfo and doesn't
    /// need to be recomputed.
    bool prepareForVisiting(FunctionInfo *FInfo) {
      assert(!FInfo->isVisited() &&
             "function visited multiple times");
      assert(FInfo->numUnscheduledCallees == 0 &&
             "already added callees at the begin of visiting a function");
      numVisited++;
      FInfo->StateAndPosition = FunctionInfoBase<FunctionInfo>::Visited;
      if (FInfo->isValid()) {
        // Now it's good time to remove invalid caller entries.
        // Note: we don't have to do this for function which are recomputed.
        FInfo->removeInvalidCallers();
        return true;
      }
      InitiallyUnscheduled.push_back(FInfo);
      // Set to valid.
      FInfo->UpdateID = CurrentUpdateID;
      return false;
    }

    /// Tries to schedule \p FInfo onto the bottom-up function order.
    /// This succeeds if \p FInfo does not have any unscheduled callees.
    /// Should be called after visiting \p FInfo during recomputation.
    void tryToSchedule(FunctionInfo *FInfo) {
      assert(FInfo->isVisited() &&
             "tried to schedule function which was not visited");
      assert(!FInfo->isScheduled() &&
             "function scheduled multiple times");
      if (FInfo->numUnscheduledCallees == 0) {
        FInfo->StateAndPosition = (int)Scheduled.size();
        Scheduled.push_back(FInfo);
        return;
      }
      assert(wasRecomputedWithCurrentUpdateID(FInfo) &&
             "not recomputed function cannot have unscheduled callees");
    }

    /// Finishes the bottom-up function schedule.
    void finishScheduling() {
      unsigned Idx = 0;
      bool NeedAnotherIteration = false;
      do {
        /// Schedule all functions we can.
        while (Idx < Scheduled.size()) {
          FunctionInfo *FInfo = Scheduled[Idx++];
          for (const auto &E : FInfo->Callers) {
            if (E.Caller->isVisited() && !E.Caller->isScheduled()) {
              E.Caller->numUnscheduledCallees--;
              tryToSchedule(E.Caller);
            }
          }
        }
        NeedAnotherIteration = false;
        // Check if there are still unscheduled functions, which means that
        // there is a cycle in the call graph.
        while (!InitiallyUnscheduled.empty()) {
          FunctionInfo *FInfo = InitiallyUnscheduled.pop_back_val();
          if (!FInfo->isScheduled()) {
            // Break the cycle by scheduling the first unscheduled node we
            // found.
            FInfo->numUnscheduledCallees = 0;
            tryToSchedule(FInfo);
            assert(FInfo->isScheduled() &&
                   "really, we should be able to schedule this function");
            NeedAnotherIteration = true;
            break;
          }
        }
      } while (NeedAnotherIteration);
    }
  };

  BottomUpIPAnalysis(SILAnalysisKind k) : SILAnalysis(k) {}

  /// Increments the CurrentUpdateID.
  /// Should be called at the beginning of a recomputation.
  void allocNewUpdateID() { ++CurrentUpdateID; }

  /// Returns the ID of the current update-cycle.
  int getCurrentUpdateID() const { return CurrentUpdateID; }

  /// Invalidates \p FInfo, including all analysis data which depend on it, i.e.
  /// the callers.
  template<typename FunctionInfo>
  void invalidateIncludingAllCallers(FunctionInfo *FInfo) {
    llvm::SmallVector<FunctionInfo *, 8> WorkList;
    WorkList.push_back(FInfo);

    while (!WorkList.empty()) {
      FunctionInfo *FInfo = WorkList.pop_back_val();
      for (const auto &E : FInfo->Callers) {
        if (E.isValid() && E.Caller->isValid())
          WorkList.push_back(E.Caller);
      }
      FInfo->clear();
      FInfo->Callers.clear();
      FInfo->UpdateID = 0;
    }
  }
};

} // end namespace swift

#endif // SWIFT_SILOPTIMIZER_ANALYSIS_BOTTOMUPIPANALYSIS_H
