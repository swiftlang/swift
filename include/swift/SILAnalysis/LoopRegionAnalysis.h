//===--- LoopRegionAnalysis.h ---------------------------------------------===//
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
///
/// Purpose
/// =======
///
/// This is an analysis that is an implementation of interval analysis in order
/// to allow for dataflow to be done in a structured way on the loop tree. The
/// facilities it provides are:
///
/// 1. An abstraction called ``LoopRegion`` that abstracts over natural loops,
/// basic blocks, and the top level function. This is meant to provide a
/// function level pointer for summarizing information.
///
/// 2. For each Region, if it has subregions (i.e. is not a Block), an RPO
/// ordering of the Subregions are provided. Loops in this RPO ordering have the
/// RPO number of their header.
///
/// 3. Natural loops are defined by the loops that SILLoopInfo can find. Thus it
/// is useful to be able to know about loops that are not able to be found by
/// SILLoopInfo (i.e. irreducible loops). This information is computed by
/// finding all backedges and any backedge that is not recognized by SILLoopInfo
/// as a backedge has its head and tail region marked as unknown control flow
/// boundaries. This can be queries via the methods:
///
/// - LoopRegion::isUnknownControlFlowEdgeHead().
/// - LoopRegion::isUnknownControlFlowEdgeTail().
///
/// 4. Only loops with single back edges are supported by this analysis. If such
/// a loop is detected, we mark the head, tail of the edge as irreducible
/// control flow boundaries. To handle such loops, we rely on loop
/// canonicalization to transform the multiple backedge loops into multiple
/// loops with singular backedges or by merging the backedge blocks.
///
/// Algorithm
/// =========
///
/// The main consideration here is that we want to ensure that we only traverse
/// the CFG once and reuse the information that is already provided via the loop
/// info analysis and the post order analysis.
///
/// We begin by visiting each block in RPO order. During this RPO traversal, we:
///
/// 1. Create LoopRegions for each block and wire up each block's predecessor
/// and successor blocks as predecessors/successors of the block's region.
///
/// 2. Detect any backedges that are unknown to SILLoopInfo and mark the blocks
/// that form the head/tail of the backedge as unknown control flow boundaries.
///
/// 3. We lookup/create the region for the innermost loop that contains the
/// block and add the block's region as a subregion of that loop.
///
/// Then we perform a postorder DFS of the loop nest. The postorder provides the
/// inductive rule that all loops will be visited after all of their subloops
/// hae been visited. If a Loop has no subloops (i.e. all subregions are
/// blocks), we do nothing. Otherwise, if the loop does have subloops, we visit
/// each subloop and do the following:
///
/// 1. The region data structure of the header of the subloop still is the
/// successor of its predecessor outside of the subloop in the various block
/// datastructures. Rewire those regions to consider the loop to be their
/// successor instead and make each of those regions on of the predecessors of
/// the subloop. Then clear the predecessor list of the subloop's header
/// region. The header should have no predecessor edges after this operation
/// completes.
///
/// 2. For each of the exiting blocks of the subloop (that is the blocks inside
/// the loop that leave the subloop), we perform a similar operation as with the
/// header, replacing the predecessor edge in the exiting blocks successors with
/// an edge pointing at the subloop instead of at the exiting block and
/// vis-a-versa. After this is complete, the exiting blocks should have no
/// successor edges going outside the loop.
///
/// 3. If the loop has multiple back edges, mark each of the backedges as
/// unknown control flow boundaries.
///
/// We complete by performing the same operation on the top level function.
///
/// After this is done, each level of the loop hierarchy should be able to be
/// treated from a dataflow perspective as its own separate function with only
/// one caller, the parent loop region (or the top level function), enabling
/// extremely aggressive operations (or even outlining).
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILANALYSIS_LOOPNESTANALYSIS_H
#define SWIFT_SILANALYSIS_LOOPNESTANALYSIS_H

#include "swift/SILAnalysis/Analysis.h"
#include "swift/SILAnalysis/PostOrderAnalysis.h"
#include "swift/SILAnalysis/LoopAnalysis.h"
#include "swift/SILPasses/PassManager.h"
#include "swift/SIL/LoopInfo.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILFunction.h"
#include "swift/Basic/Range.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

class LoopRegionFunctionInfo;

/// A loop region is a data structure which represents one of a basic block,
/// loop, or function. In the case of a loop, function, it contains an internal
/// data structure that represents the subregions of the loop/function. This
/// data is tail allocated so that the basic block case is not penalized by
/// storing this unnecessary information.
class LoopRegion {

  /// This is a data structure that is an unsigned integer with a top bit flag
  /// that says whether it is an RPO ID for a BB Region or is an RPO ID of the
  /// header BB of a Loop Region that used as a placeholder to ensure that the
  /// Loop is visited in the proper RPO order. When the placeholder flag is hit,
  /// the subloop array should be searched for the pair that has that flag as a
  /// first element. The loop's flag will be the second element of the pair.
  struct SubregionID {
    static constexpr unsigned IDBitSize = (sizeof(unsigned) * CHAR_BIT) - 1;
    static constexpr unsigned MaxID = (unsigned(1) << IDBitSize) - 1;
    unsigned IsLoop : 1;
    unsigned ID : IDBitSize;

    SubregionID(unsigned id, bool isloop) {
      IsLoop = unsigned(isloop);
      ID = id;
    }
    bool operator<(const SubregionID &Other) const { return ID < Other.ID; }
  };
  /// These checks are just for performance.
  static_assert(IsTriviallyCopyable<SubregionID>::value,
                "Expected trivially copyable type");

  struct SubregionData;

public:
  /// We operate in terms of FunctionTy, LoopTy, and BlockTy so that this can
  /// potentially be ported to LLVM in the future.
  using FunctionTy = SILFunction;
  using LoopTy = SILLoop;
  using BlockTy = SILBasicBlock;

  /// An iterator that knows how to iterate over the subregion indices of a
  /// region.
  class subregion_iterator :
    public std::iterator<std::bidirectional_iterator_tag, unsigned> {
    friend struct SubregionData;
    llvm::SmallVectorImpl<SubregionID>::iterator InnerIter;
    const llvm::SmallVectorImpl<std::pair<unsigned, unsigned>> *Subloops;

    /// A flag that says that this iterator is an invalid iterator belonging to
    /// a basic block. The iterator can only be compared against another invalid
    /// iterator. Any other use causes an unreachable being hit.
    ///
    /// The reason this is needed is because basic blocks can not have
    /// subregions, yet many graph algorithms want to be able to iterate over
    /// the subregions of a region regardless of whether it is a basic block,
    /// loop, or function. By allowing for invalid iterators for basic blocks,
    /// we can allow for this.
    unsigned IsInvalid : 1;

    subregion_iterator(
        llvm::SmallVectorImpl<SubregionID>::iterator iter,
        const llvm::SmallVectorImpl<std::pair<unsigned, unsigned>> *subloops)
        : InnerIter(iter), Subloops(subloops), IsInvalid(false) {}

  public:
    using value_type = unsigned;
    using reference = unsigned;
    using pointer = void;
    using iterator_category = std::bidirectional_iterator_tag;
    using difference_type = int;

    /// Construct a subregion_iterator suitable for use with a basic block. It
    /// does not contain any data and can only be compared against another
    /// invalid iterator (for which it will return true). Any other usage
    /// results in an unreachable being hit.
    subregion_iterator() : InnerIter(), Subloops(), IsInvalid(true) {}

    /// Return the index of the current subregion index.
    unsigned operator*() const {
      if (IsInvalid)
        llvm_unreachable("Invalid Iterator?!");
      auto ID = *InnerIter;
      if (!ID.IsLoop)
        return ID.ID;
      for (auto &p : *Subloops) {
        if (p.first == ID.ID) {
          return p.second;
        }
      }
      llvm_unreachable("Out of sync subloops array?!");
    }

    subregion_iterator &operator++() {
      if (IsInvalid)
        llvm_unreachable("Invalid Iterator?!");
      InnerIter++;
      return *this;
    }
    subregion_iterator operator++(int) {
      if (IsInvalid)
        llvm_unreachable("Invalid Iterator?!");
      return subregion_iterator(InnerIter++, Subloops);
    }
    subregion_iterator &operator--() {
      if (IsInvalid)
        llvm_unreachable("Invalid Iterator?!");
      InnerIter--;
      return *this;
    }
    subregion_iterator operator--(int) {
      if (IsInvalid)
        llvm_unreachable("Invalid Iterator?!");
      return subregion_iterator(InnerIter--, Subloops);
    }
    bool operator==(subregion_iterator rhs) const {
      if (IsInvalid) {
        if (rhs.IsInvalid)
          return true;
        llvm_unreachable("Invalid Iterator?!");
      }
      return InnerIter == rhs.InnerIter;
    }
    bool operator!=(subregion_iterator rhs) const { return !(*this == rhs); }
  };
  using subregion_reverse_iterator = std::reverse_iterator<subregion_iterator>;

private:
  /// A pointer to one of a Loop, Basic Block, or Function represented by this
  /// region.
  llvm::PointerUnion3<FunctionTy *, LoopTy *, BlockTy *> Ptr;

  /// The ID of this region.
  unsigned ID;

  /// The parent region of this ID if it exists.
  llvm::Optional<unsigned> ParentID;

  /// The IDs of the predecessor regions of this region.
  llvm::SmallVector<unsigned, 4> Preds;

  /// The IDs of the successor regions of this region.
  llvm::SmallVector<unsigned, 4> Succs;

  /// True if this region the head of an edge that results from control flow
  /// that we do not handle.
  ///
  /// Currently this includes irreducible control flow and loops with multiple
  /// backedges. We rely on loop canonicalization to handle the multiple
  /// backedge case.
  bool IsUnknownControlFlowEdgeHead;

  /// True if this region the tail of an edge that results from control flow
  /// that we do not handle.
  ///
  /// Currently this includes irreducible control flow and loops with multiple
  /// backedges. We rely on loop canonicalization to handle the multiple
  /// backedge case.
  bool IsUnknownControlFlowEdgeTail;

  /// A tail allocated LoopSubregionData structure that is used to store state
  /// about subregions of a loop.
  ///
  /// We tail allocate this onto LoopRegions for loops/functions so that in the
  /// case of having a Block, we do not have any memory size overhead of these
  /// SmallVectors, but at the same time have reasonable memory performance. In
  /// the case of loops this overhead is acceptable since we shouldn't have many
  /// loops (compared to Blocks).
  struct SubregionData {
    /// The RPO number of the header block of this loop or function. This is
    /// used as the RPO number for the whole region.
    unsigned RPONumOfHeaderBlock;

    /// A list of subregion IDs of this region sorted in RPO order.
    ///
    /// This takes advantage of the fact that the ID of a basic block is the
    /// block's RPO number.
    ///
    /// This contains IDs that represent both basic blocks and loops that are
    /// subregions of this region. What is key to notice is that a loop is
    /// represented by the RPO number of its header. We use an auxillary map to
    /// map the preheader's RPO number to the loop's ID.
    llvm::SmallVector<SubregionID, 16> Subregions;

    /// A map from RPO number of a subregion loop's preheader to a subloop
    /// regions id. This is neccessary since we represent a loop in the
    /// Subregions array by the RPO number of its header.
    llvm::SmallVector<std::pair<unsigned, unsigned>, 2> Subloops;

    subregion_iterator begin() {
      return subregion_iterator(Subregions.begin(), &Subloops);
    }
    subregion_iterator end() {
      return subregion_iterator(Subregions.end(), &Subloops);
    }
    subregion_reverse_iterator rbegin() {
      return subregion_reverse_iterator(begin());
    }
    subregion_reverse_iterator rend() {
      return subregion_reverse_iterator(end());
    }

    unsigned size() const { return Subregions.size(); }
    bool empty() const { return Subregions.empty(); }

    void addBlockSubregion(LoopRegion *R) {
      assert(R->ID <= SubregionID::MaxID && "Unrepresentable ID");
      Subregions.push_back(SubregionID(R->ID, false));
    }

    void addLoopSubregion(LoopRegion *L, LoopRegion *Header) {
      assert(Header->ID <= SubregionID::MaxID && "Unrepresentable ID");
      Subregions.push_back(SubregionID(Header->ID, true));
      Subloops.push_back({Header->ID, L->ID});
    }

    /// Once we finish processing a loop, we sort its subregions so that they
    /// are guaranteed to be in RPO order. This works because each BB's ID is
    /// its RPO number and we represent loops by the RPO number of their
    /// preheader (with a flag in the first bit to say to look in the subloop
    /// array for the *real* ID of the loop).
    ///
    /// TODO: Is this necessary? We visit BBs in RPO order. This means that we
    /// should always add BBs in RPO order to subregion lists, no? For now I am
    /// going to sort just to be careful while bringing this up.
    void sortSubregions() { std::sort(Subregions.begin(), Subregions.end()); }
  };

  SubregionData &getSubregionData() {
    assert(!isBlock() && "BBs do not have subregion data");
    return reinterpret_cast<SubregionData &>(*(this + 1));
  }

  const SubregionData &getSubregionData() const {
    assert(!isBlock() && "BBs do not have subregion data");
    return reinterpret_cast<const SubregionData &>(*(this + 1));
  }

  friend class LoopRegionFunctionInfo;

public:
  /// These will assert if this is not a loop region. If this is a loop region,
  /// the forward iterators will give the IDs of the subregions in RPO order.
  /// The backward iterators will give the IDs of the subregions in PO order.
  ///
  /// The reason why all of this work is being done for subregions is because we
  /// are working around the following contradiction:
  ///
  /// 1. There are not that many loops in a function so it makes sense to take
  /// advantage of this to store more data that makes performing analysis easy
  /// (such as the IDs of subregions of the loop in RPO order).
  /// 2. There are a lot of BBs in a function so it makes sense to try to
  /// minimize the amount of state stored for each BB.
  /// 3. This is a data structure that attempts to generalize over both of them
  /// without using dynamic dispatch.
  ///
  /// We use tail allocation of the extra data for loops so we do not incur the
  /// memory cost of large SmallVectors for BBs.
  subregion_iterator subregion_begin() {
    if (isBlock())
      return subregion_iterator();
    return getSubregionData().begin();
  }

  /// This is the end equivalent of subregion_begin(). Please see comment on
  /// subregion_begin().
  subregion_iterator subregion_end() {
    if (isBlock())
      return subregion_iterator();
    return getSubregionData().end();
  }

  bool subregions_empty() const { return getSubregionData().empty(); }
  unsigned subregions_size() const { return getSubregionData().size(); }
  subregion_reverse_iterator subregion_rbegin() {
    return getSubregionData().rbegin();
  }
  subregion_reverse_iterator subregion_rend() {
    return getSubregionData().rend();
  }
  llvm::iterator_range<subregion_iterator> getSubregions() {
    return {subregion_begin(), subregion_end()};
  }
  llvm::iterator_range<subregion_reverse_iterator> getReverseSubregions() {
    return {subregion_rbegin(), subregion_rend()};
  }

  /// Returns true if \p R is an immediate subregion of this region.
  bool containsSubregion(LoopRegion *R) {
    auto End = subregion_end();
    return std::find(subregion_begin(), End, R->getID()) != End;
  }

  using pred_iterator = llvm::SmallVectorImpl<unsigned>::iterator;
  using pred_const_iterator = llvm::SmallVectorImpl<unsigned>::const_iterator;

  pred_iterator pred_begin() { return Preds.begin(); }
  pred_iterator pred_end() { return Preds.end(); }
  pred_const_iterator pred_begin() const { return Preds.begin(); }
  pred_const_iterator pred_end() const { return Preds.end(); }
  bool pred_empty() const { return Preds.empty(); }
  unsigned pred_size() const { return Preds.size(); }

  using succ_iterator = llvm::SmallVectorImpl<unsigned>::iterator;
  using succ_const_iterator = llvm::SmallVectorImpl<unsigned>::const_iterator;
  succ_iterator succ_begin() { return Succs.begin(); }
  succ_iterator succ_end() { return Succs.end(); }
  succ_const_iterator succ_begin() const { return Succs.begin(); }
  succ_const_iterator succ_end() const { return Succs.end(); }
  bool succ_empty() const { return Succs.empty(); }
  unsigned succ_size() const { return Succs.size(); }

  BlockTy *getBlock() const;
  LoopTy *getLoop() const;
  FunctionTy *getFunction() const;

  bool isBlock() const { return Ptr.is<BlockTy *>(); }
  bool isLoop() const { return Ptr.is<LoopTy *>(); }
  bool isFunction() const { return Ptr.is<FunctionTy *>(); }

  /// Is this the head of an edge that causes unknown control flow.
  ///
  /// This means that dataflow that enters the region must not propagate any
  /// information into this region from predecessors.
  bool isUnknownControlFlowEdgeHead() const {
    return IsUnknownControlFlowEdgeHead;
  }

  /// Is this the head of an edge that causes unknown control flow.
  ///
  /// This means that dataflow state must not be propagated out of this region.
  bool isUnknownControlFlowEdgeTail() const {
    return IsUnknownControlFlowEdgeTail;
  }

  /// Returns the ID of this region in the region array.
  ///
  /// For basic blocks this is the RPO number of the basic block. This is done
  /// just as a convenient way to compress our region data structures.
  unsigned getID() const { return ID; }

  /// Return the ID of the parent region of this BB. Asserts if this is a
  /// function region.
  unsigned getParentID() const { return *ParentID; }

  unsigned getRPONumber() const {
    if (isBlock())
      return getID();
    return getSubregionData().RPONumOfHeaderBlock;
  }

  void replacePred(unsigned OldPredID, unsigned NewPredID) {
    for (unsigned i : indices(Preds)) {
      if (Preds[i] != OldPredID)
        continue;
      Preds[i] = NewPredID;
      // This may not be necessary. I am just being conservative for now and it
      // shouldn't be expensive.
      //
      // TODO: Investigate if this can be removed.
      sortUnique(Preds);
      return;
    }
    llvm_unreachable("Replacing OldPredID that is not a predecessor?");
  }

  void replaceSucc(unsigned OldSuccID, unsigned NewSuccID) {
    for (unsigned i : indices(Succs)) {
      if (Succs[i] != OldSuccID)
        continue;
      Succs[i] = NewSuccID;
      // This may not be necessary. I am just being conservative for now and it
      // shouldn't be expensive.
      //
      // TODO: Investigate if this can be removed.
      sortUnique(Succs);
      return;
    }
    llvm_unreachable("Attempting to replace a successor that is not a "
                     "succesor?!");
  }

  void dump() const;
  void print(llvm::raw_ostream &os, bool insertSpaces = false) const;
  void dumpName() const;
  void printName(llvm::raw_ostream &os) const;

private:
  LoopRegion(LoopTy *L, unsigned Idx)
      : Ptr(L), ID(Idx), ParentID(), Preds(), Succs(),
        IsUnknownControlFlowEdgeHead(false),
        IsUnknownControlFlowEdgeTail(false) {}

  LoopRegion(BlockTy *BB, unsigned Idx)
      : Ptr(BB), ID(Idx), ParentID(), Preds(), Succs(),
        IsUnknownControlFlowEdgeHead(false),
        IsUnknownControlFlowEdgeTail(false) {}

  LoopRegion(FunctionTy *F, unsigned Idx)
      : Ptr(F), ID(Idx), ParentID(), Preds(), Succs(),
        IsUnknownControlFlowEdgeHead(false),
        IsUnknownControlFlowEdgeTail(false) {}

  void setParent(LoopRegion *PR) {
    assert(!isFunction() && "Functions can not be subregions");
    assert(!PR->isBlock() && "BB regions can not be parents of a region");
    ParentID = PR->getID();
  }

  void addPred(LoopRegion *LNR) {
    assert(!isFunction() && "Functions can not have predecessors");
    Preds.push_back(LNR->ID);
    sortUnique(Preds);
  }

  void addSucc(LoopRegion *LNR) {
    assert(!isFunction() && "Functions can not have successors");
    Succs.push_back(LNR->ID);
    sortUnique(Succs);
  }

  void addBlockSubregion(LoopRegion *R) {
    assert(!isBlock() && "Blocks can not have subregions");
    assert(R->isBlock() && "Assumed R was a basic block");
    R->setParent(this);
    getSubregionData().addBlockSubregion(R);
  }

  void addLoopSubregion(LoopRegion *L, LoopRegion *Header) {
    assert(!isBlock() && "Blocks can not have subregions");
    assert(L->isLoop() && "Assumed L was a loop");
    assert(Header->isBlock() && "Assumed Header was a loop");
    L->setParent(this);
    getSubregionData().addLoopSubregion(L, Header);
  }

};

class LoopRegionFunctionInfo {
  using RegionTy = LoopRegion;
  using BlockTy = SILBasicBlock;
  using LoopInfoTy = SILLoopInfo;
  using LoopTy = SILLoop;
  using FunctionTy = SILFunction;

  /// The function that this data structure contains loop regions for.
  FunctionTy *F;

  /// A bump ptr allocator that we allocate regions from.
  llvm::BumpPtrAllocator Allocator;

  /// The ID in the IDToRegionMap of the Region associated with \p F.
  llvm::Optional<unsigned> FunctionRegionID;

  /// A map from a BB to the ID in the IDToRegionMap of the Region associated
  /// with the BB.
  ///
  /// *NOTE* This ID is *also* the function level RPO number of the BB.
  llvm::DenseMap<BlockTy *, unsigned> BBToIDMap;

  /// A map from a Loop to the ID in the IDToRegionMap of the Region associated
  /// with the loop.
  llvm::DenseMap<LoopTy *, unsigned> LoopToIDMap;

  /// A map from an unsigned integer ID to a region.
  ///
  /// *WARNING* Before modifying the initializiation of this field of the data
  /// structure please read the comment below:
  ///
  /// We assign IDs to BBs, Loops, and the top level Function, so that we can
  /// abstract above the underlying type of any specific region. A key thing to
  /// notice is that we always allocate regions associated with BBs before
  /// regions associated with Loops or the top level function (1). The reason
  /// that we do this is to ensure that each BBs ID is the RPO number of the BB
  /// at the function level. This is done so we do not have to represent the RPO
  /// numbers in a separate array. This means that when initializing this data
  /// structure, we need to be careful about how we initialize regions in order
  /// to preserve said property.
  ///
  /// In order to avoid having to initialize loop/function regions after BB
  /// regions (which would cause us to have to visit BBs twice), we instead
  /// allocate the memory for all of the BBs up front and set the pointer entry
  /// to be nullptr.
  ///
  /// Then when we initialize BBRegions, we just assign the resulting Region's
  /// pointer into the array using its RPO index rather than performing a
  /// push_back. For regions associated with Loops, Functions we still perform a
  /// push_back. This enables us to initialize regions for all types at the same
  /// time while preserving said property.
  ///
  /// (1) Currently there is no work done to ensure that the creation of the
  /// function level region will have any ordering with respect to the creation
  /// of any of the loop regions.
  std::vector<RegionTy *> IDToRegionMap;

  /// True if all BB regions have been created. Only in Debug Builds. Used to
  /// fire asserts to make sure that:
  ///
  ///   RegionTy *createRegion(SILBasicBlock *, unsigned)
  ///
  /// is only called in initializeBBRegions and everywhere else:
  ///
  ///   RegionTy *getRegion(SILBasicBlock *)
  ///
  /// is called.
#ifndef NDEBUG
  unsigned AllBBRegionsCreated : 1;
#endif

public:
  LoopRegionFunctionInfo(FunctionTy *F, PostOrderFunctionInfo *POI,
                         LoopInfoTy *LI);


  RegionTy *getRegion(unsigned RegionID) const {
    return IDToRegionMap[RegionID];
  }

  /// Look up the region associated with this block and return it. Asserts if
  /// the block does not have a region associated with it.
  ///
  /// Regions associated with blocks are only created by the function
  /// createRegion(). FunctionTy and LoopTy have their RegionTys created via
  /// getRegion() lazily. For more information read the documentation for
  /// IDToRegionMap.
  RegionTy *getRegion(BlockTy *BB) const;

  /// Return the RegionTy associated with \p F. Creates the region if it does
  /// not exist yet.
  RegionTy *getRegion(FunctionTy *F) const;

  /// Return the RegionTy associated with \p Loop. Creates the region if it does
  /// not exist yet.
  RegionTy *getRegion(LoopTy *Loop) const;

  using iterator = decltype(IDToRegionMap)::iterator;
  using const_iterator = decltype(IDToRegionMap)::const_iterator;
  iterator begin() { return IDToRegionMap.begin(); }
  iterator end() { return IDToRegionMap.end(); }
  const_iterator begin() const { return IDToRegionMap.begin(); }
  const_iterator end() const { return IDToRegionMap.end(); }
  unsigned size() const { return IDToRegionMap.size(); }
  bool empty() const { return IDToRegionMap.empty(); }
  RegionTy *getTopLevelRegion() { return getRegion(F); }

  FunctionTy *getFunction() const { return F; }

  void dump() const;
  void print(llvm::raw_ostream &os) const;
  void viewLoopRegions() const;

private:
  RegionTy *createRegion(BlockTy *BB, unsigned RPONum);

  /// Initialize regions for all basic blocks.
  ///
  /// This initializes all BBs to have their regular predecessors, successors in
  /// the CFG ignoring BBs that are unreachable from the entry. During
  /// initialization of loops, we modify these edges in the region graph by
  /// removing and or substituting edges to loops for basic block edges. See
  /// large comment at the top of the file.
  void initializeBlockRegions(PostOrderFunctionInfo *PI, LoopInfoTy *LI);

  /// Initialize regions for all loops.
  ///
  /// This traverses the loop nest bottom up:
  ///
  /// 1. Removing backedges of loops.
  /// 2. Changing header predecessors to be loop predecessors.
  /// 3. Changing exiting block successors to be loop successors.
  ///
  /// For more information see large comment at the top of LoopRegionAnalysis.h.
  void initializeLoopRegions(LoopInfoTy *LI);

  /// Initialize the subregions of a function, treating the function as a top
  /// level loop.
  void
  initializeFunctionRegion(iterator_range<LoopInfoTy::iterator> TopLevelLoops);

  /// This is a refactored routine that handles common initialization of loops
  /// and functions.
  void
  initializeLoopFunctionRegion(RegionTy *ParentRegion,
                               iterator_range<LoopInfoTy::iterator> SubLoops);

  /// This helper routine rewrites all predecessors of the header of \p Loop to
  /// be predecessors of \p Loop and vis-a-versa. Returns the region for the
  /// SubLoopHeaderRegion
  RegionTy *
  rewriteLoopHeaderPredecessors(LoopTy *Loop, RegionTy *LoopRegion);

  /// This helper routine rewrites all successors of loop region exiting blocks
  /// to be successors of the loop. It also removes backedges and ignores
  /// non-backedge edges in the loop.
  void
  rewriteLoopExitingBlockSuccessors(LoopTy *Loop, RegionTy *LoopRegion);

  /// This helper routine for a given block, creates successor edges in between
  /// the block's region and all of the regions associated with the block's
  /// successor blocks.
  void initializeBlockRegionSuccessors(BlockTy *BB, RegionTy *BBRegion,
                                       PostOrderFunctionInfo *PI);

  /// This helper method goes through the predecessors of the basic block \p
  /// NonHeaderBB and determines if any of them are back edges. Since we know
  /// that \p NonHeaderBB is a basic block that has not been identified by loop
  /// info as a loop header, we know that these backedges are not known to us
  /// via loop info. We treat them as irreducible control flow edges to be
  /// conservative. In truth some of them may not be due to deficiencies in loop
  /// info.
  ///
  /// TODO: This needs a better name.
  void
  markIrreducibleLoopPredecessorsOfNonLoopHeader(BlockTy *NonHeaderBB,
                                                 RegionTy *NonHeaderBBRegion,
                                                 PostOrderFunctionInfo *PI);

  /// This helper method takes in a loop that has multiple loop latches and
  /// marks each of those loop latches as being unknown control flow tails. We
  /// do not support loops with multiple loop latches and instead rely on loops
  /// to be canonicalized to have one back edge. But we still need to be
  /// conservatively correct.
  ///
  /// TODO: This needs a better name.
  void
  markMultipleLoopLatchLoopBackEdges(RegionTy *LoopHeaderRegion,
                                     LoopTy *L,
                                     PostOrderFunctionInfo *PI);
};

class LoopRegionAnalysis : public FunctionAnalysisBase<LoopRegionFunctionInfo> {
  SILLoopAnalysis *SLA;
  PostOrderAnalysis *POA;

public:
  LoopRegionAnalysis(SILModule *M)
    : FunctionAnalysisBase<LoopRegionFunctionInfo>(AnalysisKind::LoopRegion) {}

  LoopRegionAnalysis(const LoopRegionAnalysis &) = delete;
  LoopRegionAnalysis &operator=(const LoopRegionAnalysis &) = delete;

  virtual ~LoopRegionAnalysis() {}

  virtual void initialize(SILPassManager *PM) override;

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::LoopRegion;
  }

  virtual LoopRegionFunctionInfo *newFunctionAnalysis(SILFunction *F) override {
    return new LoopRegionFunctionInfo(F, POA->get(F), SLA->get(F));
  }

  virtual bool shouldInvalidate(SILAnalysis::PreserveKind K) override {
    return !(K & PreserveKind::Branches);
  }
};

} // end namespace swift

namespace llvm {
raw_ostream &operator<<(raw_ostream &os, swift::LoopRegion &LR);
}

#endif
