#include "swift/AST/AutoDiff.h"
#include "swift/AST/Module.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Range.h"

using namespace swift;

AutoDiffIndexSubset *
AutoDiffIndexSubset::getFromString(ASTContext &ctx, StringRef string) {
  if (string.size() < 0) return nullptr;
  unsigned capacity = string.size();
  llvm::SmallBitVector indices(capacity);
  for (unsigned i : range(capacity)) {
    if (string[i] == 'S')
      indices.set(i);
    else if (string[i] != 'U')
      return nullptr;
  }
  return get(ctx, indices);
}

std::string AutoDiffIndexSubset::getString() const {
  std::string result;
  result.reserve(capacity);
  for (unsigned i : range(capacity))
    result += contains(i) ? 'S' : 'U';
  return result;
}

bool AutoDiffIndexSubset::isSubsetOf(AutoDiffIndexSubset *other) const {
  assert(capacity == other->capacity);
  for (auto index : range(numBitWords))
    if (getBitWord(index) & ~other->getBitWord(index))
      return false;
  return true;
}

bool AutoDiffIndexSubset::isSupersetOf(AutoDiffIndexSubset *other) const {
  assert(capacity == other->capacity);
  for (auto index : range(numBitWords))
    if (~getBitWord(index) & other->getBitWord(index))
      return false;
  return true;
}

AutoDiffIndexSubset *AutoDiffIndexSubset::adding(unsigned index,
                                                 ASTContext &ctx) const {
  assert(index < getCapacity());
  if (contains(index))
    return const_cast<AutoDiffIndexSubset *>(this);
  SmallBitVector newIndices(capacity);
  bool inserted = false;
  for (auto curIndex : getIndices()) {
    if (!inserted && curIndex > index) {
      newIndices.set(index);
      inserted = true;
    }
    newIndices.set(curIndex);
  }
  return get(ctx, newIndices);
}

AutoDiffIndexSubset *AutoDiffIndexSubset::extendingCapacity(
    ASTContext &ctx, unsigned newCapacity) const {
  assert(newCapacity >= capacity);
  if (newCapacity == capacity)
    return const_cast<AutoDiffIndexSubset *>(this);
  SmallBitVector indices(newCapacity);
  for (auto index : getIndices())
    indices.set(index);
  return AutoDiffIndexSubset::get(ctx, indices);
}

int AutoDiffIndexSubset::findNext(int startIndex) const {
  assert(startIndex < (int)capacity && "Start index cannot be past the end");
  unsigned bitWordIndex = 0, offset = 0;
  if (startIndex >= 0) {
    auto indexAndOffset = getBitWordIndexAndOffset(startIndex);
    bitWordIndex = indexAndOffset.first;
    offset = indexAndOffset.second + 1;
  }
  for (; bitWordIndex < numBitWords; ++bitWordIndex, offset = 0) {
    for (; offset < numBitsPerBitWord; ++offset) {
      auto index = bitWordIndex * numBitsPerBitWord + offset;
      auto bitWord = getBitWord(bitWordIndex);
      if (!bitWord)
        break;
      if (index >= capacity)
        return capacity;
      if (bitWord & ((BitWord)1 << offset))
        return index;
    }
  }
  return capacity;
}

int AutoDiffIndexSubset::findPrevious(int endIndex) const {
  assert(endIndex >= 0 && "End index cannot be before the start");
  int bitWordIndex = numBitWords - 1, offset = numBitsPerBitWord - 1;
  if (endIndex < (int)capacity) {
    auto indexAndOffset = getBitWordIndexAndOffset(endIndex);
    bitWordIndex = (int)indexAndOffset.first;
    offset = (int)indexAndOffset.second - 1;
  }
  for (; bitWordIndex >= 0; --bitWordIndex, offset = numBitsPerBitWord - 1) {
    for (; offset < (int)numBitsPerBitWord; --offset) {
      auto index = bitWordIndex * (int)numBitsPerBitWord + offset;
      auto bitWord = getBitWord(bitWordIndex);
      if (!bitWord)
        break;
      if (index < 0)
        return -1;
      if (bitWord & ((BitWord)1 << offset))
        return index;
    }
  }
  return -1;
}
