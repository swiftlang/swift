#include "swift/Basic/ListMerger.h"
#include "llvm/ADT/ArrayRef.h"

#include <vector>
#include <random>
#include <iostream>

using namespace swift;

static int compare_unsigned(unsigned lhs, unsigned rhs) {
  return (lhs < rhs ? -1 : lhs > rhs ? 1 : 0);
}

namespace {
struct Entry {
  unsigned id;
  unsigned value;
  Entry *next;
};

struct EntryListTraits {
  static Entry *getNext(Entry *e) { return e->next; }
  static void setNext(Entry *e, Entry *next) { e->next = next; }
  static int compare(Entry *lhs, Entry *rhs) {
    return compare_unsigned(lhs->value, rhs->value);
  }
};

enum Op {
  insert,
  beginMerge,
  endMerge
};

/// An instruction to the test harness: either a simple value
/// (an "insert") or one of the special instructions.
struct Instruction {
  Op op;
  unsigned value;

  Instruction(Op op) : op(op), value(0) { assert(op != insert); }
  Instruction(unsigned value) : op(insert), value(value) {}

  friend std::ostream &operator<<(std::ostream &str, const Instruction &inst) {
    switch (inst.op) {
    case insert: str << inst.value; break;
    case beginMerge: str << "beginMerge"; break;
    case endMerge: str << "endMerge"; break;
    }
    return str;
  }
};
} // end anonymous namespace

template <class T>
static std::ostream &operator<<(std::ostream &str, llvm::ArrayRef<T> list) {
  str << "[";
  for (auto b = list.begin(), i = b, e = list.end(); i != e; ++i) {
    if (i != b) str << ", ";
    str << *i;
  }
  str << "]";
  return str;
}

template <class T>
static std::ostream &operator<<(std::ostream &str, const std::vector<T> &list) {
  return (str << llvm::makeArrayRef(list));
}

static void runTest(llvm::ArrayRef<Instruction> values) {
  std::vector<std::unique_ptr<Entry>> entries;

  ListMerger<Entry*, EntryListTraits> merger;

  unsigned nextID = 0;

  // Between beginMerge and endMerge instructions, values don't get
  // inserted immediately: they build up into a separate list of items
  // that will be merged at the time of the endMerge.  We record this
  // mode by making lastMergeEntry non-null.
  Entry *firstMergeEntry;
  Entry **lastMergeEntry = nullptr;

  for (auto &inst : values) {
    switch (inst.op) {
    case insert: {
      // Create the new entry.
      Entry *entry = new Entry{nextID++, inst.value, nullptr};
      entries.emplace_back(entry);

      // If we're building a merge list, append to the end of it.
      if (lastMergeEntry) {
        *lastMergeEntry = entry;
        lastMergeEntry = &entry->next;

      // Otherwise, just do an insertion.
      } else {
        merger.insert(entry);
      }
      break;
    }

    case beginMerge:
      assert(!lastMergeEntry && "already building a merge list");
      lastMergeEntry = &firstMergeEntry;
      break;

    case endMerge:
      assert(lastMergeEntry && "not building a merge list?");
      // Cap off the merge list we built.
      *lastMergeEntry = nullptr;

      // Do the merge.
      merger.merge(firstMergeEntry);

      // We're no longer building a merge list.
      lastMergeEntry = nullptr;
      break;
    }
  }
  assert(!lastMergeEntry && "ended while still building a merge list");

  // Do a stable sort of the entries.
  std::stable_sort(entries.begin(), entries.end(),
                   [](const std::unique_ptr<Entry> &lhs,
                      const std::unique_ptr<Entry> &rhs) {
                     return (lhs->value < rhs->value);
                   });

  // Make sure that we end up with the same list.
  auto list = merger.release();
  for (auto &entry : entries) {
    std::cout << "  " << list->value << " (" << list->id << ")\n";
    assert(list == entry.get());
    list = list->next;
  }
  assert(list == nullptr);
}

int main() {
  runTest({ 5, 0, 3, 0, 1, 0, 7 });

  std::random_device randomDevice;
  std::default_random_engine e(randomDevice());
  std::uniform_int_distribution<unsigned> dist(0, 20);

  std::vector<Instruction> ins;
  for (unsigned testN = 0; testN < 1000; ++testN) {
    ins.clear();

    const size_t noMerge = -1;
    size_t mergeStart = noMerge;
    for (unsigned i = 0; i < 2000 || mergeStart != noMerge; ++i) {
      if (dist(e) == 0) {
        if (mergeStart != noMerge) {
          std::sort(ins.begin() + mergeStart, ins.end(),
                    [](const Instruction &lhs, const Instruction &rhs) {
                      return lhs.value < rhs.value;
                    });
          ins.push_back(endMerge);
          mergeStart = noMerge;
        } else {
          ins.push_back(beginMerge);
          mergeStart = ins.size();
        }
      } else {
        ins.push_back(dist(e));
      }
    }

    std::cout << ins << std::endl;
    runTest(ins);
  }
}
