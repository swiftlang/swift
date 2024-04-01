#include <cassert>
#include <iostream>
#include <random>
#include <vector>

#define private public
#include "swift/Basic/PriorityQueue.h"

using namespace swift;

namespace {

enum EntryOrder { priorityOrder, reverseCreationOrder };

struct Entry {
  unsigned id;
  unsigned value;
  Entry *next;
};

class EntryFactory {
  std::vector<std::unique_ptr<Entry>> entries;
  unsigned nextID = 0;

public:
  Entry *create(unsigned value) {
    auto entry = new Entry{nextID++, value, nullptr};
    entries.emplace_back(entry);
    return entry;
  }

  void remove(Entry *e) {
    auto it = std::find_if(
        entries.begin(), entries.end(),
        [=](const std::unique_ptr<Entry> &ptr) { return ptr.get() == e; });
    assert(it != entries.end());
    entries.erase(it);
  }

  /// Sort the entries in this list.
  ///
  /// \param reverseCreationOrder - if true, then order equal-value
  ///   nodes in the reverse of creation order; otherwise
  ///   creator order of creation order
  void sort(EntryOrder order) {
    if (order == priorityOrder) {
      std::sort(entries.begin(), entries.end(),
                [=](const std::unique_ptr<Entry> &lhs,
                    const std::unique_ptr<Entry> &rhs) {
                  if (lhs->value != rhs->value)
                    return lhs->value < rhs->value;
                  return lhs->id < rhs->id;
                });
    } else {
      std::sort(
          entries.begin(), entries.end(),
          [=](const std::unique_ptr<Entry> &lhs,
              const std::unique_ptr<Entry> &rhs) { return lhs->id > rhs->id; });
    }
  }

  void checkSameAs(Entry *list) {
    for (auto &entry : entries) {
      std::cout << "  " << list->value << " (" << list->id << ")\n";
      assert(list == entry.get());
      list = list->next;
    };
    assert(list == nullptr);
  }
};

struct EntryListTraits {
  static Entry *getNext(Entry *e) { return e->next; }
  static void setNext(Entry *e, Entry *next) { e->next = next; }
  enum { prioritiesCount = 4 };
  static int getPriorityIndex(Entry *e) {
    assert(e->value < prioritiesCount);
    return (int)e->value;
  }
};

using EntrySimpleQueue = SimpleQueue<Entry *, EntryListTraits>;
using EntryPriorityQueue = PriorityQueue<Entry *, EntryListTraits>;

static void runPrependTest() {
  std::cout << "runPrependTest()" << std::endl;
  EntryFactory entries;
  EntrySimpleQueue queue;
  assert(!queue.head);
  assert(!queue.tail);

  auto first = entries.create(3);
  queue.prepend(first);
  assert(queue.head == first);
  assert(queue.tail == first);

  auto second = entries.create(0);
  queue.prepend(second);
  assert(queue.head == second);
  assert(queue.tail == first);

  auto third = entries.create(2);
  queue.prepend(third);
  assert(queue.head == third);
  assert(queue.tail == first);

  entries.sort(reverseCreationOrder);
  entries.checkSameAs(queue.head);
}

static void runEnqueueDequeueTest() {
  std::cout << "runEnqueueDequeueTest()" << std::endl;
  EntryFactory entries;
  EntryPriorityQueue queue;
  assert(!queue.head);
  assert(!queue.tails[0]);
  assert(!queue.tails[1]);
  assert(!queue.tails[2]);
  assert(!queue.tails[3]);

  auto first = entries.create(3);
  queue.enqueue(first);
  assert(queue.head == first);
  assert(!queue.tails[0]);
  assert(!queue.tails[1]);
  assert(!queue.tails[2]);
  assert(queue.tails[3] == first);

  auto second = entries.create(3);
  queue.enqueue(second);
  assert(queue.head == first);
  assert(!queue.tails[0]);
  assert(!queue.tails[1]);
  assert(!queue.tails[2]);
  assert(queue.tails[3] == second);

  auto third = entries.create(1);
  queue.enqueue(third);
  assert(queue.head == third);
  assert(!queue.tails[0]);
  assert(queue.tails[1] == third);
  assert(!queue.tails[2]);
  assert(queue.tails[3] == second);

  auto fourth = entries.create(2);
  queue.enqueue(fourth);
  assert(queue.head == third);
  assert(!queue.tails[0]);
  assert(queue.tails[1] == third);
  assert(queue.tails[2] == fourth);
  assert(queue.tails[3] == second);

  entries.sort(priorityOrder);
  entries.checkSameAs(queue.head);

  auto pop = [&](Entry *expected) {
    auto e = queue.dequeue();
    assert(e == expected);
    entries.remove(e);
    entries.sort(priorityOrder);
    entries.checkSameAs(queue.head);
  };

  pop(third);
  assert(queue.head == fourth);
  assert(!queue.tails[0]);
  assert(!queue.tails[1]);
  assert(queue.tails[2] == fourth);
  assert(queue.tails[3] == second);

  pop(fourth);
  assert(queue.head == first);
  assert(!queue.tails[0]);
  assert(!queue.tails[1]);
  assert(!queue.tails[2]);
  assert(queue.tails[3] == second);

  pop(first);
  assert(queue.head == second);
  assert(!queue.tails[0]);
  assert(!queue.tails[1]);
  assert(!queue.tails[2]);
  assert(queue.tails[3] == second);

  pop(second);
  assert(!queue.head);
  assert(!queue.tails[0]);
  assert(!queue.tails[1]);
  assert(!queue.tails[2]);
  assert(!queue.tails[3]);

  assert(!queue.dequeue());
  assert(!queue.head);
  assert(!queue.tails[0]);
  assert(!queue.tails[1]);
  assert(!queue.tails[2]);
  assert(!queue.tails[3]);
}

static void runConcreteTests() {
  runPrependTest();
  runEnqueueDequeueTest();
}

struct TestConfig {
  unsigned numTests;
  unsigned maxEntries;
};

static void runEnqueueDequeueTests(const TestConfig &config) {
  std::random_device randomDevice;
  std::default_random_engine e(randomDevice());
  std::uniform_int_distribution<unsigned> valueDist(
      0, EntryListTraits::prioritiesCount - 1);
  std::uniform_int_distribution<unsigned> numEntriesDist(0, config.maxEntries);

  for (unsigned testN = 0; testN < config.numTests; ++testN) {
    std::cout << "runEnqueueDequeueTests() #" << testN << std::endl;
    EntryFactory entries;
    EntryPriorityQueue queue;
    unsigned numEntries = numEntriesDist(e);
    std::cout << "numEntries = " << numEntries << std::endl;
    for (unsigned i = 0; i < numEntries; ++i) {
      auto value = valueDist(e);
      std::cout << "-- " << value << std::endl;
      queue.enqueue(entries.create(value));
    }
    entries.sort(priorityOrder);
    entries.checkSameAs(queue.head);
    for (unsigned i = 0; i < numEntries; ++i) {
      auto e = queue.dequeue();
      std::cout << "pop " << e->value << std::endl;
      entries.remove(e);
      entries.sort(priorityOrder);
      entries.checkSameAs(queue.head);
    }
  }
}

} // namespace

int main() {
  runConcreteTests();
  TestConfig config = {.numTests = 1000, .maxEntries = 20};
  runEnqueueDequeueTests(config);
}
