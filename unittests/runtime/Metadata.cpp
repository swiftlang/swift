//===--- Metadata.cpp - Metadata tests ------------------------------------===//
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

#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Concurrent.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/Once.h"
#include "gtest/gtest.h"
#include <iterator>
#include <functional>
#include <vector>
#include <thread>
#include <condition_variable>

using namespace swift;

// Race testing.

template <typename T>
struct RaceThreadContext {
  std::function<T()> code;
  T result;

  unsigned numThreads;
  unsigned &numThreadsReady;
  std::mutex &sharedMutex;
  std::condition_variable &start_condition;
};

template <typename T>
void RaceThunk(RaceThreadContext<T> &ctx) {
  // update shared state
  std::unique_lock<std::mutex> lk(ctx.sharedMutex);
  ++ctx.numThreadsReady;
  bool isLastThread = ctx.numThreadsReady == ctx.numThreads;

  // wait until the rest of the thunks are ready
  ctx.start_condition.wait(lk, [&ctx]{ // waiting releases the lock
    return ctx.numThreadsReady == ctx.numThreads;
  });
  lk.unlock();

  // The last thread will signal the condition_variable to kick off the rest
  // of the waiting threads to start.
  if (isLastThread) ctx.start_condition.notify_all();

  ctx.result = ctx.code();
}

/// RaceTest(code) runs code in many threads simultaneously,
/// and returns a vector of all returned results.
template <typename T, unsigned NumThreads = 64>
std::vector<T> RaceTest(std::function<T()> code) {
  unsigned numThreadsReady = 0;
  std::mutex sharedMutex;
  std::condition_variable start_condition;
  T result = NULL;

  // Create the contexts
  std::vector<RaceThreadContext<T>> contexts(NumThreads, {
		  code,
		  result,
		  NumThreads,
		  numThreadsReady,
		  sharedMutex,
		  start_condition});

  // Create the threads
  std::vector<std::thread> threads;
  threads.reserve(NumThreads);
  for (unsigned i = 0; i < NumThreads; i++)
    threads.emplace_back(std::bind(RaceThunk<T>, std::ref(contexts[i])));

  // Collect results.
  std::vector<T> results;
  results.reserve(NumThreads);
  for (unsigned i = 0; i < NumThreads; i++) {
    threads[i].join();
    results.emplace_back(contexts[i].result);
  }
  return results;
}

/// RaceTest_ExpectEqual(code) runs code in many threads simultaneously,
/// verifies that they all returned the same value, and returns that value.
template<typename T>
T RaceTest_ExpectEqual(std::function<T()> code)
{
  auto results = RaceTest<T>(code);
  auto r0 = results[0];
  for (auto r : results) {
    EXPECT_EQ(r0, r);
  }

  return r0;
}

/// Some unique global pointers.
uint32_t Global1 = 0;
uint32_t Global2 = 0;
uint32_t Global3 = 0;

struct TestObjContainer {
  swift_once_t token;
  HeapObject obj;
};

TestObjContainer StaticTestObj;
HeapMetadata StaticTestMD;

TEST(StaticObjects, ini) {
  RaceTest_ExpectEqual<const Metadata *>(
     [&]() -> const Metadata * {
       // Check if the object header is initialized once.
       HeapObject *o = swift_initStaticObject(&StaticTestMD, &StaticTestObj.obj);
       EXPECT_EQ(o, &StaticTestObj.obj);
       EXPECT_EQ(StaticTestObj.obj.metadata, &StaticTestMD);
#ifdef __APPLE__
       EXPECT_NE(StaticTestObj.token, 0);
#endif
       const int NumRcOps = 1000;
       for (int i = 0; i < NumRcOps; i++) {
         swift_retain(&StaticTestObj.obj);
       }
       for (int i = 0; i < NumRcOps; i++) {
         swift_release(&StaticTestObj.obj);
       }
       return StaticTestObj.obj.metadata;
     });
  EXPECT_EQ(swift_retainCount(&StaticTestObj.obj), 1u);
}

TEST(Concurrent, ConcurrentList) {
  const int numElem = 100;
  const int elemVal = 1;

  ConcurrentList<int> List;
  auto results = RaceTest<int*>(
    [&]() -> int* {
        for (int i = 0; i < numElem; i++)
          List.push_front(elemVal);
        return nullptr;
    }
  );

  size_t ListLen = std::distance(List.begin(), List.end());
  // Check that all of the values are initialized properly.
  for (auto A : List) {
    EXPECT_EQ(elemVal, A);
  }

  // Check that the length of the list is correct.
  EXPECT_EQ(ListLen, results.size() * numElem);
}


TEST(Concurrent, ConcurrentMap) {
  const int numElem = 100;

  struct Entry {
    size_t Key;
    Entry(size_t key) : Key(key) {}
    int compareWithKey(size_t key) const {
      return (key == Key ? 0 : (key < Key ? -1 : 1));
    }
    static size_t getExtraAllocationSize(size_t key) { return 0; }
    size_t getExtraAllocationSize() const { return 0; }
  };

  ConcurrentMap<Entry> Map;

  // Add a bunch of numbers to the map concurrently.
   auto results = RaceTest<int*>(
    [&]() -> int* {
      for (int i = 0; i < numElem; i++) {
        size_t hash = (i * 123512) % 0xFFFF ;
        Map.getOrInsert(hash);
      }
      return nullptr;
    }
  );

  // Check that all of the values that we inserted are in the map.
  for (int i=0; i < numElem; i++) {
    size_t hash = (i * 123512) % 0xFFFF ;
    EXPECT_TRUE(Map.find(hash));
  }
}

FullMetadata<ClassMetadata> MetadataTest2 = {
  { { nullptr }, { &VALUE_WITNESS_SYM(Bo) } },
  { { nullptr }, ClassFlags(), 0, 0, 0, 0, 0, 0 }
};

TEST(MetadataTest, getMetatypeMetadata) {
  auto inst1 = RaceTest_ExpectEqual<const MetatypeMetadata *>(
    [&]() -> const MetatypeMetadata * {
      auto inst = swift_getMetatypeMetadata(&METADATA_SYM(Bi64_).base);

      EXPECT_EQ(sizeof(void*), inst->getValueWitnesses()->size);
      return inst;
    });

  auto inst2 = RaceTest_ExpectEqual<const MetatypeMetadata *>(
    [&]() -> const MetatypeMetadata * {
      auto inst = swift_getMetatypeMetadata(&METADATA_SYM(Bi32_).base);

      EXPECT_EQ(sizeof(void*), inst->getValueWitnesses()->size);
      return inst;
    });

  auto inst3 = RaceTest_ExpectEqual<const MetatypeMetadata *>(
    [&]() -> const MetatypeMetadata * {
      auto inst = swift_getMetatypeMetadata(&MetadataTest2);

      EXPECT_EQ(sizeof(void*), inst->getValueWitnesses()->size);
      return inst;
    });

  auto inst4 = RaceTest_ExpectEqual<const MetatypeMetadata *>(
    [&]() -> const MetatypeMetadata * {
      auto inst = swift_getMetatypeMetadata(inst3);
      EXPECT_EQ(sizeof(void*), inst->getValueWitnesses()->size);
      return inst;
    });

  auto inst5 = RaceTest_ExpectEqual<const MetatypeMetadata *>(
    [&]() -> const MetatypeMetadata * {
      auto inst = swift_getMetatypeMetadata(inst1);
      EXPECT_EQ(sizeof(void*), inst->getValueWitnesses()->size);
      return inst;
    });

  // After all this, the instance type fields should still be valid.
  ASSERT_EQ(&METADATA_SYM(Bi64_).base, inst1->InstanceType);
  ASSERT_EQ(&METADATA_SYM(Bi32_).base, inst2->InstanceType);
  ASSERT_EQ(&MetadataTest2, inst3->InstanceType);
  ASSERT_EQ(inst3, inst4->InstanceType);
  ASSERT_EQ(inst1, inst5->InstanceType);
}

ProtocolDescriptor ProtocolA{
  "_TMp8Metadata9ProtocolA",
  nullptr,
  ProtocolDescriptorFlags()
    .withSwift(true)
    .withClassConstraint(ProtocolClassConstraint::Any)
    .withDispatchStrategy(ProtocolDispatchStrategy::Swift)
};

ProtocolDescriptor ProtocolB{
  "_TMp8Metadata9ProtocolB",
  nullptr,
  ProtocolDescriptorFlags()
    .withSwift(true)
    .withClassConstraint(ProtocolClassConstraint::Any)
    .withDispatchStrategy(ProtocolDispatchStrategy::Swift)
};

ProtocolDescriptor ProtocolError{
  "_TMp8Metadata13ProtocolError",
  nullptr,
  ProtocolDescriptorFlags()
    .withSwift(true)
    .withClassConstraint(ProtocolClassConstraint::Any)
    .withDispatchStrategy(ProtocolDispatchStrategy::Swift)
    .withSpecialProtocol(SpecialProtocol::Error)
};

ProtocolDescriptor ProtocolClassConstrained{
  "_TMp8Metadata24ProtocolClassConstrained",
  nullptr,
  ProtocolDescriptorFlags()
    .withSwift(true)
    .withClassConstraint(ProtocolClassConstraint::Class)
    .withDispatchStrategy(ProtocolDispatchStrategy::Swift)
};

ProtocolDescriptor ProtocolNoWitnessTable{
  "_TMp8Metadata22ProtocolNoWitnessTable",
  nullptr,
  ProtocolDescriptorFlags()
    .withSwift(true)
    .withClassConstraint(ProtocolClassConstraint::Class)
    .withDispatchStrategy(ProtocolDispatchStrategy::ObjC)
};

TEST(MetadataTest, getExistentialMetadata) {
  const ProtocolDescriptor *protoList1[] = {};
  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto any = swift_getExistentialTypeMetadata(ProtocolClassConstraint::Any,
                                                  /*superclass=*/nullptr,
                                                  0, protoList1);
      EXPECT_EQ(MetadataKind::Existential, any->getKind());
      EXPECT_EQ(0U, any->Flags.getNumWitnessTables());
      EXPECT_EQ(ProtocolClassConstraint::Any, any->Flags.getClassConstraint());
      EXPECT_EQ(0U, any->Protocols.NumProtocols);
      EXPECT_EQ(SpecialProtocol::None,
                any->Flags.getSpecialProtocol());
      EXPECT_EQ(nullptr,
                any->getSuperclassConstraint());
      return any;
    });

  const ProtocolDescriptor *protoList2[] = {
    &ProtocolA
  };
  auto exA = RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto a = swift_getExistentialTypeMetadata(ProtocolClassConstraint::Any,
                                                /*superclass=*/nullptr,
                                                1, protoList2);
      EXPECT_EQ(MetadataKind::Existential, a->getKind());
      EXPECT_EQ(1U, a->Flags.getNumWitnessTables());
      EXPECT_EQ(ProtocolClassConstraint::Any, a->Flags.getClassConstraint());
      EXPECT_EQ(1U, a->Protocols.NumProtocols);
      EXPECT_EQ(&ProtocolA, a->Protocols[0]);
      EXPECT_EQ(SpecialProtocol::None,
                a->Flags.getSpecialProtocol());
      EXPECT_EQ(nullptr,
                a->getSuperclassConstraint());
      return a;
    });

  const ProtocolDescriptor *protoList3[] = {
    &ProtocolB
  };
  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto b = swift_getExistentialTypeMetadata(ProtocolClassConstraint::Any,
                                                /*superclass=*/nullptr,
                                                1, protoList3);
      EXPECT_NE(exA, b);
      EXPECT_EQ(MetadataKind::Existential, b->getKind());
      EXPECT_EQ(1U, b->Flags.getNumWitnessTables());
      EXPECT_EQ(ProtocolClassConstraint::Any, b->Flags.getClassConstraint());
      EXPECT_EQ(1U, b->Protocols.NumProtocols);
      EXPECT_EQ(&ProtocolB, b->Protocols[0]);
      EXPECT_EQ(SpecialProtocol::None,
                b->Flags.getSpecialProtocol());
      EXPECT_EQ(nullptr,
                b->getSuperclassConstraint());
      return b;
    });

  const ProtocolDescriptor *protoList6[] = {
    &ProtocolClassConstrained,
  };
  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto classConstrained
        = swift_getExistentialTypeMetadata(ProtocolClassConstraint::Class,
                                           /*superclass=*/nullptr,
                                           1, protoList6);
      EXPECT_EQ(MetadataKind::Existential, classConstrained->getKind());
      EXPECT_EQ(1U, classConstrained->Flags.getNumWitnessTables());
      EXPECT_EQ(ProtocolClassConstraint::Class,
                classConstrained->Flags.getClassConstraint());
      EXPECT_EQ(1U, classConstrained->Protocols.NumProtocols);
      EXPECT_EQ(SpecialProtocol::None,
                classConstrained->Flags.getSpecialProtocol());
      EXPECT_EQ(&ProtocolClassConstrained, classConstrained->Protocols[0]);
      EXPECT_EQ(nullptr,
                classConstrained->getSuperclassConstraint());
      return classConstrained;
    });

  const ProtocolDescriptor *protoList7[] = {
    &ProtocolNoWitnessTable
  };
  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto noWitnessTable
        = swift_getExistentialTypeMetadata(ProtocolClassConstraint::Class,
                                           /*superclass=*/nullptr,
                                           1, protoList7);
      EXPECT_EQ(MetadataKind::Existential, noWitnessTable->getKind());
      EXPECT_EQ(0U, noWitnessTable->Flags.getNumWitnessTables());
      EXPECT_EQ(ProtocolClassConstraint::Class,
                noWitnessTable->Flags.getClassConstraint());
      EXPECT_EQ(1U, noWitnessTable->Protocols.NumProtocols);
      EXPECT_EQ(SpecialProtocol::None,
                noWitnessTable->Flags.getSpecialProtocol());
      EXPECT_EQ(&ProtocolNoWitnessTable, noWitnessTable->Protocols[0]);
      EXPECT_EQ(nullptr,
                noWitnessTable->getSuperclassConstraint());
      return noWitnessTable;
    });

  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      const ProtocolDescriptor *protoList8[] = {
        &ProtocolNoWitnessTable,
        &ProtocolA,
        &ProtocolB
      };

      auto mixedWitnessTable
        = swift_getExistentialTypeMetadata(ProtocolClassConstraint::Class,
                                           /*superclass=*/nullptr,
                                           3, protoList8);
      EXPECT_EQ(MetadataKind::Existential, mixedWitnessTable->getKind());
      EXPECT_EQ(2U, mixedWitnessTable->Flags.getNumWitnessTables());
      EXPECT_EQ(ProtocolClassConstraint::Class,
                mixedWitnessTable->Flags.getClassConstraint());
      EXPECT_EQ(3U, mixedWitnessTable->Protocols.NumProtocols);
      EXPECT_EQ(SpecialProtocol::None,
                mixedWitnessTable->Flags.getSpecialProtocol());
      EXPECT_EQ(nullptr,
                mixedWitnessTable->getSuperclassConstraint());
      return mixedWitnessTable;
    });

  const ValueWitnessTable *ExpectedErrorValueWitnesses;
#if SWIFT_OBJC_INTEROP
  ExpectedErrorValueWitnesses = &VALUE_WITNESS_SYM(BO);
#else
  ExpectedErrorValueWitnesses = &VALUE_WITNESS_SYM(Bo);
#endif

  const ProtocolDescriptor *protoList9[] = {
    &ProtocolError
  };
  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto special
        = swift_getExistentialTypeMetadata(ProtocolClassConstraint::Any,
                                           /*superclass=*/nullptr,
                                           1, protoList9);
      EXPECT_EQ(MetadataKind::Existential, special->getKind());
      EXPECT_EQ(1U, special->Flags.getNumWitnessTables());
      EXPECT_EQ(SpecialProtocol::Error,
                special->Flags.getSpecialProtocol());
      EXPECT_EQ(ExpectedErrorValueWitnesses,
                special->getValueWitnesses());
      EXPECT_EQ(nullptr,
                special->getSuperclassConstraint());
      return special;
    });

  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      const ProtocolDescriptor *protoList10[] = {
        &ProtocolError,
        &ProtocolA
      };

      auto special
        = swift_getExistentialTypeMetadata(ProtocolClassConstraint::Any,
                                           /*superclass=*/nullptr,
                                           2, protoList10);
      EXPECT_EQ(MetadataKind::Existential, special->getKind());
      EXPECT_EQ(2U, special->Flags.getNumWitnessTables());
      // Compositions of special protocols aren't special.
      EXPECT_EQ(SpecialProtocol::None,
                special->Flags.getSpecialProtocol());
      EXPECT_NE(ExpectedErrorValueWitnesses,
                special->getValueWitnesses());
      EXPECT_EQ(nullptr,
                special->getSuperclassConstraint());
      return special;
    });
}

static ProtocolDescriptor OpaqueProto1 = { "OpaqueProto1", nullptr,
  ProtocolDescriptorFlags().withSwift(true)
                          .withDispatchStrategy(ProtocolDispatchStrategy::Swift)
                          .withClassConstraint(ProtocolClassConstraint::Any)
};
static ProtocolDescriptor OpaqueProto2 = { "OpaqueProto2", nullptr,
  ProtocolDescriptorFlags().withSwift(true)
                          .withDispatchStrategy(ProtocolDispatchStrategy::Swift)
                          .withClassConstraint(ProtocolClassConstraint::Any)
};
static ProtocolDescriptor OpaqueProto3 = { "OpaqueProto3", nullptr,
  ProtocolDescriptorFlags().withSwift(true)
                          .withDispatchStrategy(ProtocolDispatchStrategy::Swift)
                          .withClassConstraint(ProtocolClassConstraint::Any)
};
static ProtocolDescriptor ClassProto1 = { "ClassProto1", nullptr,
  ProtocolDescriptorFlags().withSwift(true)
                          .withDispatchStrategy(ProtocolDispatchStrategy::Swift)
                          .withClassConstraint(ProtocolClassConstraint::Class)
};

TEST(MetadataTest, getExistentialTypeMetadata_opaque) {
  const ProtocolDescriptor *protoList1[] = {
    &OpaqueProto1
  };
  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto ex1 = swift_getExistentialTypeMetadata(ProtocolClassConstraint::Any,
                                                  /*superclass=*/nullptr,
                                                  1, protoList1);
      EXPECT_EQ(MetadataKind::Existential, ex1->getKind());
      EXPECT_EQ(5 * sizeof(void*), ex1->getValueWitnesses()->getSize());
      EXPECT_EQ(alignof(void*), ex1->getValueWitnesses()->getAlignment());
      EXPECT_FALSE(ex1->getValueWitnesses()->isPOD());
      EXPECT_FALSE(ex1->getValueWitnesses()->isBitwiseTakable());
      EXPECT_EQ(nullptr,
                ex1->getSuperclassConstraint());
      return ex1;
    });

  const ProtocolDescriptor *protoList2[] = {
    &OpaqueProto1, &OpaqueProto2
  };
  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto ex2 = swift_getExistentialTypeMetadata(ProtocolClassConstraint::Any,
                                                  /*superclass=*/nullptr,
                                                  2, protoList2);
      EXPECT_EQ(MetadataKind::Existential, ex2->getKind());
      EXPECT_EQ(6 * sizeof(void*), ex2->getValueWitnesses()->getSize());
      EXPECT_EQ(alignof(void*), ex2->getValueWitnesses()->getAlignment());
      EXPECT_FALSE(ex2->getValueWitnesses()->isPOD());
      EXPECT_FALSE(ex2->getValueWitnesses()->isBitwiseTakable());
      EXPECT_EQ(nullptr,
                ex2->getSuperclassConstraint());
      return ex2;
    });

  const ProtocolDescriptor *protoList3[] = {
    &OpaqueProto1, &OpaqueProto2, &OpaqueProto3
  };
  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto ex3 = swift_getExistentialTypeMetadata(ProtocolClassConstraint::Any,
                                                  /*superclass=*/nullptr,
                                                  3, protoList3);
      EXPECT_EQ(MetadataKind::Existential, ex3->getKind());
      EXPECT_EQ(7 * sizeof(void*), ex3->getValueWitnesses()->getSize());
      EXPECT_EQ(alignof(void*), ex3->getValueWitnesses()->getAlignment());
      EXPECT_FALSE(ex3->getValueWitnesses()->isPOD());
      EXPECT_FALSE(ex3->getValueWitnesses()->isBitwiseTakable());
      EXPECT_EQ(nullptr,
                ex3->getSuperclassConstraint());
      return ex3;
    });
}

TEST(MetadataTest, getExistentialTypeMetadata_class) {
  const ProtocolDescriptor *protoList1[] = {
    &ClassProto1
  };
  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto ex1 = swift_getExistentialTypeMetadata(ProtocolClassConstraint::Class,
                                                  /*superclass=*/nullptr,
                                                  1, protoList1);
      EXPECT_EQ(MetadataKind::Existential, ex1->getKind());
      EXPECT_EQ(2 * sizeof(void*), ex1->getValueWitnesses()->getSize());
      EXPECT_EQ(alignof(void*), ex1->getValueWitnesses()->getAlignment());
      EXPECT_FALSE(ex1->getValueWitnesses()->isPOD());
      EXPECT_TRUE(ex1->getValueWitnesses()->isBitwiseTakable());
      EXPECT_EQ(nullptr,
                ex1->getSuperclassConstraint());
      return ex1;
    });

  const ProtocolDescriptor *protoList2[] = {
    &OpaqueProto1, &ClassProto1
  };
  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto ex2 = swift_getExistentialTypeMetadata(ProtocolClassConstraint::Class,
                                                  /*superclass=*/nullptr,
                                                  2, protoList2);
      EXPECT_EQ(MetadataKind::Existential, ex2->getKind());
      EXPECT_EQ(3 * sizeof(void*), ex2->getValueWitnesses()->getSize());
      EXPECT_EQ(alignof(void*), ex2->getValueWitnesses()->getAlignment());
      EXPECT_FALSE(ex2->getValueWitnesses()->isPOD());
      EXPECT_TRUE(ex2->getValueWitnesses()->isBitwiseTakable());
      EXPECT_EQ(nullptr,
                ex2->getSuperclassConstraint());
      return ex2;
    });

  const ProtocolDescriptor *protoList3[] = {
    &OpaqueProto1, &OpaqueProto2, &ClassProto1
  };
  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto ex3 = swift_getExistentialTypeMetadata(ProtocolClassConstraint::Class,
                                                  /*superclass=*/nullptr,
                                                  3, protoList3);
      EXPECT_EQ(MetadataKind::Existential, ex3->getKind());
      EXPECT_EQ(4 * sizeof(void*), ex3->getValueWitnesses()->getSize());
      EXPECT_EQ(alignof(void*), ex3->getValueWitnesses()->getAlignment());
      EXPECT_FALSE(ex3->getValueWitnesses()->isPOD());
      EXPECT_TRUE(ex3->getValueWitnesses()->isBitwiseTakable());
      EXPECT_EQ(nullptr,
                ex3->getSuperclassConstraint());
      return ex3;
    });
}

TEST(MetadataTest, getExistentialTypeMetadata_subclass) {
  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      const ProtocolDescriptor *protoList1[] = {
        &OpaqueProto1
      };
      auto ex1 = swift_getExistentialTypeMetadata(ProtocolClassConstraint::Class,
                                                  /*superclass=*/&MetadataTest2,
                                                  1, protoList1);
      EXPECT_EQ(MetadataKind::Existential, ex1->getKind());
      EXPECT_EQ(2 * sizeof(void*), ex1->getValueWitnesses()->getSize());
      EXPECT_EQ(alignof(void*), ex1->getValueWitnesses()->getAlignment());
      EXPECT_FALSE(ex1->getValueWitnesses()->isPOD());
      EXPECT_TRUE(ex1->getValueWitnesses()->isBitwiseTakable());
      EXPECT_EQ(ProtocolClassConstraint::Class,
                ex1->Flags.getClassConstraint());
      EXPECT_EQ(1U, ex1->Protocols.NumProtocols);
      EXPECT_EQ(&OpaqueProto1, ex1->Protocols[0]);
      EXPECT_EQ(&MetadataTest2, ex1->getSuperclassConstraint());
      return ex1;
    });


  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      const ProtocolDescriptor *protoList2[] = {
        &OpaqueProto1,
        &ClassProto1
      };
      auto ex2 = swift_getExistentialTypeMetadata(ProtocolClassConstraint::Class,
                                                  /*superclass=*/&MetadataTest2,
                                                  2, protoList2);
      EXPECT_EQ(MetadataKind::Existential, ex2->getKind());
      EXPECT_EQ(3 * sizeof(void*), ex2->getValueWitnesses()->getSize());
      EXPECT_EQ(alignof(void*), ex2->getValueWitnesses()->getAlignment());
      EXPECT_FALSE(ex2->getValueWitnesses()->isPOD());
      EXPECT_TRUE(ex2->getValueWitnesses()->isBitwiseTakable());
      EXPECT_EQ(ProtocolClassConstraint::Class,
                ex2->Flags.getClassConstraint());
      EXPECT_EQ(2U, ex2->Protocols.NumProtocols);
      EXPECT_TRUE(ex2->Protocols[0] == &OpaqueProto1 &&
                  ex2->Protocols[1] == &ClassProto1);
      EXPECT_EQ(&MetadataTest2, ex2->getSuperclassConstraint());
      return ex2;
    });
}

namespace swift {
  void installCommonValueWitnesses(const TypeLayout &layout,
                                   ValueWitnessTable *vwtable);
} // namespace swift


TEST(MetadataTest, installCommonValueWitnesses_pod_indirect) {
  ValueWitnessTable testTable;
  FullMetadata<Metadata> testMetadata{{&testTable}, {MetadataKind::Opaque}};

  // rdar://problem/21375421 - pod_indirect_initializeBufferWithTakeOfBuffer
  // should move ownership of a fixed-size buffer.

  testTable.size = sizeof(ValueBuffer) + 1;
  testTable.flags = ValueWitnessFlags()
    .withAlignment(alignof(ValueBuffer))
    .withPOD(true)
    .withBitwiseTakable(true)
    .withInlineStorage(false);
  testTable.stride = sizeof(ValueBuffer) + alignof(ValueBuffer);

  installCommonValueWitnesses(*testTable.getTypeLayout(), &testTable);

  // Replace allocateBuffer and destroyBuffer with logging versions.
  struct {
    ValueBuffer buffer;
    uintptr_t canary;
  } buf1{{}, 0x5A5A5A5AU}, buf2{{}, 0xA5A5A5A5U};
  testMetadata.allocateBoxForExistentialIn(&buf1.buffer);

  testTable.initializeBufferWithTakeOfBuffer(&buf2.buffer, &buf1.buffer,
                                             &testMetadata);

  // The existential's box reference should be copied.
  EXPECT_EQ(buf1.buffer.PrivateData[0], buf2.buffer.PrivateData[0]);

  // Ownership of the box should have been transferred.
  auto *reference = reinterpret_cast<HeapObject *>(buf2.buffer.PrivateData[0]);
  EXPECT_TRUE(swift_isUniquelyReferencedOrPinned_nonNull_native(reference));

  EXPECT_EQ(buf1.canary, (uintptr_t)0x5A5A5A5AU);
  EXPECT_EQ(buf2.canary, (uintptr_t)0xA5A5A5A5U);

  // Release the buffer.
  swift_release(reference);
}

// We cannot construct RelativeDirectPointer instances, so define
// a "shadow" struct for that purpose
struct GenericWitnessTableStorage {
  uint16_t WitnessTableSizeInWords;
  uint16_t WitnessTablePrivateSizeInWords;
  int32_t Protocol;
  int32_t Pattern;
  int32_t ResilientWitnesses;
  int32_t Instantiator;
  int32_t PrivateData;
};

template<typename T>
static void initializeRelativePointer(int32_t *ptr, T value) {
  *ptr = (int32_t)(value == nullptr ? 0 : (uintptr_t) value - (uintptr_t) ptr);
}

// Tests for resilient witness table instantiation, with runtime-provided
// default requirements

static void witnessTableInstantiator(WitnessTable *instantiatedTable,
                                     const Metadata *type,
                                     void **const *instantiationArgs) {
  EXPECT_EQ(type, nullptr);

  EXPECT_EQ(((void **) instantiatedTable)[2], (void*) 123);
  EXPECT_EQ(((void **) instantiatedTable)[3], (void*) 234);

  // The last witness is computed dynamically at instantiation time.
  ((void **) instantiatedTable)[4] = (void *) 345;

  auto conditionalTables =
      reinterpret_cast<const WitnessTable *const *>(instantiationArgs);

  EXPECT_EQ(conditionalTables[0], (void *)678);
  ((void **)instantiatedTable)[-1] = (void *)conditionalTables[0];
}

static void fakeDefaultWitness1() {}
static void fakeDefaultWitness2() {}

static void fakeRequirement1() {}
static void fakeRequirement2() {}
static void fakeRequirement3() {}
static void fakeRequirement4() {}
static void fakeRequirement5() {}

// A mock protocol descriptor with some default witnesses at the end.
//
// Note: It is not standards-compliant to compare function pointers for
// equality, so we just use fake addresses instead.
struct TestProtocol {
  ProtocolDescriptor descriptor;
  union {
    ProtocolRequirement requirements[6];
  };

  TestProtocol()
    : descriptor("TestProtocol",
                 nullptr,
                 ProtocolDescriptorFlags().withResilient(true)) {
    descriptor.NumRequirements = 6;
    initializeRelativePointer(
      (int32_t *) &descriptor.Requirements,
      requirements);

    using Flags = ProtocolRequirementFlags;

    requirements[0].Flags = Flags(Flags::Kind::BaseProtocol);
    requirements[1].Flags = Flags(Flags::Kind::Method);
    initializeRelativePointer(
      (int32_t *) &requirements[1].Function,
      fakeRequirement1);
    requirements[1].DefaultImplementation = nullptr;
    requirements[2].Flags = Flags(Flags::Kind::Method);
    initializeRelativePointer(
      (int32_t *) &requirements[2].Function,
      fakeRequirement2);
    requirements[2].DefaultImplementation = nullptr;
    requirements[3].Flags = Flags(Flags::Kind::Method);
    initializeRelativePointer(
      (int32_t *) &requirements[3].Function,
      fakeRequirement3);
    requirements[3].DefaultImplementation = nullptr;
    requirements[4].Flags = Flags(Flags::Kind::Method);
    initializeRelativePointer(
      (int32_t *) &requirements[4].Function,
      fakeRequirement4);
    initializeRelativePointer(
      (int32_t *) &requirements[4].DefaultImplementation,
      fakeDefaultWitness1);
    requirements[5].Flags = Flags(Flags::Kind::Method);
    initializeRelativePointer(
      (int32_t *) &requirements[5].Function,
      fakeRequirement5);
    initializeRelativePointer(
      (int32_t *) &requirements[5].DefaultImplementation,
      fakeDefaultWitness2);
  }
};

// All of these have to be global to relative reference each other, and
// the instantiator function.
static TestProtocol testProtocol;
static GenericWitnessTableStorage tableStorage1;
static GenericWitnessTableStorage tableStorage2;
static GenericWitnessTable::PrivateDataType tablePrivateData1;
GenericWitnessTable::PrivateDataType tablePrivateData2;

const void *witnesses[] = {
  (void *) 0,   // protocol descriptor
  (void *) 777, // base protocol reference
  (void *) 123,
  (void *) 234,
  (void *) 0,   // filled in by instantiator function
  (void *) 456,
  (void *) 567
};

WitnessTable *conditionalTablesBuffer[] = {(WitnessTable *)678};

TEST(WitnessTableTest, getGenericWitnessTable) {
  EXPECT_EQ(sizeof(GenericWitnessTableStorage), sizeof(GenericWitnessTable));

  // Conformance provides all requirements, and we don't have an
  // instantiator, so we can just return the pattern.
  {
    tableStorage1.WitnessTableSizeInWords = 7;
    tableStorage1.WitnessTablePrivateSizeInWords = 0;
    initializeRelativePointer(&tableStorage1.Protocol, &testProtocol.descriptor);
    initializeRelativePointer(&tableStorage1.Pattern, witnesses);
    initializeRelativePointer(&tableStorage1.Instantiator, nullptr);
    initializeRelativePointer(&tableStorage1.PrivateData, &tablePrivateData1);

    GenericWitnessTable *table = reinterpret_cast<GenericWitnessTable *>(
        &tableStorage1);

    RaceTest_ExpectEqual<const WitnessTable *>(
      [&]() -> const WitnessTable * {
        const WitnessTable *instantiatedTable =
            swift_getGenericWitnessTable(table, nullptr, nullptr);

        EXPECT_EQ(instantiatedTable, table->Pattern.get());
        return instantiatedTable;
      });
  }

  // Conformance provides all requirements, but we have private storage
  // and an initializer, so we must instantiate.
  {
    tableStorage2.WitnessTableSizeInWords = 7;
    tableStorage2.WitnessTablePrivateSizeInWords = 1 + 1;
    initializeRelativePointer(&tableStorage2.Protocol, &testProtocol.descriptor);
    initializeRelativePointer(&tableStorage2.Pattern, witnesses);
    initializeRelativePointer(&tableStorage2.Instantiator,
                              (const void *) witnessTableInstantiator);
    initializeRelativePointer(&tableStorage2.PrivateData, &tablePrivateData2);

    GenericWitnessTable *table = reinterpret_cast<GenericWitnessTable *>(
        &tableStorage2);

    RaceTest_ExpectEqual<const WitnessTable *>(
      [&]() -> const WitnessTable * {
        const WitnessTable *instantiatedTable = swift_getGenericWitnessTable(
            table, nullptr, (void ***)conditionalTablesBuffer);

        EXPECT_NE(instantiatedTable, table->Pattern.get());

        EXPECT_EQ(reinterpret_cast<void * const *>(instantiatedTable)[-2],
                  reinterpret_cast<void *>(0));
        EXPECT_EQ(reinterpret_cast<void * const *>(instantiatedTable)[-1],
                  reinterpret_cast<void *>(678));

        EXPECT_EQ(reinterpret_cast<void * const *>(instantiatedTable)[1],
                  reinterpret_cast<void *>(777));
        EXPECT_EQ(reinterpret_cast<void * const *>(instantiatedTable)[2],
                  reinterpret_cast<void *>(123));
        EXPECT_EQ(reinterpret_cast<void * const *>(instantiatedTable)[3],
                  reinterpret_cast<void *>(234));
        EXPECT_EQ(reinterpret_cast<void * const *>(instantiatedTable)[4],
                  reinterpret_cast<void *>(345));
        EXPECT_EQ(reinterpret_cast<void * const *>(instantiatedTable)[5],
                  reinterpret_cast<void *>(456));
        EXPECT_EQ(reinterpret_cast<void * const *>(instantiatedTable)[6],
                  reinterpret_cast<void *>(567));

        return instantiatedTable;
      });
  }
}

static GenericWitnessTableStorage tableStorage3;
static GenericWitnessTable::PrivateDataType tablePrivateData3;

static void *gotFakeRequirement1[] = { (void *) fakeRequirement1 };
static void *gotFakeRequirement2[] = { (void *) fakeRequirement2 };
static void *gotFakeRequirement3[] = { (void *) fakeRequirement3 };
static void *gotFakeRequirement5[] = { (void *) fakeRequirement5 };

static void fakeWitness1() {}
static void fakeWitness2() {}
static void fakeWitness3() {}
static void fakeWitness5() {}

struct ResilientWitnessStorage {
  int32_t Requirement;
  int32_t Witness;
};

struct ResilientWitnessTableStorage {
  int32_t numWitnesses;
  ResilientWitnessStorage witnesses[4];

  ResilientWitnessTableStorage() {
    // Note the funny order -- we want to make sure it's order-independent.
    numWitnesses = 4;

    initializeRelativePointer(
      &witnesses[0].Requirement,
      &gotFakeRequirement3);
    initializeRelativePointer(
      &witnesses[0].Witness,
      fakeWitness3);

    initializeRelativePointer(
      &witnesses[1].Requirement,
      &gotFakeRequirement2);
    initializeRelativePointer(
      &witnesses[1].Witness,
      fakeWitness2);

    initializeRelativePointer(
      &witnesses[2].Requirement,
      &gotFakeRequirement1);
    initializeRelativePointer(
      &witnesses[2].Witness,
      fakeWitness1);

    initializeRelativePointer(
      &witnesses[3].Requirement,
      &gotFakeRequirement5);
    initializeRelativePointer(
      &witnesses[3].Witness,
      fakeWitness5);
  }
};

static ResilientWitnessTableStorage resilientWitnesses;

TEST(WitnessTableTest, ResilientWitnessTable) {
  ResilientWitnessTableStorage wtable;

  // Conformance needs both default requirements to be filled in
  {
    tableStorage3.WitnessTableSizeInWords = 2;
    initializeRelativePointer(&tableStorage3.Protocol, &testProtocol.descriptor);
    initializeRelativePointer(&tableStorage3.Pattern, witnesses);
    initializeRelativePointer(&tableStorage3.ResilientWitnesses,
                              &resilientWitnesses);
    initializeRelativePointer(&tableStorage3.PrivateData, &tablePrivateData3);

    GenericWitnessTable *table = reinterpret_cast<GenericWitnessTable *>(
        &tableStorage3);

    RaceTest_ExpectEqual<const WitnessTable *>(
      [&]() -> const WitnessTable * {
        const WitnessTable *instantiatedTable = swift_getGenericWitnessTable(
            table, nullptr, (void ***)conditionalTablesBuffer);

        EXPECT_NE(instantiatedTable, table->Pattern.get());

        // From the pattern
        EXPECT_EQ(reinterpret_cast<void * const *>(instantiatedTable)[1],
                  reinterpret_cast<void *>(777));

        // The rest come from the order-independent resilient witness
        // descriptors
        EXPECT_EQ(reinterpret_cast<void * const *>(instantiatedTable)[2],
                  reinterpret_cast<void *>(fakeWitness1));
        EXPECT_EQ(reinterpret_cast<void * const *>(instantiatedTable)[3],
                  reinterpret_cast<void *>(fakeWitness2));
        EXPECT_EQ(reinterpret_cast<void * const *>(instantiatedTable)[4],
                  reinterpret_cast<void *>(fakeWitness3));
        EXPECT_EQ(reinterpret_cast<void * const *>(instantiatedTable)[5],
                  reinterpret_cast<void *>(fakeDefaultWitness1));
        EXPECT_EQ(reinterpret_cast<void * const *>(instantiatedTable)[6],
                  reinterpret_cast<void *>(fakeWitness5));

        return instantiatedTable;
      });
  }
}

static void initialize_pod_witness_table(ValueWitnessTable &testTable) {
  testTable.size = sizeof(ValueBuffer);
  testTable.flags = ValueWitnessFlags()
    .withAlignment(alignof(ValueBuffer))
    .withPOD(true)
    .withBitwiseTakable(true)
    .withInlineStorage(true);
  testTable.stride = sizeof(ValueBuffer);
  installCommonValueWitnesses(*testTable.getTypeLayout(), &testTable);
}

TEST(TestOpaqueExistentialBox, test_assignWithCopy_pod) {
  auto any =
      swift_getExistentialTypeMetadata(ProtocolClassConstraint::Any,
                                       /*superclass=*/nullptr, 0, nullptr);
  auto anyVWT = any->getValueWitnesses();

  ValueWitnessTable testTable;
  initialize_pod_witness_table(testTable);
  FullOpaqueMetadata testMetadata = {{&testTable}, {{MetadataKind::Opaque}}};
  Metadata *metadata = &testMetadata.base;

  ValueWitnessTable testTable2;
  initialize_pod_witness_table(testTable2);
  FullOpaqueMetadata testMetadata2 = {{&testTable2}, {{MetadataKind::Opaque}}};
  Metadata *metadata2 = &testMetadata2.base;

  void *zeroPtr = nullptr;
  void *otherPtr = &zeroPtr;
  struct {
    ValueBuffer buffer;
    Metadata *type;
    uintptr_t canary;
  } existBox{{{zeroPtr, zeroPtr, zeroPtr}}, metadata, 0x5A5A5A5AU},
    existBox2{{{otherPtr, otherPtr, zeroPtr}}, metadata2, 0xB5A5A5A5U};

  anyVWT->assignWithCopy(reinterpret_cast<OpaqueValue *>(&existBox),
                         reinterpret_cast<OpaqueValue *>(&existBox2), any);
  EXPECT_EQ(existBox.type, metadata2);
  EXPECT_EQ(existBox.canary, 0x5A5A5A5AU);
  EXPECT_EQ(existBox.buffer.PrivateData[0], otherPtr);
  EXPECT_EQ(existBox.buffer.PrivateData[1], otherPtr);
  EXPECT_EQ(existBox.buffer.PrivateData[2], zeroPtr);
}

TEST(TestOpaqueExistentialBox, test_assignWithTake_pod) {
  auto any =
      swift_getExistentialTypeMetadata(ProtocolClassConstraint::Any,
                                       /*superclass=*/nullptr, 0, nullptr);
  auto anyVWT = any->getValueWitnesses();

  ValueWitnessTable testTable;
  initialize_pod_witness_table(testTable);
  FullOpaqueMetadata testMetadata = {{&testTable}, {{MetadataKind::Opaque}}};
  Metadata *metadata = &testMetadata.base;

  ValueWitnessTable testTable2;
  initialize_pod_witness_table(testTable2);
  FullOpaqueMetadata testMetadata2 = {{&testTable2}, {{MetadataKind::Opaque}}};
  Metadata *metadata2 = &testMetadata2.base;

  void *zeroPtr = nullptr;
  void *otherPtr = &zeroPtr;
  struct {
    ValueBuffer buffer;
    Metadata *type;
    uintptr_t canary;
  } existBox{{{zeroPtr, zeroPtr, zeroPtr}}, metadata, 0x5A5A5A5AU},
    existBox2{{{otherPtr, otherPtr, zeroPtr}}, metadata2, 0xB5A5A5A5U};

  anyVWT->assignWithTake(reinterpret_cast<OpaqueValue *>(&existBox),
                         reinterpret_cast<OpaqueValue *>(&existBox2), any);
  EXPECT_EQ(existBox.type, metadata2);
  EXPECT_EQ(existBox.canary, 0x5A5A5A5AU);
  EXPECT_EQ(existBox.buffer.PrivateData[0], otherPtr);
  EXPECT_EQ(existBox.buffer.PrivateData[1], otherPtr);
  EXPECT_EQ(existBox.buffer.PrivateData[2], zeroPtr);
}

static void initialize_indirect_witness_table(ValueWitnessTable &testTable) {
  testTable.size = sizeof(ValueBuffer) + 1;
  testTable.flags = ValueWitnessFlags()
    .withAlignment(alignof(ValueBuffer))
    .withPOD(true)
    .withBitwiseTakable(true)
    .withInlineStorage(false);
  testTable.stride = sizeof(ValueBuffer) + 1;
  installCommonValueWitnesses(*testTable.getTypeLayout(), &testTable);
}

TEST(TestOpaqueExistentialBox, test_assignWithCopy_indirect_indirect) {
  auto any =
      swift_getExistentialTypeMetadata(ProtocolClassConstraint::Any,
                                       /*superclass=*/nullptr, 0, nullptr);
  auto anyVWT = any->getValueWitnesses();

  ValueWitnessTable testTable;
  initialize_indirect_witness_table(testTable);
  FullOpaqueMetadata testMetadata = {{&testTable}, {{MetadataKind::Opaque}}};
  Metadata *metadata = &testMetadata.base;

  ValueWitnessTable testTable2;
  initialize_indirect_witness_table(testTable2);
  FullOpaqueMetadata testMetadata2 = {{&testTable2}, {{MetadataKind::Opaque}}};
  Metadata *metadata2 = &testMetadata2.base;

  auto refAndObjectAddr = BoxPair(swift_allocBox(metadata));
  swift_retain(refAndObjectAddr.object);
  auto refAndObjectAddr2 = BoxPair(swift_allocBox(metadata2));
  struct {
    ValueBuffer buffer;
    Metadata *type;
    uintptr_t canary;
  } existBox{{{refAndObjectAddr.object, nullptr, nullptr}}, metadata, 0x5A5A5A5AU},
    existBox2{{{refAndObjectAddr2.object, nullptr, nullptr}}, metadata2, 0xB5A5A5A5U};

  anyVWT->assignWithCopy(reinterpret_cast<OpaqueValue *>(&existBox),
                         reinterpret_cast<OpaqueValue *>(&existBox2), any);

  EXPECT_EQ(existBox.type, metadata2);
  EXPECT_EQ(existBox.canary, 0x5A5A5A5AU);
  EXPECT_EQ(existBox.buffer.PrivateData[0], refAndObjectAddr2.object);
  EXPECT_EQ(swift_retainCount(refAndObjectAddr.object), 1u);
  EXPECT_EQ(swift_retainCount(refAndObjectAddr2.object), 2u);
}

TEST(TestOpaqueExistentialBox, test_assignWithTake_indirect_indirect) {
  auto any =
      swift_getExistentialTypeMetadata(ProtocolClassConstraint::Any,
                                       /*superclass=*/nullptr, 0, nullptr);
  auto anyVWT = any->getValueWitnesses();

  ValueWitnessTable testTable;
  initialize_indirect_witness_table(testTable);
  FullOpaqueMetadata testMetadata = {{&testTable}, {{MetadataKind::Opaque}}};
  Metadata *metadata = &testMetadata.base;

  ValueWitnessTable testTable2;
  initialize_indirect_witness_table(testTable2);
  FullOpaqueMetadata testMetadata2 = {{&testTable2}, {{MetadataKind::Opaque}}};
  Metadata *metadata2 = &testMetadata2.base;

  auto refAndObjectAddr = BoxPair(swift_allocBox(metadata));
  swift_retain(refAndObjectAddr.object);
  auto refAndObjectAddr2 = BoxPair(swift_allocBox(metadata2));
  struct {
    ValueBuffer buffer;
    Metadata *type;
    uintptr_t canary;
  } existBox{{{refAndObjectAddr.object, nullptr, nullptr}}, metadata, 0x5A5A5A5AU},
    existBox2{{{refAndObjectAddr2.object, nullptr, nullptr}}, metadata2, 0xB5A5A5A5U};

  anyVWT->assignWithTake(reinterpret_cast<OpaqueValue *>(&existBox),
                         reinterpret_cast<OpaqueValue *>(&existBox2), any);

  EXPECT_EQ(existBox.type, metadata2);
  EXPECT_EQ(existBox.canary, 0x5A5A5A5AU);
  EXPECT_EQ(existBox.buffer.PrivateData[0], refAndObjectAddr2.object);
  EXPECT_EQ(swift_retainCount(refAndObjectAddr.object), 1u);
  EXPECT_EQ(swift_retainCount(refAndObjectAddr2.object), 1u);
}

TEST(TestOpaqueExistentialBox, test_assignWithCopy_pod_indirect) {
  auto any =
      swift_getExistentialTypeMetadata(ProtocolClassConstraint::Any,
                                       /*superclass=*/nullptr, 0, nullptr);
  auto anyVWT = any->getValueWitnesses();

  ValueWitnessTable testTable;
  initialize_pod_witness_table(testTable);
  FullOpaqueMetadata testMetadata = {{&testTable}, {{MetadataKind::Opaque}}};
  Metadata *metadata = &testMetadata.base;

  ValueWitnessTable testTable2;
  initialize_indirect_witness_table(testTable2);
  FullOpaqueMetadata testMetadata2 = {{&testTable2}, {{MetadataKind::Opaque}}};
  Metadata *metadata2 = &testMetadata2.base;

  auto refAndObjectAddr2 = BoxPair(swift_allocBox(metadata2));
  struct {
    ValueBuffer buffer;
    Metadata *type;
    uintptr_t canary;
  } existBox{{{nullptr, nullptr, nullptr}}, metadata, 0x5A5A5A5AU},
    existBox2{{{refAndObjectAddr2.object, nullptr, nullptr}}, metadata2, 0xB5A5A5A5U};

  anyVWT->assignWithCopy(reinterpret_cast<OpaqueValue *>(&existBox),
                         reinterpret_cast<OpaqueValue *>(&existBox2), any);

  EXPECT_EQ(existBox.type, metadata2);
  EXPECT_EQ(existBox.canary, 0x5A5A5A5AU);
  EXPECT_EQ(existBox.buffer.PrivateData[0], refAndObjectAddr2.object);
  EXPECT_EQ(swift_retainCount(refAndObjectAddr2.object), 2u);
}

TEST(TestOpaqueExistentialBox, test_assignWithTake_pod_indirect) {
  auto any =
      swift_getExistentialTypeMetadata(ProtocolClassConstraint::Any,
                                       /*superclass=*/nullptr, 0, nullptr);
  auto anyVWT = any->getValueWitnesses();

  ValueWitnessTable testTable;
  initialize_pod_witness_table(testTable);
  FullOpaqueMetadata testMetadata = {{&testTable}, {{MetadataKind::Opaque}}};
  Metadata *metadata = &testMetadata.base;

  ValueWitnessTable testTable2;
  initialize_indirect_witness_table(testTable2);
  FullOpaqueMetadata testMetadata2 = {{&testTable2}, {{MetadataKind::Opaque}}};
  Metadata *metadata2 = &testMetadata2.base;

  auto refAndObjectAddr2 = BoxPair(swift_allocBox(metadata2));
  struct {
    ValueBuffer buffer;
    Metadata *type;
    uintptr_t canary;
  } existBox{{{nullptr, nullptr, nullptr}}, metadata, 0x5A5A5A5AU},
    existBox2{{{refAndObjectAddr2.object, nullptr, nullptr}}, metadata2, 0xB5A5A5A5U};

  anyVWT->assignWithTake(reinterpret_cast<OpaqueValue *>(&existBox),
                         reinterpret_cast<OpaqueValue *>(&existBox2), any);

  EXPECT_EQ(existBox.type, metadata2);
  EXPECT_EQ(existBox.canary, 0x5A5A5A5AU);
  EXPECT_EQ(existBox.buffer.PrivateData[0], refAndObjectAddr2.object);
  EXPECT_EQ(swift_retainCount(refAndObjectAddr2.object), 1u);
}

TEST(TestOpaqueExistentialBox, test_assignWithCopy_indirect_pod) {
  auto any =
      swift_getExistentialTypeMetadata(ProtocolClassConstraint::Any,
                                       /*superclass=*/nullptr, 0, nullptr);
  auto anyVWT = any->getValueWitnesses();

  ValueWitnessTable testTable;
  initialize_pod_witness_table(testTable);
  FullOpaqueMetadata testMetadata = {{&testTable}, {{MetadataKind::Opaque}}};
  Metadata *metadata = &testMetadata.base;

  ValueWitnessTable testTable2;
  initialize_indirect_witness_table(testTable2);
  FullOpaqueMetadata testMetadata2 = {{&testTable2}, {{MetadataKind::Opaque}}};
  Metadata *metadata2 = &testMetadata2.base;

  auto refAndObjectAddr2 = BoxPair(swift_allocBox(metadata2));
  void *someAddr = &anyVWT;
  swift_retain(refAndObjectAddr2.object);
  struct {
    ValueBuffer buffer;
    Metadata *type;
    uintptr_t canary;
  } existBox2{{{someAddr, nullptr, someAddr}}, metadata, 0x5A5A5A5AU},
    existBox{{{refAndObjectAddr2.object, nullptr, nullptr}}, metadata2, 0xB5A5A5A5U};

  anyVWT->assignWithCopy(reinterpret_cast<OpaqueValue *>(&existBox),
                         reinterpret_cast<OpaqueValue *>(&existBox2), any);

  EXPECT_EQ(existBox.type, metadata);
  EXPECT_EQ(existBox.canary, 0xB5A5A5A5U);
  EXPECT_EQ(existBox.buffer.PrivateData[0], someAddr);
  EXPECT_EQ(existBox.buffer.PrivateData[1], nullptr);
  EXPECT_EQ(existBox.buffer.PrivateData[2], someAddr);
  EXPECT_EQ(swift_retainCount(refAndObjectAddr2.object), 1u);
}

TEST(TestOpaqueExistentialBox, test_assignWithTake_indirect_pod) {
  auto any =
      swift_getExistentialTypeMetadata(ProtocolClassConstraint::Any,
                                       /*superclass=*/nullptr, 0, nullptr);
  auto anyVWT = any->getValueWitnesses();

  ValueWitnessTable testTable;
  initialize_pod_witness_table(testTable);
  FullOpaqueMetadata testMetadata = {{&testTable}, {{MetadataKind::Opaque}}};
  Metadata *metadata = &testMetadata.base;

  ValueWitnessTable testTable2;
  initialize_indirect_witness_table(testTable2);
  FullOpaqueMetadata testMetadata2 = {{&testTable2}, {{MetadataKind::Opaque}}};
  Metadata *metadata2 = &testMetadata2.base;

  auto refAndObjectAddr2 = BoxPair(swift_allocBox(metadata2));
  void *someAddr = &anyVWT;
  swift_retain(refAndObjectAddr2.object);
  struct {
    ValueBuffer buffer;
    Metadata *type;
    uintptr_t canary;
  } existBox2{{{someAddr, nullptr, someAddr}}, metadata, 0x5A5A5A5AU},
    existBox{{{refAndObjectAddr2.object, nullptr, nullptr}}, metadata2, 0xB5A5A5A5U};

  anyVWT->assignWithTake(reinterpret_cast<OpaqueValue *>(&existBox),
                         reinterpret_cast<OpaqueValue *>(&existBox2), any);

  EXPECT_EQ(existBox.type, metadata);
  EXPECT_EQ(existBox.canary, 0xB5A5A5A5U);
  EXPECT_EQ(existBox.buffer.PrivateData[0], someAddr);
  EXPECT_EQ(existBox.buffer.PrivateData[1], nullptr);
  EXPECT_EQ(existBox.buffer.PrivateData[2], someAddr);
  EXPECT_EQ(swift_retainCount(refAndObjectAddr2.object), 1u);
}

TEST(TestOpaqueExistentialBox, test_initWithCopy_pod) {
  auto any =
      swift_getExistentialTypeMetadata(ProtocolClassConstraint::Any,
                                       /*superclass=*/nullptr, 0, nullptr);
  auto anyVWT = any->getValueWitnesses();

  ValueWitnessTable testTable;
  initialize_pod_witness_table(testTable);
  FullOpaqueMetadata testMetadata = {{&testTable}, {{MetadataKind::Opaque}}};
  Metadata *metadata = &testMetadata.base;

  ValueWitnessTable testTable2;
  initialize_pod_witness_table(testTable2);
  FullOpaqueMetadata testMetadata2 = {{&testTable2}, {{MetadataKind::Opaque}}};
  Metadata *metadata2 = &testMetadata2.base;

  void *zeroPtr = nullptr;
  void *otherPtr = &zeroPtr;
  struct {
    ValueBuffer buffer;
    Metadata *type;
    uintptr_t canary;
  } existBox{{{zeroPtr, zeroPtr, zeroPtr}}, metadata, 0x5A5A5A5AU},
    existBox2{{{otherPtr, otherPtr, zeroPtr}}, metadata2, 0xB5A5A5A5U};

  anyVWT->initializeWithCopy(reinterpret_cast<OpaqueValue *>(&existBox),
                       reinterpret_cast<OpaqueValue *>(&existBox2), any);
  EXPECT_EQ(existBox.type, metadata2);
  EXPECT_EQ(existBox.canary, 0x5A5A5A5AU);
  EXPECT_EQ(existBox.buffer.PrivateData[0], otherPtr);
  EXPECT_EQ(existBox.buffer.PrivateData[1], otherPtr);
  EXPECT_EQ(existBox.buffer.PrivateData[2], zeroPtr);
}

TEST(TestOpaqueExistentialBox, test_initWithTake_pod) {
  auto any =
      swift_getExistentialTypeMetadata(ProtocolClassConstraint::Any,
                                       /*superclass=*/nullptr, 0, nullptr);
  auto anyVWT = any->getValueWitnesses();

  ValueWitnessTable testTable;
  initialize_pod_witness_table(testTable);
  FullOpaqueMetadata testMetadata = {{&testTable}, {{MetadataKind::Opaque}}};
  Metadata *metadata = &testMetadata.base;

  ValueWitnessTable testTable2;
  initialize_pod_witness_table(testTable2);
  FullOpaqueMetadata testMetadata2 = {{&testTable2}, {{MetadataKind::Opaque}}};
  Metadata *metadata2 = &testMetadata2.base;

  void *zeroPtr = nullptr;
  void *otherPtr = &zeroPtr;
  struct {
    ValueBuffer buffer;
    Metadata *type;
    uintptr_t canary;
  } existBox{{{zeroPtr, zeroPtr, zeroPtr}}, metadata, 0x5A5A5A5AU},
    existBox2{{{otherPtr, otherPtr, zeroPtr}}, metadata2, 0xB5A5A5A5U};

  anyVWT->initializeWithTake(reinterpret_cast<OpaqueValue *>(&existBox),
                       reinterpret_cast<OpaqueValue *>(&existBox2), any);
  EXPECT_EQ(existBox.type, metadata2);
  EXPECT_EQ(existBox.canary, 0x5A5A5A5AU);
  EXPECT_EQ(existBox.buffer.PrivateData[0], otherPtr);
  EXPECT_EQ(existBox.buffer.PrivateData[1], otherPtr);
  EXPECT_EQ(existBox.buffer.PrivateData[2], zeroPtr);
}

TEST(TestOpaqueExistentialBox, test_initWithCopy_indirect) {
  auto any =
      swift_getExistentialTypeMetadata(ProtocolClassConstraint::Any,
                                       /*superclass=*/nullptr, 0, nullptr);
  auto anyVWT = any->getValueWitnesses();

  ValueWitnessTable testTable;
  initialize_pod_witness_table(testTable);
  FullOpaqueMetadata testMetadata = {{&testTable}, {{MetadataKind::Opaque}}};
  Metadata *metadata = &testMetadata.base;

  ValueWitnessTable testTable2;
  initialize_indirect_witness_table(testTable2);
  FullOpaqueMetadata testMetadata2 = {{&testTable2}, {{MetadataKind::Opaque}}};
  Metadata *metadata2 = &testMetadata2.base;

  auto refAndObjectAddr2 = BoxPair(swift_allocBox(metadata2));
  struct {
    ValueBuffer buffer;
    Metadata *type;
    uintptr_t canary;
  } existBox{{{nullptr, nullptr, nullptr}}, metadata, 0x5A5A5A5AU},
    existBox2{{{refAndObjectAddr2.object, nullptr, nullptr}}, metadata2, 0xB5A5A5A5U};

  anyVWT->initializeWithCopy(reinterpret_cast<OpaqueValue *>(&existBox),
                             reinterpret_cast<OpaqueValue *>(&existBox2), any);

  EXPECT_EQ(existBox.type, metadata2);
  EXPECT_EQ(existBox.canary, 0x5A5A5A5AU);
  EXPECT_EQ(existBox.buffer.PrivateData[0], refAndObjectAddr2.object);
  EXPECT_EQ(swift_retainCount(refAndObjectAddr2.object), 2u);
}

TEST(TestOpaqueExistentialBox, test_initWithTake_indirect) {
  auto any =
      swift_getExistentialTypeMetadata(ProtocolClassConstraint::Any,
                                       /*superclass=*/nullptr, 0, nullptr);
  auto anyVWT = any->getValueWitnesses();

  ValueWitnessTable testTable;
  initialize_pod_witness_table(testTable);
  FullOpaqueMetadata testMetadata = {{&testTable}, {{MetadataKind::Opaque}}};
  Metadata *metadata = &testMetadata.base;

  ValueWitnessTable testTable2;
  initialize_indirect_witness_table(testTable2);
  FullOpaqueMetadata testMetadata2 = {{&testTable2}, {{MetadataKind::Opaque}}};
  Metadata *metadata2 = &testMetadata2.base;

  auto refAndObjectAddr2 = BoxPair(swift_allocBox(metadata2));
  struct {
    ValueBuffer buffer;
    Metadata *type;
    uintptr_t canary;
  } existBox{{{nullptr, nullptr, nullptr}}, metadata, 0x5A5A5A5AU},
    existBox2{{{refAndObjectAddr2.object, nullptr, nullptr}}, metadata2, 0xB5A5A5A5U};

  anyVWT->initializeWithTake(reinterpret_cast<OpaqueValue *>(&existBox),
                             reinterpret_cast<OpaqueValue *>(&existBox2), any);

  EXPECT_EQ(existBox.type, metadata2);
  EXPECT_EQ(existBox.canary, 0x5A5A5A5AU);
  EXPECT_EQ(existBox.buffer.PrivateData[0], refAndObjectAddr2.object);
  EXPECT_EQ(swift_retainCount(refAndObjectAddr2.object), 1u);
}
