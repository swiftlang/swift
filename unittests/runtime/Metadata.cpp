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
#include "swift/Runtime/Concurrent.h"
#include "gtest/gtest.h"
#include <iterator>
#include <functional>
#include <sys/mman.h>
#include <vector>
#include <pthread.h>

#if !defined(_POSIX_BARRIERS) || _POSIX_BARRIERS < 0
// Implement pthread_barrier_* for platforms that don't implement them (Darwin)

#define PTHREAD_BARRIER_SERIAL_THREAD 1
struct pthread_barrier_t {
  pthread_mutex_t mutex;
  pthread_cond_t cond;

  unsigned count;
  unsigned numThreadsWaiting;
};
typedef void *pthread_barrierattr_t;

static int pthread_barrier_init(pthread_barrier_t *barrier,
                                pthread_barrierattr_t*, unsigned count) {
  if (count == 0) {
    errno = EINVAL;
    return -1;
  }
  if (pthread_mutex_init(&barrier->mutex, nullptr) != 0) {
    return -1;
  }
  if (pthread_cond_init(&barrier->cond, nullptr) != 0) {
    pthread_mutex_destroy(&barrier->mutex);
    return -1;
  }
  barrier->count = count;
  barrier->numThreadsWaiting = 0;
  return 0;
}

static int pthread_barrier_destroy(pthread_barrier_t *barrier) {
  // want to destroy both even if destroying one fails.
  int ret = 0;
  if (pthread_cond_destroy(&barrier->cond) != 0) {
    ret = -1;
  }
  if (pthread_mutex_destroy(&barrier->mutex) != 0) {
    ret = -1;
  }
  return ret;
}

static int pthread_barrier_wait(pthread_barrier_t *barrier) {
  if (pthread_mutex_lock(&barrier->mutex) != 0) {
    return -1;
  }
  ++barrier->numThreadsWaiting;
  if (barrier->numThreadsWaiting < barrier->count) {
    // Put the thread to sleep.
    if (pthread_cond_wait(&barrier->cond, &barrier->mutex) != 0) {
      return -1;
    }
    if (pthread_mutex_unlock(&barrier->mutex) != 0) {
      return -1;
    }
    return 0;
  } else {
    // Reset thread count.
    barrier->numThreadsWaiting = 0;

    // Wake up all threads.
    if (pthread_cond_broadcast(&barrier->cond) != 0) {
      return -1;
    }
    if (pthread_mutex_unlock(&barrier->mutex) != 0) {
      return -1;
    }
    return PTHREAD_BARRIER_SERIAL_THREAD;
  }
}
#endif

using namespace swift;

// Race testing.

template <typename T>
struct RaceArgs {
  std::function<T()> code;
  pthread_barrier_t *go;
};

void *RaceThunk(void *vargs) {
  RaceArgs<void*> *args = static_cast<RaceArgs<void*> *>(vargs);
  // Signal ready. Wait for go.
  pthread_barrier_wait(args->go);
  return args->code();
}

/// RaceTest(code) runs code in many threads simultaneously, 
/// and returns a vector of all returned results.
template <typename T, int NumThreads = 64>
std::vector<T> 
RaceTest(std::function<T()> code)
{
  const unsigned threadCount = NumThreads;

  pthread_barrier_t go;
  pthread_barrier_init(&go, nullptr, threadCount);

  // Create the threads.
  pthread_t threads[threadCount];
  std::vector<RaceArgs<T>> args(threadCount, {code, &go});

  for (unsigned i = 0; i < threadCount; i++) {
    pthread_create(&threads[i], nullptr, &RaceThunk, &args[i]);
  }

  // Collect results.
  std::vector<T> results;
  for (unsigned i = 0; i < threadCount; i++) {
    void *result;
    pthread_join(threads[i], &result);
    results.push_back(static_cast<T>(result));
  }

  pthread_barrier_destroy(&go);

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

/// The general structure of a generic metadata.
template <typename Instance>
struct GenericMetadataTest {
  GenericMetadata Header;
  Instance Template;
};

GenericMetadataTest<StructMetadata> MetadataTest1 = {
  // Header
  {
    // allocation function
    [](GenericMetadata *pattern, const void *args) -> Metadata * {
      auto metadata = swift_allocateGenericValueMetadata(pattern, args);
      auto metadataWords = reinterpret_cast<const void**>(metadata);
      auto argsWords = reinterpret_cast<const void* const*>(args);
      metadataWords[2] = argsWords[0];
      return metadata;
    },
    3 * sizeof(void*), // metadata size
    1, // num arguments
    0, // address point
    {} // private data
  },

  // Fields
  {
    MetadataKind::Struct,
    reinterpret_cast<const NominalTypeDescriptor*>(&Global1),
    nullptr
  }
};

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


TEST(MetadataTest, getGenericMetadata) {
  auto metadataTemplate = (GenericMetadata*) &MetadataTest1;

  void *args[] = { &Global2 };

  auto result1 = RaceTest_ExpectEqual<const Metadata *>(
    [&]() -> const Metadata * {
      auto inst = static_cast<const StructMetadata*>
        (swift_getGenericMetadata(metadataTemplate, args));

      auto fields = reinterpret_cast<void * const *>(inst);

      EXPECT_EQ(MetadataKind::Struct, inst->getKind());
      EXPECT_EQ((const NominalTypeDescriptor*)&Global1,
                inst->Description.get());

      EXPECT_EQ(&Global2, fields[2]);

      return inst;
    });

  args[0] = &Global3;

  RaceTest_ExpectEqual<const Metadata *>(
    [&]() -> const Metadata * {
      auto inst = static_cast<const StructMetadata*>
        (swift_getGenericMetadata(metadataTemplate, args));
      EXPECT_NE(inst, result1);

      auto fields = reinterpret_cast<void * const *>(inst);
      EXPECT_EQ(MetadataKind::Struct, inst->getKind());
      EXPECT_EQ((const NominalTypeDescriptor*)&Global1,
                inst->Description.get());

      EXPECT_EQ(&Global3, fields[2]);

      return inst;
    });
}

FullMetadata<ClassMetadata> MetadataTest2 = {
  { { nullptr }, { &VALUE_WITNESS_SYM(Bo) } },
  { { { MetadataKind::Class } }, nullptr, /*rodata*/ 1,
    ClassFlags(), nullptr, 0, 0, 0, 0, 0 }
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

static const ExistentialTypeMetadata *test_getExistentialMetadata(
                  std::initializer_list<const ProtocolDescriptor *> descriptors)
{
  std::vector<const ProtocolDescriptor *> mutDescriptors(descriptors);

  return swift_getExistentialTypeMetadata(mutDescriptors.size(),
                                          mutDescriptors.data());
}

TEST(MetadataTest, getExistentialMetadata) {
  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto any = test_getExistentialMetadata({});
      EXPECT_EQ(MetadataKind::Existential, any->getKind());
      EXPECT_EQ(0U, any->Flags.getNumWitnessTables());
      EXPECT_EQ(ProtocolClassConstraint::Any, any->Flags.getClassConstraint());
      EXPECT_EQ(0U, any->Protocols.NumProtocols);
      EXPECT_EQ(SpecialProtocol::None,
                any->Flags.getSpecialProtocol());
      return any;
    });

  auto exA = RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto a = test_getExistentialMetadata({&ProtocolA});
      EXPECT_EQ(MetadataKind::Existential, a->getKind());
      EXPECT_EQ(1U, a->Flags.getNumWitnessTables());
      EXPECT_EQ(ProtocolClassConstraint::Any, a->Flags.getClassConstraint());
      EXPECT_EQ(1U, a->Protocols.NumProtocols);
      EXPECT_EQ(&ProtocolA, a->Protocols[0]);
      EXPECT_EQ(SpecialProtocol::None,
                a->Flags.getSpecialProtocol());
      return a;
    });

  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto b = test_getExistentialMetadata({&ProtocolB});
      EXPECT_NE(exA, b);
      EXPECT_EQ(MetadataKind::Existential, b->getKind());
      EXPECT_EQ(1U, b->Flags.getNumWitnessTables());
      EXPECT_EQ(ProtocolClassConstraint::Any, b->Flags.getClassConstraint());
      EXPECT_EQ(1U, b->Protocols.NumProtocols);
      EXPECT_EQ(&ProtocolB, b->Protocols[0]);
      EXPECT_EQ(SpecialProtocol::None,
                b->Flags.getSpecialProtocol());
      return b;
    });

  // protocol compositions are order-invariant
  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto ab = test_getExistentialMetadata({&ProtocolA, &ProtocolB});
      auto ba = test_getExistentialMetadata({&ProtocolB, &ProtocolA});
      EXPECT_EQ(ab, ba);
      EXPECT_EQ(MetadataKind::Existential, ab->getKind());
      EXPECT_EQ(2U, ab->Flags.getNumWitnessTables());
      EXPECT_EQ(ProtocolClassConstraint::Any, ab->Flags.getClassConstraint());
      EXPECT_EQ(2U, ab->Protocols.NumProtocols);
      EXPECT_TRUE(
           (ab->Protocols[0]==&ProtocolA && ab->Protocols[1]==&ProtocolB)
        || (ab->Protocols[0]==&ProtocolB && ab->Protocols[1]==&ProtocolA));
      EXPECT_EQ(SpecialProtocol::None,
                ab->Flags.getSpecialProtocol());
      EXPECT_EQ(SpecialProtocol::None,
                ba->Flags.getSpecialProtocol());
      return ab;
    });

  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto classConstrained
        = test_getExistentialMetadata({&ProtocolClassConstrained});
      EXPECT_EQ(MetadataKind::Existential, classConstrained->getKind());
      EXPECT_EQ(1U, classConstrained->Flags.getNumWitnessTables());
      EXPECT_EQ(ProtocolClassConstraint::Class,
                classConstrained->Flags.getClassConstraint());
      EXPECT_EQ(1U, classConstrained->Protocols.NumProtocols);
      EXPECT_EQ(SpecialProtocol::None,
                classConstrained->Flags.getSpecialProtocol());
      EXPECT_EQ(&ProtocolClassConstrained, classConstrained->Protocols[0]);
      return classConstrained;
    });

  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto noWitnessTable
        = test_getExistentialMetadata({&ProtocolNoWitnessTable});
      EXPECT_EQ(MetadataKind::Existential, noWitnessTable->getKind());
      EXPECT_EQ(0U, noWitnessTable->Flags.getNumWitnessTables());
      EXPECT_EQ(ProtocolClassConstraint::Class,
                noWitnessTable->Flags.getClassConstraint());
      EXPECT_EQ(1U, noWitnessTable->Protocols.NumProtocols);
      EXPECT_EQ(SpecialProtocol::None,
                noWitnessTable->Flags.getSpecialProtocol());
      EXPECT_EQ(&ProtocolNoWitnessTable, noWitnessTable->Protocols[0]);
      return noWitnessTable;
    });

  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto mixedWitnessTable
        = test_getExistentialMetadata({&ProtocolNoWitnessTable,
                                       &ProtocolA, &ProtocolB});
      EXPECT_EQ(MetadataKind::Existential, mixedWitnessTable->getKind());
      EXPECT_EQ(2U, mixedWitnessTable->Flags.getNumWitnessTables());
      EXPECT_EQ(ProtocolClassConstraint::Class,
                mixedWitnessTable->Flags.getClassConstraint());
      EXPECT_EQ(3U, mixedWitnessTable->Protocols.NumProtocols);
      EXPECT_EQ(SpecialProtocol::None,
                mixedWitnessTable->Flags.getSpecialProtocol());
      return mixedWitnessTable;
    });
  
  const ValueWitnessTable *ExpectedErrorValueWitnesses;
#if SWIFT_OBJC_INTEROP
  ExpectedErrorValueWitnesses = &VALUE_WITNESS_SYM(BO);
#else
  ExpectedErrorValueWitnesses = &VALUE_WITNESS_SYM(Bo);
#endif

  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto special
        = test_getExistentialMetadata({&ProtocolError});
      EXPECT_EQ(MetadataKind::Existential, special->getKind());
      EXPECT_EQ(1U, special->Flags.getNumWitnessTables());
      EXPECT_EQ(SpecialProtocol::Error,
                special->Flags.getSpecialProtocol());
      EXPECT_EQ(ExpectedErrorValueWitnesses,
                special->getValueWitnesses());
      return special;
    });

  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto special
        = test_getExistentialMetadata({&ProtocolError, &ProtocolA});
      EXPECT_EQ(MetadataKind::Existential, special->getKind());
      EXPECT_EQ(2U, special->Flags.getNumWitnessTables());
      // Compositions of special protocols aren't special.
      EXPECT_EQ(SpecialProtocol::None,
                special->Flags.getSpecialProtocol());
      EXPECT_NE(ExpectedErrorValueWitnesses,
                special->getValueWitnesses());
      return special;
    });
}

static SWIFT_CC(swift) void destroySuperclass(SWIFT_CONTEXT HeapObject *toDestroy) {}

struct {
  void *Prefix[4];
  FullMetadata<ClassMetadata> Metadata;
} SuperclassWithPrefix = {
  { &Global1, &Global3, &Global2, &Global3 },
  { { { &destroySuperclass }, { &VALUE_WITNESS_SYM(Bo) } },
    { { { MetadataKind::Class } }, nullptr, /*rodata*/ 1, ClassFlags(), nullptr,
      0, 0, 0, sizeof(SuperclassWithPrefix),
      sizeof(SuperclassWithPrefix.Prefix) + sizeof(HeapMetadataHeader) } }
};
ClassMetadata * const SuperclassWithPrefix_AddressPoint =
  &SuperclassWithPrefix.Metadata;

static SWIFT_CC(swift) void destroySubclass(SWIFT_CONTEXT HeapObject *toDestroy) {}

struct {
  GenericMetadata Header;
  FullMetadata<ClassMetadata> Pattern;
  void *Suffix[3];
} GenericSubclass = {
  {
    // allocation function
    [](GenericMetadata *pattern, const void *args) -> Metadata* {
      auto metadata =
        swift_allocateGenericClassMetadata(pattern, args,
                                           SuperclassWithPrefix_AddressPoint);
      char *bytes = (char*) metadata + sizeof(ClassMetadata);
      auto metadataWords = reinterpret_cast<const void**>(bytes);
      auto argsWords = reinterpret_cast<const void* const *>(args);
      metadataWords[2] = argsWords[0];
      return metadata;
    },
    sizeof(GenericSubclass.Pattern) + sizeof(GenericSubclass.Suffix), // pattern size
    1, // num arguments
    sizeof(HeapMetadataHeader), // address point
    {} // private data
  },
  { { { &destroySubclass }, { &VALUE_WITNESS_SYM(Bo) } },
    { { { MetadataKind::Class } }, nullptr, /*rodata*/ 1, ClassFlags(), nullptr,
      0, 0, 0,
      sizeof(GenericSubclass.Pattern) + sizeof(GenericSubclass.Suffix),
      sizeof(HeapMetadataHeader) } },
  { &Global2, &Global1, &Global2 }
};

TEST(MetadataTest, getGenericMetadata_SuperclassWithUnexpectedPrefix) {
  auto metadataTemplate = &GenericSubclass.Header;

  void *args[] = { &Global3 };

  RaceTest_ExpectEqual<const ClassMetadata *>(
    [&]() -> const ClassMetadata * {
      auto inst = static_cast<const ClassMetadata*>(
        swift_getGenericMetadata(metadataTemplate, args));
      void * const *fields = reinterpret_cast<void * const *>(inst);

      // Assert that we copied the extra prefix data from the superclass.
      EXPECT_EQ(&Global1, fields[-6]);
      EXPECT_EQ(&Global3, fields[-5]);
      EXPECT_EQ(&Global2, fields[-4]);
      EXPECT_EQ(&Global3, fields[-3]);

      // Assert that we copied the shared prefix data from the subclass.
      EXPECT_EQ((void*) &destroySubclass, fields[-2]);
      EXPECT_EQ(&VALUE_WITNESS_SYM(Bo), fields[-1]);

      // Assert that we set the superclass field.
      EXPECT_EQ(SuperclassWithPrefix_AddressPoint, fields[1]);
    
      // Assert that we copied the subclass suffix data.
      auto suffix = (void * const *) ((char*) inst + sizeof(ClassMetadata));
      EXPECT_EQ(&Global2, suffix[0]);
      EXPECT_EQ(&Global1, suffix[1]);

      // This should have been overwritten by the creation function.
      EXPECT_EQ(&Global3, suffix[2]);

      EXPECT_EQ(7 * sizeof(void*) + sizeof(GenericSubclass.Pattern),
                inst->getClassSize());
      EXPECT_EQ(4 * sizeof(void*) + sizeof(HeapMetadataHeader),
                inst->getClassAddressPoint());

      // These are all expected to be equal.
      return inst;
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
      auto ex1 = swift_getExistentialTypeMetadata(1, protoList1);
      EXPECT_EQ(MetadataKind::Existential, ex1->getKind());
      EXPECT_EQ(5 * sizeof(void*), ex1->getValueWitnesses()->getSize());
      EXPECT_EQ(alignof(void*), ex1->getValueWitnesses()->getAlignment());
      EXPECT_FALSE(ex1->getValueWitnesses()->isPOD());
      EXPECT_FALSE(ex1->getValueWitnesses()->isBitwiseTakable());
      return ex1;
    });

  const ProtocolDescriptor *protoList2[] = {
    &OpaqueProto1, &OpaqueProto2
  };
  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto ex2 = swift_getExistentialTypeMetadata(2, protoList2);
      EXPECT_EQ(MetadataKind::Existential, ex2->getKind());
      EXPECT_EQ(6 * sizeof(void*), ex2->getValueWitnesses()->getSize());
      EXPECT_EQ(alignof(void*), ex2->getValueWitnesses()->getAlignment());
      EXPECT_FALSE(ex2->getValueWitnesses()->isPOD());
      EXPECT_FALSE(ex2->getValueWitnesses()->isBitwiseTakable());
      return ex2;
    });

  const ProtocolDescriptor *protoList3[] = {
    &OpaqueProto1, &OpaqueProto2, &OpaqueProto3
  };
  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto ex3 = swift_getExistentialTypeMetadata(3, protoList3);
      EXPECT_EQ(MetadataKind::Existential, ex3->getKind());
      EXPECT_EQ(7 * sizeof(void*), ex3->getValueWitnesses()->getSize());
      EXPECT_EQ(alignof(void*), ex3->getValueWitnesses()->getAlignment());
      EXPECT_FALSE(ex3->getValueWitnesses()->isPOD());
      EXPECT_FALSE(ex3->getValueWitnesses()->isBitwiseTakable());
      return ex3;
    });
}

TEST(MetadataTest, getExistentialTypeMetadata_class) {
  const ProtocolDescriptor *protoList1[] = {
    &ClassProto1
  };
  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto ex1 = swift_getExistentialTypeMetadata(1, protoList1);
      EXPECT_EQ(MetadataKind::Existential, ex1->getKind());
      EXPECT_EQ(2 * sizeof(void*), ex1->getValueWitnesses()->getSize());
      EXPECT_EQ(alignof(void*), ex1->getValueWitnesses()->getAlignment());
      EXPECT_FALSE(ex1->getValueWitnesses()->isPOD());
      EXPECT_TRUE(ex1->getValueWitnesses()->isBitwiseTakable());
      return ex1;
    });

  const ProtocolDescriptor *protoList2[] = {
    &OpaqueProto1, &ClassProto1
  };
  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto ex2 = swift_getExistentialTypeMetadata(2, protoList2);
      EXPECT_EQ(MetadataKind::Existential, ex2->getKind());
      EXPECT_EQ(3 * sizeof(void*), ex2->getValueWitnesses()->getSize());
      EXPECT_EQ(alignof(void*), ex2->getValueWitnesses()->getAlignment());
      EXPECT_FALSE(ex2->getValueWitnesses()->isPOD());
      EXPECT_TRUE(ex2->getValueWitnesses()->isBitwiseTakable());
      return ex2;
    });

  const ProtocolDescriptor *protoList3[] = {
    &OpaqueProto1, &OpaqueProto2, &ClassProto1
  };
  RaceTest_ExpectEqual<const ExistentialTypeMetadata *>(
    [&]() -> const ExistentialTypeMetadata * {
      auto ex3 = swift_getExistentialTypeMetadata(3, protoList3);
      EXPECT_EQ(MetadataKind::Existential, ex3->getKind());
      EXPECT_EQ(4 * sizeof(void*), ex3->getValueWitnesses()->getSize());
      EXPECT_EQ(alignof(void*), ex3->getValueWitnesses()->getAlignment());
      EXPECT_FALSE(ex3->getValueWitnesses()->isPOD());
      EXPECT_TRUE(ex3->getValueWitnesses()->isBitwiseTakable());
      return ex3;
    });
}

static const void *AllocatedBuffer = nullptr;
static const void *DeallocatedBuffer = nullptr;

namespace swift {
  void installCommonValueWitnesses(ValueWitnessTable *vwtable);
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

  installCommonValueWitnesses(&testTable);

  // Replace allocateBuffer and destroyBuffer with logging versions.
  testTable.allocateBuffer =
    [](ValueBuffer *buf, const Metadata *self) -> OpaqueValue * {
      void *mem = malloc(self->getValueWitnesses()->size);
      *reinterpret_cast<void**>(buf) = mem;
      AllocatedBuffer = mem;

      return reinterpret_cast<OpaqueValue *>(mem);
    };
  testTable.destroyBuffer =
    [](ValueBuffer *buf, const Metadata *self) -> void {
      void *mem = *reinterpret_cast<void**>(buf);
      DeallocatedBuffer = mem;

      free(mem);
    };
  struct {
    ValueBuffer buffer;
    uintptr_t canary;
  } buf1{{}, 0x5A5A5A5AU}, buf2{{}, 0xA5A5A5A5U};
  testTable.allocateBuffer(&buf1.buffer, &testMetadata);
  testTable.initializeBufferWithTakeOfBuffer(&buf2.buffer, &buf1.buffer,
                                             &testMetadata);
  testTable.destroyBuffer(&buf2.buffer, &testMetadata);

  EXPECT_EQ(AllocatedBuffer, DeallocatedBuffer);
  EXPECT_EQ(buf1.canary, (uintptr_t)0x5A5A5A5AU);
  EXPECT_EQ(buf2.canary, (uintptr_t)0xA5A5A5A5U);
}

// We cannot construct RelativeDirectPointer instances, so define
// a "shadow" struct for that purpose
struct GenericWitnessTableStorage {
  uint16_t WitnessTableSizeInWords;
  uint16_t WitnessTablePrivateSizeInWords;
  int32_t Protocol;
  int32_t Pattern;
  int32_t Instantiator;
  void *PrivateData[swift::NumGenericMetadataPrivateDataWords];
};

template<typename T>
static void initializeRelativePointer(int32_t *ptr, T value) {
  *ptr = (int32_t)(value == nullptr ? 0 : (uintptr_t) value - (uintptr_t) ptr);
}

// Tests for resilient witness table instantiation, with runtime-provided
// default requirements

static void witnessTableInstantiator(WitnessTable *instantiatedTable,
                                     const Metadata *type,
                                     void * const *instantiationArgs) {
  EXPECT_EQ(type, nullptr);
  EXPECT_EQ(instantiationArgs, nullptr);

  EXPECT_EQ(((void **) instantiatedTable)[0], (void*) 123);
  EXPECT_EQ(((void **) instantiatedTable)[1], (void*) 234);

  // The last witness is computed dynamically at instantiation time.
  ((void **) instantiatedTable)[2] = (void *) 345;
}

// A mock protocol descriptor with some default witnesses at the end.
//
// Note: It is not standards-compliant to compare function pointers for
// equality, so we just use fake addresses instead.
struct TestProtocol {
  ProtocolDescriptor descriptor;
  const void *witnesses[2] = {
    (void *) 996633,
    (void *) 336699
  };

  TestProtocol()
    : descriptor("TestProtocol",
                 nullptr,
                 ProtocolDescriptorFlags().withResilient(true)) {
    descriptor.MinimumWitnessTableSizeInWords = 3;
    descriptor.DefaultWitnessTableSizeInWords = 2;
  }
};

// All of these have to be global to relative reference each other, and
// the instantiator function.
TestProtocol testProtocol;
GenericWitnessTableStorage tableStorage1;
GenericWitnessTableStorage tableStorage2;
GenericWitnessTableStorage tableStorage3;
GenericWitnessTableStorage tableStorage4;

const void *witnesses[] = {
  (void *) 123,
  (void *) 234,
  (void *) 0,   // filled in by instantiator function
  (void *) 456,
  (void *) 567
};

TEST(WitnessTableTest, getGenericWitnessTable) {
  EXPECT_EQ(sizeof(GenericWitnessTableStorage), sizeof(GenericWitnessTable));

  EXPECT_EQ(testProtocol.descriptor.getDefaultWitnesses()[0],
            (void *) 996633);
  EXPECT_EQ(testProtocol.descriptor.getDefaultWitnesses()[1],
            (void *) 336699);

  // Conformance provides all requirements, and we don't have an
  // instantiator, so we can just return the pattern.
  {
    tableStorage1.WitnessTableSizeInWords = 5;
    tableStorage1.WitnessTablePrivateSizeInWords = 0;
    initializeRelativePointer(&tableStorage1.Protocol, &testProtocol.descriptor);
    initializeRelativePointer(&tableStorage1.Pattern, witnesses);
    initializeRelativePointer(&tableStorage1.Instantiator, nullptr);

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
    tableStorage2.WitnessTableSizeInWords = 5;
    tableStorage2.WitnessTablePrivateSizeInWords = 1;
    initializeRelativePointer(&tableStorage2.Protocol, &testProtocol.descriptor);
    initializeRelativePointer(&tableStorage2.Pattern, witnesses);
    initializeRelativePointer(&tableStorage2.Instantiator,
                              (const void *) witnessTableInstantiator);

    GenericWitnessTable *table = reinterpret_cast<GenericWitnessTable *>(
        &tableStorage2);

    RaceTest_ExpectEqual<const WitnessTable *>(
      [&]() -> const WitnessTable * {
        const WitnessTable *instantiatedTable =
            swift_getGenericWitnessTable(table, nullptr, nullptr);

        EXPECT_NE(instantiatedTable, table->Pattern.get());

        EXPECT_EQ(((void **) instantiatedTable)[-1], (void *) 0);

        EXPECT_EQ(((void **) instantiatedTable)[0], (void *) 123);
        EXPECT_EQ(((void **) instantiatedTable)[1], (void *) 234);
        EXPECT_EQ(((void **) instantiatedTable)[2], (void *) 345);
        EXPECT_EQ(((void **) instantiatedTable)[3], (void *) 456);
        EXPECT_EQ(((void **) instantiatedTable)[4], (void *) 567);

        return instantiatedTable;
      });
  }

  // Conformance needs one default requirement to be filled in
  {
    tableStorage3.WitnessTableSizeInWords = 4;
    tableStorage3.WitnessTablePrivateSizeInWords = 1;
    initializeRelativePointer(&tableStorage3.Protocol, &testProtocol.descriptor);
    initializeRelativePointer(&tableStorage3.Pattern, witnesses);
    initializeRelativePointer(&tableStorage3.Instantiator, witnessTableInstantiator);

    GenericWitnessTable *table = reinterpret_cast<GenericWitnessTable *>(
        &tableStorage3);

    RaceTest_ExpectEqual<const WitnessTable *>(
      [&]() -> const WitnessTable * {
        const WitnessTable *instantiatedTable =
            swift_getGenericWitnessTable(table, nullptr, nullptr);

        EXPECT_NE(instantiatedTable, table->Pattern.get());

        EXPECT_EQ(((void **) instantiatedTable)[-1], (void *) 0);

        EXPECT_EQ(((void **) instantiatedTable)[0], (void *) 123);
        EXPECT_EQ(((void **) instantiatedTable)[1], (void *) 234);
        EXPECT_EQ(((void **) instantiatedTable)[2], (void *) 345);
        EXPECT_EQ(((void **) instantiatedTable)[3], (void *) 456);
        EXPECT_EQ(((void **) instantiatedTable)[4], (void *) 336699);

        return instantiatedTable;
      });
  }

  // Third case: conformance needs both default requirements
  // to be filled in
  {
    tableStorage4.WitnessTableSizeInWords = 3;
    tableStorage4.WitnessTablePrivateSizeInWords = 1;
    initializeRelativePointer(&tableStorage4.Protocol, &testProtocol.descriptor);
    initializeRelativePointer(&tableStorage4.Pattern, witnesses);
    initializeRelativePointer(&tableStorage4.Instantiator, witnessTableInstantiator);

    GenericWitnessTable *table = reinterpret_cast<GenericWitnessTable *>(
        &tableStorage4);

    RaceTest_ExpectEqual<const WitnessTable *>(
      [&]() -> const WitnessTable * {
        const WitnessTable *instantiatedTable =
            swift_getGenericWitnessTable(table, nullptr, nullptr);

        EXPECT_NE(instantiatedTable, table->Pattern.get());

        EXPECT_EQ(((void **) instantiatedTable)[-1], (void *) 0);

        EXPECT_EQ(((void **) instantiatedTable)[0], (void *) 123);
        EXPECT_EQ(((void **) instantiatedTable)[1], (void *) 234);
        EXPECT_EQ(((void **) instantiatedTable)[2], (void *) 345);
        EXPECT_EQ(((void **) instantiatedTable)[3], (void *) 996633);
        EXPECT_EQ(((void **) instantiatedTable)[4], (void *) 336699);

        return instantiatedTable;
      });
  }
}
