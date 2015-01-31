//===- swift/unittests/runtime/Metadata.cpp - Metadata tests --------------===//
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

#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/Concurrent.h"
#include "gtest/gtest.h"
#include <iterator>
#include <functional>
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
template <typename T>
std::vector<T> 
RaceTest(std::function<T()> code)
{
  const unsigned threadCount = 64;

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
char Global1 = 0;
char Global2 = 0;
char Global3 = 0;

/// The general structure of a generic metadata.
template <unsigned NumFields>
struct GenericMetadataTest {
  GenericMetadata Header;
  void *Fields[NumFields];
};

GenericMetadataTest<3> MetadataTest1 = {
  // Header
  {
    // allocation function
    [](GenericMetadata *pattern, const void *args) {
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
    (void*) MetadataKind::Struct,
    &Global1,
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

TEST(Concurrent, ConcurrentMemoryBank) {
  const int numElem = 16;
  const int magicValue = 123;

  // Allocate a new concurrent memory bank.
  typedef ConcurrentMemoryBank<int, numElem> BankTy;
  auto Bank = new BankTy();

  // The bank should start empty.
  EXPECT_EQ(Bank->hasFreeSpace(), true);

  // Allocate some memory from multiple threads.
  auto results = RaceTest<int*>(
    [&]() -> int* {
      // Allocate memory.
      int *ptr = Bank->allocate();
      // And initialize it.
      if (ptr) *ptr = magicValue;
      return ptr;
    }
  );

  // After so many allocation requests the bank should be full.
  EXPECT_EQ(Bank->hasFreeSpace(), false);

  // Count the number of successfully allocated memory blocks
  // and read/write into that memory to make sure it is valid.
  int NonNulls = 0;
  for (auto A : results) {
    if (A != nullptr) {
      NonNulls++;
      // Check that are are getting the value that we placed earlier.
      EXPECT_EQ(magicValue, *A);
    }
  }

  // Check that we were able to allocate all of the slots in the bank.
  EXPECT_EQ(numElem, NonNulls);

  // Delete the bank.
  delete Bank;
}


TEST(Concurrent, ConcurrentMemoryAllocator) {
  const int magicValue = 123;

  // Create an allocator with only two elements per bank to stress the
  // allocator itself.
  ConcurrentMemoryAllocator<int, 2> Allocator;

  // Allocate some memory from multiple threads.
  auto results = RaceTest<int*>(
    [&]() -> int* {
      // Allocate memory.
      int *ptr = Allocator.allocate();
      EXPECT_NE(ptr, nullptr);
      // And initialize it.
      *ptr = magicValue;
      return ptr;
    }
  );

  // Check that we can access all of the memory that we've allocated.
  for (auto A : results) {
      EXPECT_NE(A, nullptr);
      // Check that are are getting the value that we placed earlier.
      EXPECT_EQ(magicValue, *A);
  }

  // Allocate some more memory.
  for (int i = 0; i < 256; i++) {
      EXPECT_NE(Allocator.allocate(), nullptr);
  }
}

TEST(MetadataTest, getGenericMetadata) {
  auto metadataTemplate = (GenericMetadata*) &MetadataTest1;

  void *args[] = { &Global2 };

  auto result1 = RaceTest_ExpectEqual<const Metadata *>(
    [&]() -> const Metadata * {
      auto inst = swift_getGenericMetadata(metadataTemplate, args);

      auto fields = reinterpret_cast<void * const *>(inst);
      EXPECT_EQ((void*) MetadataKind::Struct, fields[0]);
      EXPECT_EQ(&Global1, fields[1]);
      EXPECT_EQ(&Global2, fields[2]);

      return inst;
    });

  args[0] = &Global3;

  RaceTest_ExpectEqual<const Metadata *>(
    [&]() -> const Metadata * {
      auto inst = swift_getGenericMetadata(metadataTemplate, args);
      EXPECT_NE(inst, result1);

      auto fields = reinterpret_cast<void * const *>(inst);
      EXPECT_EQ((void*) MetadataKind::Struct, fields[0]);
      EXPECT_EQ(&Global1, fields[1]);
      EXPECT_EQ(&Global3, fields[2]);

      return inst;
    });
}

FullMetadata<ClassMetadata> MetadataTest2 = {
  { { nullptr }, { &_TWVBo } },
  { { { MetadataKind::Class } }, nullptr, 0, ClassFlags(), nullptr, 0, 0, 0, 0, 0 }
};

TEST(MetadataTest, getMetatypeMetadata) {
  auto inst1 = RaceTest_ExpectEqual<const MetatypeMetadata *>(
    [&]() -> const MetatypeMetadata * {
      auto inst = swift_getMetatypeMetadata(&_TMdBi64_.base);

      EXPECT_EQ(sizeof(void*), inst->getValueWitnesses()->size);
      return inst;
    });

  auto inst2 = RaceTest_ExpectEqual<const MetatypeMetadata *>(
    [&]() -> const MetatypeMetadata * {
      auto inst = swift_getMetatypeMetadata(&_TMdBi32_.base);

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
  ASSERT_EQ(&_TMdBi64_.base, inst1->InstanceType);
  ASSERT_EQ(&_TMdBi32_.base, inst2->InstanceType);
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
    .withNeedsWitnessTable(true)
};

ProtocolDescriptor ProtocolB{
  "_TMp8Metadata9ProtocolB",
  nullptr,
  ProtocolDescriptorFlags()
    .withSwift(true)
    .withClassConstraint(ProtocolClassConstraint::Any)
    .withNeedsWitnessTable(true)
};

ProtocolDescriptor ProtocolClassConstrained{
  "_TMp8Metadata24ProtocolClassConstrained",
  nullptr,
  ProtocolDescriptorFlags()
    .withSwift(true)
    .withClassConstraint(ProtocolClassConstraint::Class)
    .withNeedsWitnessTable(true)
};

ProtocolDescriptor ProtocolNoWitnessTable{
  "_TMp8Metadata22ProtocolNoWitnessTable",
  nullptr,
  ProtocolDescriptorFlags()
    .withSwift(true)
    .withClassConstraint(ProtocolClassConstraint::Class)
    .withNeedsWitnessTable(false)
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
      return mixedWitnessTable;
    });
}

static void destroySuperclass(HeapObject *toDestroy) {}

struct {
  void *Prefix[4];
  FullMetadata<ClassMetadata> Metadata;
} SuperclassWithPrefix = {
  { &Global1, &Global3, &Global2, &Global3 },
  { { { &destroySuperclass }, { &_TWVBo } },
    { { { MetadataKind::Class } }, nullptr, /*rodata*/ 1, ClassFlags(), nullptr,
      0, 0, 0, sizeof(SuperclassWithPrefix),
      sizeof(SuperclassWithPrefix.Prefix) + sizeof(HeapMetadataHeader) } }
};
ClassMetadata * const SuperclassWithPrefix_AddressPoint =
  &SuperclassWithPrefix.Metadata;

static void destroySubclass(HeapObject *toDestroy) {}

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
  { { { &destroySubclass }, { &_TWVBo } },
    { { { MetadataKind::Class } }, nullptr, /*rodata*/ 1, ClassFlags(), nullptr,
      0, 0, 0, sizeof(GenericSubclass.Pattern) + sizeof(GenericSubclass.Suffix),
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
      EXPECT_EQ(&_TWVBo, fields[-1]);

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
  ProtocolDescriptorFlags().withSwift(true).withNeedsWitnessTable(true)
                           .withClassConstraint(ProtocolClassConstraint::Any)
};
static ProtocolDescriptor OpaqueProto2 = { "OpaqueProto2", nullptr,
  ProtocolDescriptorFlags().withSwift(true).withNeedsWitnessTable(true)
                           .withClassConstraint(ProtocolClassConstraint::Any)
};
static ProtocolDescriptor OpaqueProto3 = { "OpaqueProto3", nullptr,
  ProtocolDescriptorFlags().withSwift(true).withNeedsWitnessTable(true)
                           .withClassConstraint(ProtocolClassConstraint::Any)
};
static ProtocolDescriptor ClassProto1 = { "ClassProto1", nullptr,
  ProtocolDescriptorFlags().withSwift(true).withNeedsWitnessTable(true)
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
