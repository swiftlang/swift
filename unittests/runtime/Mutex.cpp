//===--- Mutex.cpp - Mutex tests ------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Mutex.h"
#include "gtest/gtest.h"
#include <chrono>
#include <thread>

using namespace swift;

TEST(MutexTest, BasicLockable) {
  Mutex mutex(/* checked = */ true);

  int count = 0;
  mutex.lock();
  count++;
  mutex.unlock();

  ASSERT_EQ(count, 1);
}

TEST(MutexTest, Lockable) {
  Mutex mutex(/* checked = */ true);

  int count = 0;
  ASSERT_TRUE(mutex.try_lock());
  count++;
  mutex.unlock();

  ASSERT_EQ(count, 1);
}

TEST(MutexTest, ScopedLock) {
  Mutex mutex(/* checked = */ true);

  int count = 0;
  {
    ScopedLock guard(mutex);
    count++;
  }

  ASSERT_EQ(count, 1);
}

TEST(MutexTest, ScopedUnlock) {
  Mutex mutex(/* checked = */ true);
  mutex.lock();

  int count = 0;
  {
    ScopedUnlock unguard(mutex);
    count++;
  }

  mutex.unlock();

  ASSERT_EQ(count, 1);
}

TEST(MutexTest, ScopedUnlockNestedUnderScopedLock) {
  Mutex mutex(/* checked = */ true);

  int count = 0;
  {
    ScopedLock guard(mutex);
    {
      ScopedUnlock unguard(mutex);
      count++;
    }
  }

  ASSERT_EQ(count, 1);
}

TEST(MutexTest, CriticalSection) {
  Mutex mutex(/* checked = */ true);

  int count = 0;
  mutex.lock([&] { count++; });

  ASSERT_EQ(count, 1);
}

TEST(MutexTest, ScopedUnlockNestedCriticalSection) {
  Mutex mutex(/* checked = */ true);

  int count = 0;
  mutex.lock([&] {
    {
      ScopedUnlock unguard(mutex);
      count++;
    }
  });

  ASSERT_EQ(count, 1);
}

template <typename ThreadBody>
void threadedExecute(int threadCount, Mutex &mutex, ThreadBody threadBody) {

  std::vector<std::thread> threads;

  // Block the threads we are about to create.
  mutex.lock();

  for (int i = 0; i < 5; ++i) {
    threads.push_back(std::thread([&] { threadBody(); }));
  }

  // Allow our threads to fight for the lock.
  mutex.unlock();

  // Wait until all of our threads have finished.
  for (auto &thread : threads) {
    thread.join();
  }
}

TEST(MutexTest, BasicLockableThreaded) {
  Mutex mutex(/* checked = */ true);
  int count = 0;

  mutex.lock();
  auto thread = std::thread([&] {
    mutex.lock();
    mutex.unlock();
  });
  mutex.unlock();
  thread.join();

  threadedExecute(5, mutex, [&] {
    for (int j = 0; j < 50; ++j) {
      mutex.lock();
      count++;
      mutex.unlock();
    }
  });

  ASSERT_EQ(count, 250);
}

TEST(MutexTest, LockableThreaded) {
  Mutex mutex(/* checked = */ true);

  mutex.lock();
  auto thread = std::thread([&] { ASSERT_FALSE(mutex.try_lock()); });
  std::this_thread::sleep_for(std::chrono::milliseconds(10));
  mutex.unlock();
  thread.join();

  thread = std::thread([&] {
    ASSERT_TRUE(mutex.try_lock());
    mutex.unlock();
  });
  thread.join();

  int count = 0;
  threadedExecute(5, mutex, [&] {
    for (int j = 0; j < 50; ++j) {
      if (mutex.try_lock()) {
        count++;
        mutex.unlock();
      } else {
        j--;
      }
    }
  });

  ASSERT_EQ(count, 250);
}

TEST(MutexTest, ScopedLockThreaded) {
  Mutex mutex(/* checked = */ true);
  int count = 0;

  threadedExecute(5, mutex, [&] {
    for (int j = 0; j < 50; ++j) {
      ScopedLock guard(mutex);
      count++;
    }
  });

  ASSERT_EQ(count, 250);
}

TEST(MutexTest, ScopedUnlockNestedUnderScopedLockThreaded) {
  Mutex mutex(/* checked = */ true);
  int count = 0;
  int badCount = 0;

  threadedExecute(5, mutex, [&] {
    for (int j = 0; j < 50; ++j) {
      ScopedLock guard(mutex);
      {
        ScopedUnlock unguard(mutex);
        badCount++;
      }
      count++;
    }
  });

  ASSERT_EQ(count, 250);
}

TEST(MutexTest, CriticalSectionThreaded) {
  Mutex mutex(/* checked = */ true);
  int count = 0;

  threadedExecute(5, mutex, [&] {
    for (int j = 0; j < 50; ++j) {
      mutex.lock([&] { count++; });
    }
  });

  ASSERT_EQ(count, 250);
}

static bool trace = false;

template <typename ConsumerBody, typename ProducerBody>
void threadedExecute(Mutex &mutex, Condition &condition, bool &done,
                     ConsumerBody consumerBody, ProducerBody producerBody) {

  std::vector<std::thread> producers;
  std::vector<std::thread> consumers;

  // Block the threads we are about to create.
  mutex.lock();

  for (int i = 1; i <= 8; ++i) {
    consumers.push_back(std::thread([&, i] {
      consumerBody(i);
      if (trace)
        printf("### Consumer[%d] thread exiting.\n", i);
    }));
  }

  for (int i = 1; i <= 5; ++i) {
    producers.push_back(std::thread([&, i] {
      producerBody(i);
      if (trace)
        printf("### Producer[%d] thread exiting.\n", i);
    }));
  }

  // Allow our threads to fight for the lock.
  mutex.unlock();

  // Wait until all of our producer threads have finished.
  for (auto &thread : producers) {
    thread.join();
  }

  // Inform consumers that producers are done.
  mutex.lockAndNotifyAll(condition, [&] {
    if (trace)
      printf("### Informing consumers we are done.\n");
    done = true;
  });

  // Wait for consumers to finish.
  for (auto &thread : consumers) {
    thread.join();
  }
}

TEST(MutexTest, ConditionThreaded) {
  Mutex mutex(/* checked = */ true);
  Condition condition;

  bool done = false;
  int count = 200;

  threadedExecute(
      mutex, condition, done,
      [&](int index) {
        ScopedLock guard(mutex);
        while (true) {
          if (count - index >= 50) {
            count -= index;
            mutex.unlock(); // Only to give other consumers a chance.
            mutex.lock();
            if (trace)
              printf("Consumer[%d] count-%d = %d\n", index, index, count);
            continue; // keep trying to consume before waiting again.
          } else if (done && count == 50) {
            if (trace)
              printf("Consumer[%d] count == %d and done!\n", index, count);
            break;
          }
          guard.wait(condition);
        }
      },
      [&](int index) {
        for (int j = 0; j < 10; j++) {
          mutex.lock();
          count += index;
          if (trace)
            printf("Producer[%d] count+%d = %d\n", index, index, count);
          condition.notifyOne();
          mutex.unlock();
        }
        if (trace)
          printf("Producer[%d] done!\n", index);
      });

  ASSERT_EQ(count, 50);
}

TEST(MutexTest, ConditionLockOrWaitLockAndNotifyThreaded) {
  Mutex mutex(/* checked = */ true);
  Condition condition;

  bool done = false;
  int count = 200;

  threadedExecute(
      mutex, condition, done,
      [&](int index) {
        mutex.lockOrWait(condition, [&, index] {
          while (true) {
            if (count - index >= 50) {
              count -= index;
              mutex.unlock(); // Only to give other consumers a chance.
              mutex.lock();
              if (trace)
                printf("Consumer[%d] count-%d = %d\n", index, index, count);
              continue; // keep trying to consume before waiting again.
            } else if (done && count == 50) {
              if (trace)
                printf("Consumer[%d] count == %d and done!\n", index, count);
              return false;
            }
            return true;
          }
        });
      },
      [&](int index) {
        for (int j = 0; j < 10; j++) {
          mutex.lockAndNotifyOne(condition, [&, index] {
            count += index;
            if (trace)
              printf("Producer[%d] count+%d = %d\n", index, index, count);
          });
        }
        if (trace)
          printf("Producer[%d] done!\n", index);
      });

  ASSERT_EQ(count, 50);
}
