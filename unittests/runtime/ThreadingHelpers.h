//===--- Concurrent.cpp - Concurrent data structure tests -----------------===//
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

#ifndef THREADING_HELPERS_H
#define THREADING_HELPERS_H

#include <thread>

// When true many of the threaded tests log activity to help triage issues.
static bool trace = false;

template <typename ThreadBody, typename AfterSpinRelease>
void threadedExecute(int threadCount, ThreadBody threadBody,
                     AfterSpinRelease afterSpinRelease) {

  std::vector<std::thread> threads;

  // Block the threads we are about to create.
  std::atomic<bool> spinWait(true);
  std::atomic<int> readyCount(0);
  std::atomic<int> activeCount(0);

  for (int i = 0; i < threadCount; ++i) {
    threads.push_back(std::thread([&, i] {
      readyCount++;

      while (spinWait) {
        std::this_thread::sleep_for(std::chrono::microseconds(10));
      }
      std::this_thread::sleep_for(std::chrono::milliseconds(1));

      activeCount++;

      threadBody(i);
    }));
  }

  while (readyCount < threadCount) {
    std::this_thread::sleep_for(std::chrono::milliseconds(1));
  }

  // Allow our threads to fight for the lock.
  spinWait = false;

  while (activeCount < threadCount) {
    std::this_thread::sleep_for(std::chrono::milliseconds(1));
  }

  afterSpinRelease();

  // Wait until all of our threads have finished.
  for (auto &thread : threads) {
    thread.join();
  }
}

template <typename ThreadBody>
void threadedExecute(int threadCount, ThreadBody threadBody) {
  threadedExecute(threadCount, threadBody, [] {});
}

template <typename M, typename C, typename ConsumerBody, typename ProducerBody>
void threadedExecute(M &mutex, C &condition, bool &doneCondition,
                     ConsumerBody consumerBody, ProducerBody producerBody) {

  std::vector<std::thread> producers;
  std::vector<std::thread> consumers;

  // Block the threads we are about to create.
  std::atomic<bool> spinWait(true);

  for (int i = 1; i <= 8; ++i) {
    consumers.push_back(std::thread([&, i] {
      while (spinWait) {
        std::this_thread::sleep_for(std::chrono::microseconds(10));
      }
      std::this_thread::sleep_for(std::chrono::milliseconds(1));

      consumerBody(i);

      if (trace)
        printf("### Consumer[%d] thread exiting.\n", i);
    }));
  }

  for (int i = 1; i <= 5; ++i) {
    producers.push_back(std::thread([&, i] {
      while (spinWait) {
        std::this_thread::sleep_for(std::chrono::microseconds(10));
      }
      std::this_thread::sleep_for(std::chrono::milliseconds(1));

      producerBody(i);

      if (trace)
        printf("### Producer[%d] thread exiting.\n", i);
    }));
  }

  // Poor mans attempt to get as many threads ready as possible before
  // dropping spinWait, it doesn't have to be perfect.
  std::this_thread::sleep_for(std::chrono::milliseconds(100));

  // Allow our threads to fight for the lock.
  spinWait = false;

  // Wait until all of our producer threads have finished.
  for (auto &thread : producers) {
    thread.join();
  }

  // Inform consumers that producers are done.
  mutex.withLockThenNotifyAll(condition, [&] {
    if (trace)
      printf("### Informing consumers we are done.\n");
    doneCondition = true;
  });

  // Wait for consumers to finish.
  for (auto &thread : consumers) {
    thread.join();
  }
}

#endif
