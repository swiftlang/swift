// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// XFAIL: linux
// FIXME: Glibc needs the same overlay.

import StdlibUnittest
import Darwin

var POSIXSemaphore = TestSuite("POSIXSemaphore")

let semaphoreName = "TestSem"

POSIXSemaphore.setUp {
  sem_unlink(semaphoreName)
}

// Failed semaphore creation.
POSIXSemaphore.test("sem_open fail") {
  let sem = sem_open(semaphoreName, 0)
  expectEqual(SEM_FAILED, sem)
  expectEqual(ENOENT, errno)
}

// Successful semaphore creation.
POSIXSemaphore.test("sem_open success") {
  let sem = sem_open(semaphoreName, O_CREAT, 0o777, 1)
  expectNotEqual(SEM_FAILED, sem)

  let res = sem_close(sem)
  expectEqual(0, res)

  let res2 = sem_unlink(semaphoreName)
  expectEqual(0, res2)
}

// Successful semaphore creation with O_EXCL.
POSIXSemaphore.test("sem_open O_EXCL success") {
  let sem = sem_open(semaphoreName, O_CREAT | O_EXCL, 0o777, 1)
  expectNotEqual(SEM_FAILED, sem)

  let res = sem_close(sem)
  expectEqual(0, res)

  let res2 = sem_unlink(semaphoreName)
  expectEqual(0, res2)
}

// Successful creation and re-obtaining of existing semaphore.
POSIXSemaphore.test("sem_open existing") {
  let sem = sem_open(semaphoreName, O_CREAT, 0o777, 1)
  expectNotEqual(SEM_FAILED, sem)

  let sem2 = sem_open(semaphoreName, 0)
  // Here, we'd like to test that the semaphores are the same, but it's quite
  // difficult.
  expectNotEqual(SEM_FAILED, sem2)

  let res = sem_close(sem)
  expectEqual(0, res)

  let res2 = sem_unlink(semaphoreName)
  expectEqual(0, res2)
}

// Fail because the semaphore already exists.
POSIXSemaphore.test("sem_open existing O_EXCL fail") {
  let sem = sem_open(semaphoreName, O_CREAT, 0o777, 1)
  expectNotEqual(SEM_FAILED, sem)

  let sem2 = sem_open(semaphoreName, O_CREAT | O_EXCL, 0o777, 1)
  expectEqual(SEM_FAILED, sem2)
  expectEqual(EEXIST, errno)

  let res = sem_close(sem)
  expectEqual(0, res)

  let res2 = sem_unlink(semaphoreName)
  expectEqual(0, res2)
}

runAllTests()

