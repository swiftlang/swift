//=== llvm/Support/Unix/ThreadLocal.inc - Unix Thread Local Data -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the Unix specific (non-pthread) ThreadLocal class.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//=== WARNING: Implementation here must contain only generic UNIX code that
//===          is guaranteed to work on *all* UNIX variants.
//===----------------------------------------------------------------------===//

#include "llvm/Config/config.h"

#if defined(HAVE_PTHREAD_H) && defined(HAVE_PTHREAD_GETSPECIFIC)

#include <cassert>
#include <pthread.h>
#include <stdlib.h>

namespace llvm {
using namespace sys;

ThreadLocalImpl::ThreadLocalImpl() : data() {
  static_assert(sizeof(pthread_key_t) <= sizeof(data), "size too big");
  pthread_key_t* key = reinterpret_cast<pthread_key_t*>(&data);
  int errorcode = pthread_key_create(key, nullptr);
  assert(errorcode == 0);
  (void) errorcode;
}

ThreadLocalImpl::~ThreadLocalImpl() {
  pthread_key_t* key = reinterpret_cast<pthread_key_t*>(&data);
  int errorcode = pthread_key_delete(*key);
  assert(errorcode == 0);
  (void) errorcode;
}

void ThreadLocalImpl::setInstance(const void* d) {
  pthread_key_t* key = reinterpret_cast<pthread_key_t*>(&data);
  int errorcode = pthread_setspecific(*key, d);
  assert(errorcode == 0);
  (void) errorcode;
}

void *ThreadLocalImpl::getInstance() {
  pthread_key_t* key = reinterpret_cast<pthread_key_t*>(&data);
  return pthread_getspecific(*key);
}

void ThreadLocalImpl::removeInstance() {
  setInstance(nullptr);
}

}
#else
namespace llvm {
using namespace sys;
ThreadLocalImpl::ThreadLocalImpl() : data() { }
ThreadLocalImpl::~ThreadLocalImpl() { }
void ThreadLocalImpl::setInstance(const void* d) { data = const_cast<void*>(d);}
void *ThreadLocalImpl::getInstance() { return data; }
void ThreadLocalImpl::removeInstance() { setInstance(0); }
}
#endif
