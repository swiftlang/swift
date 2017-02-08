#include "swift/Basic/ThreadSafeRefCounted.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "gtest/gtest.h"

using llvm::IntrusiveRefCntPtr;

struct TestRelease : llvm::ThreadSafeRefCountedBase<TestRelease> {
  bool &destroy;
  TestRelease(bool &destroy) : destroy(destroy) {}
  ~TestRelease() { destroy = true; }
};

TEST(ThreadSafeRefCountedBase, ReleaseSimple) {
  bool destroyed = false;
  {
    IntrusiveRefCntPtr<TestRelease> ref = new TestRelease(destroyed);
  }
  EXPECT_TRUE(destroyed);
}
TEST(ThreadSafeRefCountedBase, Release) {
  bool destroyed = false;
  {
    IntrusiveRefCntPtr<TestRelease> ref = new TestRelease(destroyed);
    ref->Retain();
    ref->Release();
  }
  EXPECT_TRUE(destroyed);
}

struct TestReleaseVPTR : swift::ThreadSafeRefCountedBaseVPTR {
  bool &destroy;
  TestReleaseVPTR(bool &destroy) : destroy(destroy) {}
  ~TestReleaseVPTR() override { destroy = true; }
};

TEST(ThreadSafeRefCountedBaseVPTR, ReleaseSimple) {
  bool destroyed = false;
  {
    IntrusiveRefCntPtr<TestReleaseVPTR> ref = new TestReleaseVPTR(destroyed);
  }
  EXPECT_TRUE(destroyed);
}
TEST(ThreadSafeRefCountedBaseVPTR, Release) {
  bool destroyed = false;
  {
    IntrusiveRefCntPtr<TestReleaseVPTR> ref = new TestReleaseVPTR(destroyed);
    ref->Retain();
    ref->Release();
  }
  EXPECT_TRUE(destroyed);
}
