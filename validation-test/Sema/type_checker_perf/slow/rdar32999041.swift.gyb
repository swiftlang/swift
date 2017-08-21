// RUN: not %scale-test --begin 1 --end 3 --step 1 --select incrementScopeCounter %s
// REQUIRES: OS=macosx
// REQUIRES: asserts

_ = MemoryLayout<Int>.size
%for i in range(0, N):
  + MemoryLayout<Int>.size
%end
