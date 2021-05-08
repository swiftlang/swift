// RUN: %scale-test --begin 1 --end 20 --step 1 --select NumRedundantRequirementSteps --polynomial-threshold 2 %s

protocol P {}

func f<T>(_: T) where
%for i in range(0, N):
  T : P,
%end
  T : P {}
