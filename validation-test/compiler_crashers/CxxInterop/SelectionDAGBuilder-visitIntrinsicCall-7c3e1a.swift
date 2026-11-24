// {"frontendArgs":["-emit-object","-o","/dev/null","-cxx-interoperability-mode=default","-Xcc","-fprofile-instr-generate"],"kind":"custom","signature":"llvm::SelectionDAGBuilder::visitIntrinsicCall(llvm::CallInst const&, unsigned int)","signatureAssert":"instrprof failed to lower an increment","signatureNext":"SelectionDAGBuilder::visitCall"}
// RUN: not --crash %target-swift-frontend -emit-object -o /dev/null -cxx-interoperability-mode=default -Xcc -fprofile-instr-generate -I %S/Inputs/SelectionDAGBuilder-visitIntrinsicCall-7c3e1a %s

import Profiled
foo()
