## 64 bit Architecture Register Allocations

From Swift 5, the calling convention register allocation for 64 architectures is largely based on [existing](https://developer.apple.com/library/content/documentation/Xcode/Conceptual/iPhoneOSABIReference/Articles/ARM64FunctionCallingConventions.html) [standards](https://developer.apple.com/library/content/documentation/DeveloperTools/Conceptual/LowLevelABI/140-x86-64_Function_Calling_Conventions/x86_64.html) with the addition of the context and error return registers discussed in the ABI manifesto:

| Register Purpose | ARM64 | x86_64 |
| ------------- |:-------------:| ----- |
| Context register (self) | x20 | r13 |
| Error return register | x21 | r12 |
| Struct return pointer | x8 | rax |
| Float call arguments | d0 - d7 | xmm0 - xmm7 |
| Integer call arguments | x0 - x7 | rdi, rsi, rdx, rcx, r8, r9 |
| Float return | d0, d1 | xmm0, xmm1 |
| Integer return | x1, x2 | rax, rbx |
