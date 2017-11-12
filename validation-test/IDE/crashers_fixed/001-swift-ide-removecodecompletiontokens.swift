// RUN: not %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s

// ASAN Output: heap-buffer-overflow on address 0x610000007ffd at pc 0x0000009c9913 bp 0x7fff8de8ba90 sp 0x7fff8de8ba88

#^
