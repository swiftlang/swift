/* HACK: This empty file is here to stop LLVM's build scripts from adding
   `/EHs-c-` to the *assembler* command line for ARM64 Windows.

   The offending code is in `llvm_update_compile_flags`, in `AddLLVM.cmake`,
   which tries to be smart about whether it adds *source* properties or *target*
   properties, but the way it does the check is that it tests explicitly whether
   there are any plain C files in the target, and if so then it will consider
   files individually.

   If there are no plain C files, it will instead set the arguments for every
   source file, which unfortunately includes (for instance) `.asm` files, and
   the Windows ARM assembler does not like being passed `/EHs-c-`.

   Once LLVM has been fixed, we can remove this file. */
