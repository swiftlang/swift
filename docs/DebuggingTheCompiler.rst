:orphan:

Debugging the Swift Compiler
============================

.. contents::

Abstract
--------

This document contains some useful information for debugging the
swift compiler.

Printing the Intermediate Representations
-----------------------------------------

The most important thing when debugging the compiler is to examine the IR.
Here is how to dump the IR after the main phases of the swift compiler
(assuming you are compiling with optimizations enabled):

#. **Parser**. To print the AST after parsing::

    swiftc -dump-ast -O file.swift

#. **SILGen**. To print the SIL immediately after SILGen::

    swiftc -emit-silgen -O file.swift

#. **Mandatory SIL passes**. To print the SIL after the mandatory passes::

    swiftc -emit-sil -Onone file.swift

  Well, this is not quite true, because the compiler is running some passes
  for -Onone after the mandatory passes, too. But for most purposes you will
  get what you want to see.

#. **Performance SIL passes**. To print the SIL after the complete SIL
   oprimization pipeline::

    swiftc -emit-sil -O file-swift

#. **IRGen**. To print the LLVM IR after IR generation::

    swiftc -emit-ir -Xfrontend -disable-llvm-optzns -O file.swift

4. **LLVM passes**. To print the LLVM IR afer LLVM passes::

    swiftc -emit-ir -O file.swift

5. **Code generation**. To print the final generated code::

    swiftc -S -O file.swift

Compilation stops at the phase where you print the output. So if you want to
print the SIL *and* the LLVM IR, you have to run the compiler twice.
The output of all these dump options (except ``-dump-ast``) can be redirected
with an additional ``-o <file>`` option.


Debugging on SIL Level
~~~~~~~~~~~~~~~~~~~~~~

Options for Dumping the SIL
```````````````````````````

Often it is not sufficient to dump the SIL at the begin or end of the
optimization pipeline.
The SILPassManager supports useful options to dump the SIL also between
pass runs.

The option ``-Xllvm -sil-print-all`` dumps the whole SIL module after all
passes. Although it prints only functions which were changed by a pass, the
output can get *very* large.

It is useful if you identified a problem in the final SIL and you want to
check which pass did introduce the wrong SIL.

There are several other options available, e.g. to filter the output by
function names (``-Xllvm -sil-print-only-function``/``s``) or by pass names
(``-Xllvm -sil-print-before``/``after``/``around``).
For details see ``SILPassManager.cpp``.

Dumping the SIL and other Data in LLDB
``````````````````````````````````````

When debugging the swift compiler with LLDB (or Xcode, of course), there is
even a more powerful way to examine the data in the compiler, e.g. the SIL.
Following LLVM's dump() convention, many SIL classes (as well as AST classes)
provide a dump() function. You can call the dump function with LLDB's
``expression --`` or ``print`` or ``p`` command.

For example, to examine a SIL instruction::

    (lldb) p Inst->dump()
    %12 = struct_extract %10 : $UnsafeMutablePointer<X>, #UnsafeMutablePointer._rawValue // user: %13

To dump a whole function at the beginning of a function pass::

    (lldb) p getFunction()->dump()

SIL modules and even functions can get very large. Often it is more convenient
to dump their contents into a file and open the file in a separate editor.
This can be done with::

    (lldb) p getFunction()->dump("myfunction.sil")

You can also dump the CFG (control flow graph) of a function::

    (lldb) p Func->viewCFG()

This opens a preview window containing the CFG of the function. To continue
debugging press <CTRL>-C on the LLDB prompt.
Note that this only works in Xcode if the PATH variable in the scheme's
environment setting contains the path to the dot tool.

Using Breakpoints
`````````````````

LLDB has very powerful breakpoints, which can be utilized in many ways to
debug the compiler.
The examples in this section show the LLDB command lines. In Xcode you can set
the breakpoint properties by clicking 'Edit breakpoint'.

Let's start with a simple example: sometimes you see a function in the SIL
output and you want to know where the function was created in the compiler.
In this case you can set a conditional breakpoint in SILFunction::create and
check for the function name in the breakpoint condition::

    (lldb) br set -c 'name.equals("_TFC3nix1Xd")' -n SILFunction::create 

Sometimes you want to know which optimization does insert, remove or move a
certain instruction. To find out, set a breakpoint in
``ilist_traits<SILInstruction>::addNodeToList`` or
``ilist_traits<SILInstruction>::removeNodeFromList``, which are defined in
``SILInstruction.cpp``.
The following command sets a breakpoint which stops if a ``strong_retain``
instruction is removed::

    (lldb) br set -c 'I->getKind() == ValueKind::StrongRetainInst' -f SILInstruction.cpp -l 63

The condition can be made more precise e.g. by also testing in which function
this happens::

    (lldb) br set -c 'I->getKind() == ValueKind::StrongRetainInst &&
               I->getFunction()->getName().equals("_TFC3nix1Xd")'
               -f SILInstruction.cpp -l 63

Let's assume the breakpoint hits somewhere in the middle of compiling a large
file. This is the point where the problem appears. But often you want to break
a little bit earlier, e.g. at the entrance of the optimization's ``run``
function.

To achieve this, set another breakpoint and add breakpoint commands::

    (lldb) br set -n GlobalARCOpts::run
    Breakpoint 2
    (lldb) br com add 2
    > p int $n = $n + 1
    > c
    > DONE

Run the program (this can take quite a bit longer than before). When the first
breakpoint hits see what value $n has::

    (lldb) p $n
    (int) $n = 5

Now remove the breakpoint commands from the second breakpoint (or create a new
one) and set the ignore count to $n minus one::

    (lldb) br delete 2
    (lldb) br set -i 4 -n GlobalARCOpts::run

Run your program again and the breakpoint hits just before the first breakpoint.

