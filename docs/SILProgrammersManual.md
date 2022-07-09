# SIL Programmers' Manual

This document provides information for developers working on the
implementation of SIL. The formal specification of the Swift
Intermediate _Language_ is in [SIL.rst](SIL.rst). This is a guide to the internal
implementation. Source comments normally provide this level of
information as much as possible. However, some features of the
implementation are spread out across the source base. This
documentation is meant to offer a central point for approaching the
source base with links to the code for more detailed comments.

## SILType

TBD: Define the different levels of types. Explain type lowering with
examples.

## SILInstructionResults

TBD: Explain how the various types fit together with pointers to the
source: SILValue, SILInstruction, SingleValueInstruction,
MultipleValueInstructionResult. And why it was done this way.

## SILGen

TBD: Possibly link to a separate document explaining the architecture of SILGen.

Some information from SIL.rst could be moved here.

## IRGen

TBD: Possibly link to a separate document explaining the architecture of IRGen.

## SILAnalysis and the PassManager

TBD: describe the mechanism by which passes invalidate and update the
PassManager and its available analyses.

## High Level SIL Optimizations

[HighLevelSILOptimizations.rst](HighLevelSILOptimizations.rst) discusses how the optimizer imbues
certain special SIL types and SIL functions with higher level
semantics.
