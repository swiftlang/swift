#!/bin/zsh

declare -a sport=(
SwiftCompilerSources/Sources/Optimizer/Analysis/AliasAnalysis.swift
SwiftCompilerSources/Sources/Optimizer/FunctionPasses/CMakeLists.txt
SwiftCompilerSources/Sources/Optimizer/PassManager/PassRegistration.swift
SwiftCompilerSources/Sources/Optimizer/TestPasses/MemBehaviorDumper.swift
SwiftCompilerSources/Sources/Optimizer/Utilities/AccessUtils.swift
SwiftCompilerSources/Sources/Optimizer/Utilities/EscapeUtils.swift
SwiftCompilerSources/Sources/Optimizer/Utilities/WalkUtils.swift
SwiftCompilerSources/Sources/SIL/BasicBlock.swift
SwiftCompilerSources/Sources/SIL/Builder.swift
SwiftCompilerSources/Sources/SIL/Instruction.swift
SwiftCompilerSources/Sources/SIL/SmallProjectionPath.swift
)

for i in ${nums[@]}
do
echo -e "$i \n"
done
