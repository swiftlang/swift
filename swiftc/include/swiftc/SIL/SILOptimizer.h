#ifndef SWIFTC_SIL_SILOPTIMIZER_H
#define SWIFTC_SIL_SILOPTIMIZER_H

#include "swiftc/SIL/SILFunction.h"

namespace swiftc {

/// SIL optimizer that performs high-level optimizations.
class SILOptimizer {
public:
    SILOptimizer();

    /// Optimize an entire SIL module.
    void optimizeModule(SILModule& module);

private:
    /// Optimize a single function.
    void optimizeFunction(SILFunction& function);

    /// Dead code elimination pass.
    void eliminateDeadCode(SILFunction& function);

    /// Function inlining pass.
    void performInlining(SILFunction& function);

    /// ARC optimization pass.
    void optimizeARC(SILFunction& function);

    /// Control flow simplification pass.
    void simplifyControlFlow(SILFunction& function);
};

} // namespace swiftc

#endif // SWIFTC_SIL_SILOPTIMIZER_H