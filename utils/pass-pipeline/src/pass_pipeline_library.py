
import pass_pipeline as ppipe
import passes

def diagnostic_passlist():
    return ppipe.PassList([
        passes.CapturePromotion,
        passes.AllocBoxToStack,
        passes.InOutDeshadowing,
        passes.NoReturnFolding,
        passes.DefiniteInitialization,
        passes.PredictableMemoryOptimizations,
        passes.DiagnosticConstantPropagation,
        passes.DiagnoseUnreachable,
        passes.EmitDFDiagnostics,
        passes.SplitNonCondBrCriticalEdges,
    ])

def simplifycfg_silcombine_passlist():
    return ppipe.PassList([
        passes.SimplifyCFG,
        passes.SILCombine,
        passes.SimplifyCFG,
    ])

def highlevel_loopopt_passlist():
    return ppipe.PassList([
        passes.LowerAggregateInstrs,
        passes.SILCombine,
        passes.SROA,
        passes.Mem2Reg,
        passes.DCE,
        passes.SILCombine,
        simplifycfg_silcombine_passlist(),
        passes.LoopRotate,
        passes.DCE,
        passes.CSE,
        passes.SILCombine,
        passes.SimplifyCFG,
        passes.ABCOpt,
        passes.DCE,
        passes.COWArrayOpts,
        passes.DCE,
        passes.SwiftArrayOpts,
    ])

def lowlevel_loopopt_passlist():
    return ppipe.PassList([
        passes.LICM,
        passes.DCE,
        passes.CSE,
        passes.SILCombine,
        passes.SimplifyCFG,
    ])

def inliner_for_optlevel(optlevel):
    if optlevel == 'high':
        return passes.EarlyInliner
    elif optlevel == 'mid':
        return passes.PerfInliner
    elif optlevel == 'low':
        return passes.LateInliner
    else:
        raise RuntimeError('Unknown opt level')

def ssapass_passlist(optlevel):
    return ppipe.PassList([
        simplifycfg_silcombine_passlist(),
        passes.AllocBoxToStack,
        passes.CopyForwarding,
        passes.LowerAggregateInstrs,
        passes.SILCombine,
        passes.SROA,
        passes.Mem2Reg,
        passes.PerformanceConstantPropagation,
        passes.DCE,
        passes.CSE,
        passes.SILCombine,
        simplifycfg_silcombine_passlist(),
        passes.GlobalLoadStoreOpts,
        passes.CodeMotion, # Need to add proper argument here
        passes.GlobalARCOpts,
        passes.Devirtualizer,
        passes.GenericSpecializer,
        passes.SILLinker,
        inliner_for_optlevel(optlevel),
        passes.SimplifyCFG,
        passes.CodeMotion,
        passes.GlobalARCOpts,
    ])

def lower_passlist():
    return ppipe.PassList([
        passes.DeadFunctionElimination,
        passes.DeadObjectElimination,
        passes.GlobalOpt,
        passes.CapturePropagation,
        passes.ClosureSpecializer,
        passes.Devirtualizer,
        passes.InlineCaches,
        passes.FunctionSignatureOpts,
    ])
        
def specialization_passlist():
    return ppipe.PassList([passes.SILLinker, passes.GenericSpecializer])

def normal_passpipelines():
    result = []

    x = ppipe.PassPipeline('PreSpecialize', {'name': 'run_to_fixed_point'})
    x.addPass(specialization_passlist())
    result.append(x)

    x = ppipe.PassPipeline('HighLevel', {'name': 'run_n_times', 'count': 2})
    x.addPass(ssapass_passlist('high'))
    result.append(x)

    x = ppipe.PassPipeline('EarlyLoopOpt', {'name' : 'run_n_times', 'count' : 1})
    x.addPass(highlevel_loopopt_passlist())
    result.append(x)

    x = ppipe.PassPipeline('MidLevelOpt', {'name' : 'run_n_times', 'count' : 2})
    x.addPass(ssapass_passlist('mid'))
    result.append(x)

    x = ppipe.PassPipeline('Lower', {'name' : 'run_to_fixed_point'})
    x.addPass(lower_passlist())
    result.append(x)

    x = ppipe.PassPipeline('LowLevel', {'name' : 'run_n_times', 'count' : 1})
    x.addPass(ssapass_passlist('low'))
    result.append(x)

    x = ppipe.PassPipeline('LateLoopOpt', {'name' : 'run_n_times', 'count' : 1})
    x.addPass([lowlevel_loopopt_passlist(), passes.DeadFunctionElimination])
    result.append(x)

    return result
