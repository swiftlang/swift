
import pass_pipeline as ppipe

import passes as p


def simplifycfg_silcombine_passlist():
    return ppipe.PassList([
        p.SimplifyCFG,
        p.SILCombine,
        p.SimplifyCFG,
    ])


def highlevel_loopopt_passlist():
    return ppipe.PassList([
        p.LowerAggregateInstrs,
        p.SILCombine,
        p.SROA,
        p.Mem2Reg,
        p.DCE,
        p.SILCombine,
        simplifycfg_silcombine_passlist(),
        p.LoopRotate,
        p.DCE,
        p.CSE,
        p.SILCombine,
        p.SimplifyCFG,
        p.ABCOpt,
        p.DCE,
        p.COWArrayOpts,
        p.DCE,
        p.SwiftArrayOpts,
    ])


def lowlevel_loopopt_passlist():
    return ppipe.PassList([
        p.LICM,
        p.DCE,
        p.CSE,
        p.SILCombine,
        p.SimplifyCFG,
    ])


def inliner_for_optlevel(optlevel):
    if optlevel == 'high':
        return p.EarlyInliner
    elif optlevel == 'mid':
        return p.PerfInliner
    elif optlevel == 'low':
        return p.LateInliner
    else:
        raise RuntimeError('Unknown opt level')


def ssapass_passlist(optlevel):
    return ppipe.PassList([
        simplifycfg_silcombine_passlist(),
        p.AllocBoxToStack,
        p.CopyForwarding,
        p.LowerAggregateInstrs,
        p.SILCombine,
        p.SROA,
        p.Mem2Reg,
        p.PerformanceConstantPropagation,
        p.DCE,
        p.CSE,
        p.SILCombine,
        simplifycfg_silcombine_passlist(),
        p.GlobalLoadStoreOpts,
        # Need to add proper argument here
        p.CodeMotion,
        p.GlobalARCOpts,
        p.SpeculativeDevirtualizer,
        p.SILLinker,
        inliner_for_optlevel(optlevel),
        p.SimplifyCFG,
        p.CodeMotion,
        p.GlobalARCOpts,
    ])


def lower_passlist():
    return ppipe.PassList([
        p.DeadFunctionElimination,
        p.DeadObjectElimination,
        p.GlobalOpt,
        p.CapturePropagation,
        p.ClosureSpecializer,
        p.SpeculativeDevirtualizer,
        p.FunctionSignatureOpts,
    ])


def normal_passpipelines():
    result = []

    x = ppipe.PassPipeline('HighLevel', {'name': 'run_n_times', 'count': 2})
    x.add_pass(ssapass_passlist('high'))
    result.append(x)

    x = ppipe.PassPipeline('EarlyLoopOpt', {'name': 'run_n_times', 'count': 1})
    x.add_pass(highlevel_loopopt_passlist())
    result.append(x)

    x = ppipe.PassPipeline('MidLevelOpt', {'name': 'run_n_times', 'count': 2})
    x.add_pass(ssapass_passlist('mid'))
    result.append(x)

    x = ppipe.PassPipeline('Lower', {'name': 'run_to_fixed_point'})
    x.add_pass(lower_passlist())
    result.append(x)

    x = ppipe.PassPipeline('LowLevel', {'name': 'run_n_times', 'count': 1})
    x.add_pass(ssapass_passlist('low'))
    result.append(x)

    x = ppipe.PassPipeline('LateLoopOpt', {'name': 'run_n_times', 'count': 1})
    x.add_pass([lowlevel_loopopt_passlist(), p.DeadFunctionElimination])
    result.append(x)

    return result
