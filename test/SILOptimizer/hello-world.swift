// RUN: rm -rf %t && mkdir -p %t/stats
// RUN: %target-swift-frontend -emit-sil -stats-output-dir %t/stats %s -o /dev/null
// RUN: %utils/process-stats-dir.py --evaluate 'NumSILGenFunctions < 10' %t/stats
// RUN: %utils/process-stats-dir.py --evaluate 'NumSILOptFunctions < 10' %t/stats

print("Hello world")
