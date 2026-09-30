// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/stats)
// RUN: %empty-directory(%t/dump-stats)

// RUN: %target-swift-emit-pcm -module-name script -o %t/script.pcm %S/Inputs/custom-modules/module.modulemap -stats-output-dir %t/stats
// RUN: %swift-dump-pcm %t/script.pcm -stats-output-dir %t/dump-stats
