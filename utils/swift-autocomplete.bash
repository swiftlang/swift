# Provides auto-completion for swift tools options and for ninja build targets.
#
# Include this bash source code with the source command, e.g. in your bash ~/.profile file:
#    source swift-autocomplete.bash

_swift_complete()
{
  local tool currentWord prevWord

  tool="${COMP_WORDS[0]}"
  currentWord="${COMP_WORDS[COMP_CWORD]}"
  prevWord="${COMP_WORDS[COMP_CWORD-1]}"

  if [[ ${currentWord} != -* ]] ; then
    COMPREPLY=( $(compgen -f "${currentWord}") )
    return 0
  fi

  if [[ ${prevWord} == "-Xllvm" ]] ; then
    # Don't know how to get the help for llvm options automatically.
    # So we use a grep'ed static list.
    COMPREPLY=( $(compgen -W "\
      -disable-swift-specific-llvm-optzns \
      -stack-promotion-limit \
      -view-cfg-max-columns \
      -view-cfg-long-line-behavior \
      -view-cfg-remove-use-list-comments \
      -view-cfg-only-for-function \
      -sil-print-no-color \
      -verify-skip-unreachable-must-be-last \
      -aa \
      -cache-aa-results \
      -sil-dump-call-graph \
      -sil-dump-call-graph-stats \
      -call-graph-file-check-prefix \
      -view-loop-regions-max-columns \
      -view-loop-regions-long-line-behavior \
      -view-loop-regions-remove-use-list-comments \
      -view-loop-regions-only-for-function \
      -enable-rc-identity-arg-strip \
      -enable-loop-arc \
      -sil-abcopts-report \
      -enable-abcopts \
      -enable-abc-hoisting \
      -closure-specialize-eliminate-dead-closures \
      -enable-copyforwarding \
      -enable-destroyhoisting \
      -copy-forward-start \
      -copy-forward-stop \
      -view-cfg-before-cow-for \
      -sil-array-props \
      -enable-local-store-dse \
      -enable-global-redundant-load-elim \
      -sil-looprotate \
      -sil-view-cfg \
      -sil-view-guaranteed-cfg \
      -sil-view-silgen-cfg \
      -sil-print-all \
      -sil-print-pass-name \
      -sil-print-pass-time \
      -sil-opt-pass-count \
      -sil-print-only-function \
      -sil-print-only-functions \
      -sil-print-before \
      -sil-print-after \
      -sil-print-around \
      -sil-disable-pass \
      -sil-verify-without-invalidation \
      -sil-inline-test-threshold \
      -sil-inline-test \
      -sroa-args-remove-dead-args-after \
      -ml \
      -sil-print-escapes \
      -sil-print-side-effects \
      -debug-only \
      " -- ${currentWord}) )
  else
    frontendOption=""
    if [[ ${prevWord} == "-Xfrontend" ]] || echo ${COMP_WORDS[@]} | grep --quiet -e '-frontend' ; then
      frontendOption="-frontend"
    fi
    options=`${tool} ${frontendOption} -help-hidden 2>/dev/null | grep '^  *-[a-zA-Z-]' | sed -E 's/^  *(-[a-zA-Z=-]+).*/\1/'`
    COMPREPLY=( $(compgen -W "${options}" -- ${currentWord}) )
  fi
  return 0
}

_ninja_complete()
{
  local currentWord prevWord

  tool="${COMP_WORDS[0]}"
  currentWord="${COMP_WORDS[COMP_CWORD]}"
  prevWord="${COMP_WORDS[COMP_CWORD-1]}"

  if [[ ${prevWord} == -[Cfjlkdt] ]] ; then
    COMPREPLY=( $(compgen -f ${currentWord}) )
    return 0
  fi

  targets=`${tool} -t targets 2>/dev/null | grep  -v -e '\.' -e 'swift[A-Z]' -e 'cmake_order' | sed s/:.*//`
  COMPREPLY=( $(compgen -W "${targets}" -- ${currentWord}) )
  return 0
}

complete -F _swift_complete swiftc
complete -F _swift_complete swift
complete -F _swift_complete sil-opt
complete -F _swift_complete sil-func-extractor
complete -F _swift_complete swift-demangle
complete -F _swift_complete swift-llvm-opt
complete -F _swift_complete swift-ide-test
complete -F _swift_complete swift-ios-test
complete -F _swift_complete swift-sdk-analyzer
complete -F _swift_complete swift-stdlib-tool
complete -F _swift_complete lldb-moduleimport-test
complete -F _ninja_complete ninja

