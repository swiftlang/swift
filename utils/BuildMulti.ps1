# Copyright 2020 Saleem Abdulrasool <compnerd@compnerd.org>
# Copyright 2023 Tristan Labelle <tristan@thebrowser.company>

<#
.SYNOPSIS
Builds the Swift toolchain like build.ps1, with some of its parts at the same
time.

.DESCRIPTION
This script takes the same parameters as build.ps1, and runs build.ps1 several
times, with the -Toolchain, -Windows, -Android and -TestPhase parameters that
select each part of the build, some of them at the same time:

- Once the Bootstrap part of the toolchain is built, the Windows SDKs of the
  other architectures build in the background, each in a run with its own
  -BuildRoot, which is merged into the image when nothing else uses it. The
  Android SDK builds in the background too.
- The Tools part builds while the Compilers part builds at the lowest priority.
- What the test suites of the compilers need builds while the Finish part
  builds. Then check-swift runs while the other tests run in the background.

With -Package, -IncludeSBoM or -Stage, the Windows SDKs build after the
toolchain instead, with the installer, which needs all of them in one run.

The SDKs and what the tests need build at a lower priority. The runs log to
<BinaryCache>\BuildMulti\<name>.log. This script prints the log of the run that
it waits for as the run writes it, and the log of each run in the background in
one piece once the run ended and the run that it waits for is done, so that no
log is split by another. When nothing was printed for a minute, it prints the
last line that the runs that it waits for logged. After a failure, it also
prints the logs of the runs that it stops. At the end, after the summary, it
prints the lines of the logs of the runs that failed that report their
failures, then the error.
#>
[CmdletBinding(PositionalBinding = $false)]
param()

# The parameters of build.ps1, which this script takes and passes on.
dynamicparam {
  $Parameters = New-Object System.Management.Automation.RuntimeDefinedParameterDictionary
  foreach ($Parameter in (Get-Command "$PSScriptRoot\build.ps1").Parameters.Values) {
    if ([System.Management.Automation.PSCmdlet]::CommonParameters.Contains($Parameter.Name)) { continue }
    $Parameters.Add($Parameter.Name, (New-Object System.Management.Automation.RuntimeDefinedParameter($Parameter.Name, $Parameter.ParameterType, $Parameter.Attributes)))
  }
  return $Parameters
}

end {

$ErrorActionPreference = "Stop"
Set-StrictMode -Version 3.0

$BuildScript = "$PSScriptRoot\build.ps1"

# The values of the parameters of build.ps1, with their defaults.
foreach ($Parameter in (Get-Command $BuildScript).ScriptBlock.Ast.ParamBlock.Parameters) {
  $Name = $Parameter.Name.VariablePath.UserPath
  $Value = if ($PSBoundParameters.ContainsKey($Name)) {
    $PSBoundParameters[$Name]
  } elseif ($Parameter.DefaultValue) {
    Invoke-Expression $Parameter.DefaultValue.Extent.Text
  } else {
    $null
  }
  Set-Variable -Name $Name -Value $Value
}

# Work around limitations of cmd passing in array arguments via powershell.exe -File
foreach ($Name in "AndroidSDKArchitectures", "AndroidSDKLinkModes", "WindowsSDKArchitectures", "WindowsSDKLinkModes", "Test") {
  $Value = @((Get-Variable -Name $Name).Value)
  if ($Value.Length -eq 1) { Set-Variable -Name $Name -Value @("$($Value[0])".Split(",")) }
}

if ($Test -contains "*") {
  # Explicitly don't include llbuild yet since tests are known to fail on Windows
  $Test = @("lld", "lldb", "lldb-swift", "swift", "dispatch", "foundation", "xctest", "swift-format", "sourcekit-lsp")
}

$BuildArchName = if ($env:PROCESSOR_ARCHITEW6432) { $env:PROCESSOR_ARCHITEW6432 } else { $env:PROCESSOR_ARCHITECTURE }
$IsCrossCompiling = $HostArchName -ne $BuildArchName

# The architecture of the Windows SDK that the toolchain is built against.
$HostSDKArchitecture = if ($HostArchName -eq "ARM64") { "Arm64" } else { "X64" }
$SelectedWindowsSDKArchitectures = @(if ($Windows) { $WindowsSDKArchitectures })

$JobLogs = "$BinaryCache\BuildMulti"
New-Item -ItemType Directory -Path $JobLogs -ErrorAction Ignore | Out-Null
$PowerShell = (Get-Process -Id $PID).Path
# The runs share the environment of the Visual Studio developer shell, which
# build.ps1 keeps in this directory.
$OwnVsDevShellCache = -not $env:SWIFT_BUILD_VSDEVSHELL_CACHE
if ($OwnVsDevShellCache) {
  $env:SWIFT_BUILD_VSDEVSHELL_CACHE = [IO.Path]::Combine([IO.Path]::GetTempPath(), "swift-vsdevshell-$([Guid]::NewGuid())")
}
$BuildMultiStart = [DateTime]::Now
Add-Type -Namespace Win32 -Name Kernel32 -MemberDefinition @"
[DllImport("kernel32.dll")]
public static extern bool GetSystemTimes(out long lpIdleTime, out long lpKernelTime, out long lpUserTime);
"@
# The time all processors have spent busy and idle since boot.
function Get-ProcessorTimes {
  $IdleTime = $KernelTime = $UserTime = [long] 0
  $null = [Win32.Kernel32]::GetSystemTimes([ref] $IdleTime, [ref] $KernelTime, [ref] $UserTime)
  # The kernel time includes the idle time.
  return @{ Busy = $KernelTime + $UserTime - $IdleTime; Idle = $IdleTime }
}
$BuildMultiStartProcessorTimes = Get-ProcessorTimes
$BuildFailure = $null
$Jobs = New-Object System.Collections.ArrayList
$CompletedJobs = New-Object System.Collections.ArrayList
$FailedJobs = New-Object System.Collections.ArrayList
# The logs are printed as the jobs wrote them, without decoding them.
$StandardOutput = [Console]::OpenStandardOutput()

# The parameters of this script that every run of build.ps1 takes. This script
# chooses the parts of each run, cleans in the first one, and gives each run its
# own trace, from which it prints the summary of all the runs.
$Excluded = @("Toolchain", "TestPhase", "Clean", "DependenciesOnly", "Stage", "Summary", "Trace")
$SharedArguments = @{}
foreach ($Argument in $PSBoundParameters.GetEnumerator()) {
  if ($Argument.Key -notin $Excluded) {
    $SharedArguments[$Argument.Key] = $Argument.Value
  }
}

# A value as PowerShell code.
function ConvertTo-PowerShellLiteral([Object] $Value) {
  if ($Value -is [System.Management.Automation.SwitchParameter] -or $Value -is [bool]) {
    return $(if ($Value) { '$true' } else { '$false' })
  }
  if ($Value -is [int] -or $Value -is [long]) {
    return "$Value"
  }
  if ($Value -is [Array]) {
    return "@($(@($Value | ForEach-Object { ConvertTo-PowerShellLiteral $_ }) -join ", "))"
  }
  return "'$("$Value".Replace("'", "''"))'"
}

# Quotes an argument for a command line in a batch file.
function ConvertTo-CommandLineArgument([string] $Argument) {
  if ($Argument -and $Argument -notmatch '[\s"&|<>^]') {
    return $Argument.Replace("%", "%%")
  }
  $Escaped = ($Argument -replace '(\\*)"', '$1$1\"') -replace '(\\+)$', '$1$1'
  return "`"$($Escaped.Replace("%", "%%"))`""
}

# Starts a run of build.ps1 in the background, with the parameters of this
# script and $Overrides. The log of a job with -Followed is printed while this
# script waits for it, instead of once it ends.
function Start-BuildJob([string] $Name, [Hashtable] $Overrides = @{}, [switch] $BelowNormal, [switch] $Followed) {
  Write-EndedBuildJobs
  $Arguments = $SharedArguments.Clone()
  if ($Summary -or $Trace) {
    $Arguments.Trace = $true
    $Arguments.Stage = "$JobLogs\$Name"
  }
  foreach ($Override in $Overrides.GetEnumerator()) {
    $Arguments[$Override.Key] = $Override.Value
  }
  $TraceDir = if ($Arguments.ContainsKey("Stage") -and $Arguments.Stage) { $Arguments.Stage } else { $BinaryCache }
  $Code = @(foreach ($Argument in ($Arguments.GetEnumerator() | Sort-Object Name)) {
    if ($null -eq $Argument.Value) { continue }
    if ($Argument.Value -is [Array] -and $Argument.Value.Length -eq 0) { continue }
    "-$($Argument.Key):$(ConvertTo-PowerShellLiteral $Argument.Value)"
  }) -join " "
  $Now = [DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss")
  if ($Followed) {
    Write-Host -ForegroundColor Cyan "[$Now] build.ps1 $Code"
  } else {
    Write-Host -ForegroundColor Cyan "[$Now] Starting $Name in the background: build.ps1 $Code"
  }

  # The run is a script that calls build.ps1, so that the arguments keep their
  # types, which powershell.exe -File does not do for switches.
  $Run = "$JobLogs\$Name.ps1"
  # With a BOM, as Windows PowerShell reads the other scripts as ANSI.
  [IO.File]::WriteAllLines($Run, [string[]]@(
    "& $(ConvertTo-PowerShellLiteral $BuildScript) $Code",
    'exit $LastExitCode'
  ), (New-Object Text.UTF8Encoding($true)))
  # cmd.exe runs the script at the requested priority and logs its output.
  $Log = "$JobLogs\$Name.log"
  # The log of an earlier build would be printed until cmd.exe replaces it.
  Remove-Item $Log -ErrorAction Ignore
  $CommandLine = @($PowerShell, "-NoProfile", "-ExecutionPolicy", "RemoteSigned", "-File", $Run)
  $Batch = "$JobLogs\$Name.cmd"
  [IO.File]::WriteAllLines($Batch, @(
    "@echo off",
    "start `"`" /b /wait $(if ($BelowNormal) { "/belownormal" }) $(($CommandLine | ForEach-Object { ConvertTo-CommandLineArgument $_ }) -join " ") > `"$Log`" 2>&1",
    "exit /b %errorlevel%"
  ))
  $Process = Start-Process -FilePath cmd.exe -ArgumentList "/d /c `"$Batch`"" -NoNewWindow -PassThru
  # Windows PowerShell only reports the exit code after the handle was opened.
  $null = $Process.Handle

  $Job = [PSCustomObject]@{
    Name = $Name
    Process = $Process
    Log = $Log
    Trace = "$TraceDir\build-timings.json"
    Followed = [bool] $Followed
    # The bytes of the log that were printed.
    Offset = [long] 0
    Printed = $false
    Stopped = $false
  }
  [void]$Jobs.Add($Job)
  return $Job
}

# Reads the last bytes of the log of a job, while the job may still write it.
function Read-BuildJobLog([PSCustomObject] $Job, [long] $Last) {
  $Stream = [IO.File]::Open($Job.Log, [IO.FileMode]::Open, [IO.FileAccess]::Read, [IO.FileShare]::ReadWrite)
  try {
    if ($Stream.Length -gt $Last) {
      $null = $Stream.Seek(-$Last, [IO.SeekOrigin]::End)
    }
    return (New-Object IO.StreamReader($Stream, [Text.Encoding]::UTF8, $true)).ReadToEnd()
  } finally {
    $Stream.Dispose()
  }
}

# The last line that a job logged, which shows what it does.
function Get-BuildJobLastLine([PSCustomObject] $Job) {
  try {
    $Lines = @((Read-BuildJobLog $Job 4096) -split "[\r\n]+" | Where-Object { $_.Trim() })
  } catch {
    return ""
  }
  if (-not $Lines) { return "" }
  # Write-Host encodes it with the code page of the console.
  $Line = $Lines[-1].Trim() -replace "[^\x20-\x7E]", "?"
  if ($Line.Length -gt 200) { $Line = "$($Line.Substring(0, 200))..." }
  return $Line
}

# Prints what a job logged since this script last did, up to its last complete
# line unless -All, and returns whether it printed anything.
function Write-BuildJobOutput([PSCustomObject] $Job, [switch] $All) {
  try {
    $Stream = [IO.File]::Open($Job.Log, [IO.FileMode]::Open, [IO.FileAccess]::Read, [IO.FileShare]::ReadWrite)
  } catch [IO.FileNotFoundException] {
    # cmd.exe did not create it yet.
    return $false
  }
  try {
    $Count = $Stream.Length - $Job.Offset
    if ($Count -le 0) { return $false }
    $null = $Stream.Seek($Job.Offset, [IO.SeekOrigin]::Begin)
    $Bytes = New-Object byte[] $Count
    $Count = $Stream.Read($Bytes, 0, $Count)
  } finally {
    $Stream.Dispose()
  }
  $Length = if ($All) { $Count } elseif ($Count -gt 0) { [Array]::LastIndexOf($Bytes, [byte]10, $Count - 1) + 1 } else { 0 }
  if ($Length -le 0) { return $false }
  $StandardOutput.Write($Bytes, 0, $Length)
  # What this script prints next starts a line.
  if ($Bytes[$Length - 1] -ne 10) {
    $StandardOutput.Write([byte[]](13, 10), 0, 2)
  }
  $StandardOutput.Flush()
  $Job.Offset += $Length
  return $true
}

# Prints the rest of the log of a job, at once for a job that is not followed so
# that it does not interleave with what this script prints.
function Write-BuildJobLog([PSCustomObject] $Job) {
  $Job.Printed = $true
  $Result = if ($Job.Stopped) { "was stopped" } else { "exited with code $($Job.Process.ExitCode)" }
  if ($Job.Followed) {
    $null = Write-BuildJobOutput $Job -All
    if ($Job.Stopped) {
      Write-Host -ForegroundColor Cyan "===== $($Job.Name) $Result ====="
    }
    return
  }
  $Start = $Job.Process.StartTime
  $End = if ($Job.Process.HasExited) { $Job.Process.ExitTime } else { [DateTime]::Now }
  Write-Host -ForegroundColor Cyan "===== $($Job.Name) ($($Start.ToString("HH:mm:ss")) to $($End.ToString("HH:mm:ss")), $($End - $Start)), from $($Job.Log) ====="
  $null = Write-BuildJobOutput $Job -All
  Write-Host -ForegroundColor Cyan "===== $($Job.Name) $Result ====="
}

# Prints the logs of the jobs that ended since this script last did, in the
# order in which they ended, except the followed jobs.
function Write-EndedBuildJobs {
  $Ended = @($Jobs | Where-Object { -not $_.Printed -and -not $_.Followed -and $_.Process.HasExited } | Sort-Object { $_.Process.ExitTime })
  foreach ($Job in $Ended) {
    Write-BuildJobLog $Job
  }
}

function Complete-BuildJob([PSCustomObject] $Job) {
  [void]$Jobs.Remove($Job)
  [void]$CompletedJobs.Add($Job)
}

# How often this script reports the jobs that it waits for.
$JobHeartbeat = [TimeSpan]::FromMinutes(1)

# Waits for background jobs, printing the log of $Follow as it is written, and
# the logs of the other jobs that end meanwhile, after it so that they do not
# split it, and throws if any of the jobs that it waited for failed. It reports
# the jobs that it waits for when nothing was printed for a while, so that the
# build does not look stuck.
function Wait-BuildJobs([Object[]] $Wait, [PSCustomObject] $Follow = $null) {
  $Heartbeat = [DateTime]::Now + $JobHeartbeat
  while ($true) {
    $Running = @(foreach ($Job in $Wait) { if (-not $Job.Process.HasExited) { $Job } })
    if ($Follow -and (Write-BuildJobOutput $Follow)) {
      $Heartbeat = [DateTime]::Now + $JobHeartbeat
    }
    if (-not $Follow) {
      Write-EndedBuildJobs
    }
    if (-not $Running) { break }

    if ([DateTime]::Now -ge $Heartbeat) {
      foreach ($Job in $Running) {
        $Line = Get-BuildJobLastLine $Job
        Write-Host "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Waiting for $($Job.Name) ($(([DateTime]::Now - $Job.Process.StartTime).ToString("hh\:mm\:ss")))$(if ($Line) { ": $Line" })"
      }
      $Heartbeat = [DateTime]::Now + $JobHeartbeat
    }
    $null = $Running[0].Process.WaitForExit(1000)
  }
  if ($Follow) {
    Write-BuildJobLog $Follow
    Write-EndedBuildJobs
  }

  $Failed = @()
  foreach ($Job in $Wait) {
    Complete-BuildJob $Job
    if ($Job.Process.ExitCode -ne 0) {
      $Failed += $Job.Name
      [void]$FailedJobs.Add($Job)
    }
  }
  if ($Failed) {
    throw "$($Failed -join ", ") failed"
  }
}

# Stops the jobs that are still running after a failure, and prints the logs
# that were not printed yet, as only the output of this script is kept.
function Stop-BuildJobs {
  foreach ($Job in $Jobs.ToArray()) {
    if (-not $Job.Process.HasExited) {
      Write-Host -ForegroundColor Red "Stopping $($Job.Name), which logs to $($Job.Log)"
      $null = & cmd.exe /d /c "taskkill /T /F /PID $($Job.Process.Id) > nul 2>&1"
      $null = $Job.Process.WaitForExit(10000)
      $Job.Stopped = $true
    }
  }
  foreach ($Job in $Jobs.ToArray()) {
    if (-not $Job.Printed) {
      try {
        Write-BuildJobLog $Job
      } catch {
        Write-Host -ForegroundColor Red "Could not print $($Job.Log): $_"
      }
    }
    if (-not $Job.Stopped -and $Job.Process.HasExited -and $Job.Process.ExitCode -ne 0) {
      [void]$FailedJobs.Add($Job)
    }
    Complete-BuildJob $Job
  }
}

# What the build and test tools print about failures.
$FailurePattern = "recorded an issue|failed after .* with \d+ issues?|^error: |: (fatal )?error: |\berror (LNK|C)\d+:|^FAILED: |^(FAIL|XPASS|UNRESOLVED|TIMEOUT): |^Test Case .* failed|^Error: |CMake Error"
# lit lists the tests that failed at the end of its output, under these lines.
$LitListPattern = "^(Failed|Unexpectedly Passed|Unresolved|Timed Out) Tests \(\d+\):$"
$LitResultPattern = "^(FAIL|XPASS|UNRESOLVED|TIMEOUT): (.+) \(\d+ of \d+(, \d+ of \d+ attempts)?\)$"
# lit prints the output of each of these tests between these lines.
$LitOutputPattern = "^\*{20} TEST '(.+)' FAILED \*{20}$"
# The tests of SourceKit-LSP print the records that it logged, which report
# expected errors too, from this line to a line with "---".
$LogRecordPattern = "^\[[^\]]+\] \w+ \d{4}-\d\d-\d\d \d\d:\d\d:\d\d"

# Prints the lists of the tests that failed and the lines that report failures
# in the logs of the jobs that failed, with the lines that follow these, as the
# logs may be far above the error that ends the output. Without such lines, it
# prints the end of the log.
function Write-BuildJobFailures {
  foreach ($Job in $FailedJobs) {
    $Name = "$($Job.Name) ($($Job.Process.StartTime.ToString("HH:mm:ss")) to $($Job.Process.ExitTime.ToString("HH:mm:ss")))"
    try {
      # Some tools end lines with \r\r\n.
      $Lines = [regex]::Split([IO.File]::ReadAllText($Job.Log, [Text.Encoding]::UTF8).TrimEnd("`r", "`n"), "\r*\n")
    } catch {
      Write-Host -ForegroundColor Red "===== $Name exited with code $($Job.Process.ExitCode), and its log could not be read: $_"
      continue
    }

    # The tests that lit lists, which are not shown again.
    $Listed = New-Object System.Collections.Generic.HashSet[string]
    for ($Index = 0; $Index -lt $Lines.Count; $Index++) {
      if ($Lines[$Index] -cmatch $LitListPattern) {
        for ($Line = $Index + 1; $Line -lt $Lines.Count -and $Lines[$Line].StartsWith("  "); $Line++) {
          [void]$Listed.Add($Lines[$Line].Trim())
        }
      }
    }

    $Shown = New-Object System.Collections.Generic.List[string]
    $Next = 0
    $Found = 0
    $InLogRecord = $false
    for ($Index = 0; $Index -lt $Lines.Count; $Index++) {
      if ($Lines[$Index] -cmatch $LogRecordPattern) {
        $InLogRecord = $true
      } elseif ($Lines[$Index] -ceq "---") {
        $InLogRecord = $false
      }
      if ($InLogRecord) {
        continue
      } elseif ($Lines[$Index] -cmatch $LitListPattern) {
        # The list, with at most 200 tests.
        $End = $Index + 1
        while ($End -lt $Lines.Count -and $Lines[$End].StartsWith("  ") -and $End -le $Index + 200) { $End++ }
        for ($Line = [Math]::Max($Index, $Next); $Line -lt $End; $Line++) {
          $Shown.Add($Lines[$Line])
        }
        if ($End -lt $Lines.Count -and $Lines[$End].StartsWith("  ")) {
          $Shown.Add("  ...")
        }
        $Next = [Math]::Max($Next, $End)
        $Found++
      } elseif ($Lines[$Index] -cmatch $LitOutputPattern -and $Listed.Contains($Matches[1])) {
        # The output of a listed test, which a search for its name finds.
        while ($Index + 1 -lt $Lines.Count -and $Lines[$Index + 1] -cne "********************") { $Index++ }
      } elseif ($Found -lt 50 -and $Lines[$Index] -cmatch $FailurePattern) {
        if ($Lines[$Index] -cmatch $LitResultPattern -and $Listed.Contains($Matches[2])) { continue }
        $Found++
        for ($Line = [Math]::Max($Index, $Next); $Line -le [Math]::Min($Index + 3, $Lines.Count - 1); $Line++) {
          $Shown.Add($Lines[$Line])
        }
        $Next = [Math]::Max($Next, $Index + 4)
      }
    }
    if ($Found) {
      Write-Host -ForegroundColor Red "===== $Name exited with code $($Job.Process.ExitCode), with these failures in its log above ====="
    } else {
      Write-Host -ForegroundColor Red "===== $Name exited with code $($Job.Process.ExitCode), at the end of its log above ====="
      $Shown.AddRange([string[]]@($Lines | Select-Object -Last 20))
    }
    $Bytes = [Text.Encoding]::UTF8.GetBytes(($Shown -join "`r`n") + "`r`n")
    $StandardOutput.Write($Bytes, 0, $Bytes.Length)
    $StandardOutput.Flush()
  }
}

# Runs build.ps1 and waits for it, printing its log as it runs, then the logs
# of the jobs that ended meanwhile, at the priority of this script or with
# -Idle at the lowest one.
function Invoke-BuildRun([string] $Name, [Hashtable] $Overrides = @{}, [switch] $Idle) {
  $ThisProcess = Get-Process -Id $PID
  $ThisPriority = $ThisProcess.PriorityClass
  # The runs that this script starts get its priority.
  if ($Idle) { $ThisProcess.PriorityClass = [Diagnostics.ProcessPriorityClass]::Idle }
  try {
    $Job = Start-BuildJob $Name $Overrides -Followed
  } finally {
    $ThisProcess.PriorityClass = $ThisPriority
  }
  Wait-BuildJobs @($Job) $Job
}

# Prints how long each run took.
function Write-BuildRuns {
  $Rows = foreach ($Job in ($CompletedJobs | Sort-Object { $_.Process.StartTime })) {
    $End = if ($Job.Process.HasExited) { $Job.Process.ExitTime } else { [DateTime]::Now }
    [PSCustomObject]@{
      Run = $Job.Name
      Start = "{0:hh\:mm\:ss}" -f ($Job.Process.StartTime - $BuildMultiStart)
      End = "{0:hh\:mm\:ss}" -f ($End - $BuildMultiStart)
      "Elapsed Time" = "{0:hh\:mm\:ss}" -f ($End - $Job.Process.StartTime)
      Result = if ($Job.Stopped) { "stopped" } elseif ($Job.Process.HasExited) { "exit $($Job.Process.ExitCode)" } else { "running" }
      Where = if ($Job.Followed) { "" } else { "background" }
    }
  }
  $Rows | Format-Table -AutoSize | Out-String -Width 200 | Write-Host
  Write-Host "Total: $("{0:hh\:mm\:ss}" -f ([DateTime]::Now - $BuildMultiStart))"
}

# Prints the steps of all the runs like build.ps1 -Summary does, with the run of
# each step, from the traces of the runs. The percentages are of the time of the
# whole build, so the steps that ran at the same time add up to more than it.
# The steps without substeps that more than two runs did, like getting the
# dependencies, are added up in one line.
function Write-BuildSummary([TimeSpan] $TotalTime, [double] $TotalCPU) {
  function Get-TraceArgument([PSCustomObject] $Event, [string] $Name) {
    if ($Event.PSObject.Properties["args"] -and $Event.args.PSObject.Properties[$Name]) { return $Event.args.$Name }
    return ""
  }

  $Roots = New-Object System.Collections.ArrayList
  $UntrackedTime = [TimeSpan]::Zero
  foreach ($Job in ($CompletedJobs | Sort-Object { $_.Process.StartTime })) {
    if (-not (Test-Path $Job.Trace)) { continue }
    try {
      $Events = @((Get-Content -Raw $Job.Trace | ConvertFrom-Json).traceEvents | Where-Object { $_.ph -eq "X" -and $_.pid -eq 0 })
    } catch {
      continue
    }
    $RunTime = [TimeSpan]::Zero
    $Open = New-Object System.Collections.Stack
    foreach ($Event in ($Events | Sort-Object { [long]$_.ts }, { -[long]$_.dur })) {
      $CPU = Get-TraceArgument $Event "CPU"
      $Entry = [PSCustomObject]@{
        "Build Step" = $Event.name
        Variant = Get-TraceArgument $Event "Variant"
        Platform = Get-TraceArgument $Event "Platform"
        Arch = Get-TraceArgument $Event "Arch"
        End = [long]$Event.ts + [long]$Event.dur
        "Elapsed Time" = [TimeSpan]::FromTicks([long]$Event.dur * 10)
        CPU = if ($CPU -ne "") { [double]$CPU } else { 0 }
        Runs = @($Job.Name)
        Children = New-Object System.Collections.ArrayList
      }
      # A step is a substep of the last step that contains it.
      while ($Open.Count -and $Open.Peek().End -lt $Entry.End) { [void]$Open.Pop() }
      if ($Open.Count) {
        [void]$Open.Peek().Children.Add($Entry)
      } else {
        [void]$Roots.Add($Entry)
        $RunTime += $Entry."Elapsed Time"
      }
      $Open.Push($Entry)
    }
    $Elapsed = $Job.Process.ExitTime - $Job.Process.StartTime
    if ($Elapsed -gt $RunTime) { $UntrackedTime += $Elapsed - $RunTime }
  }

  $Count = @{}
  foreach ($Entry in $Roots) {
    if ($Entry.Children.Count) { continue }
    $Key = "$($Entry.'Build Step')|$($Entry.Variant)|$($Entry.Platform)|$($Entry.Arch)"
    $Count[$Key] = 1 + $(if ($Count.Contains($Key)) { $Count[$Key] } else { 0 })
  }
  $Merged = [ordered]@{}
  $Entries = New-Object System.Collections.ArrayList
  foreach ($Entry in $Roots) {
    $Key = "$($Entry.'Build Step')|$($Entry.Variant)|$($Entry.Platform)|$($Entry.Arch)"
    $Merge = $Entry.Children.Count -eq 0 -and $Count[$Key] -gt 2
    if ($Merge -and $Merged.Contains($Key)) {
      $Into = $Merged[$Key]
      $Seconds = $Into."Elapsed Time".TotalSeconds + $Entry."Elapsed Time".TotalSeconds
      if ($Seconds -gt 0) {
        $Into.CPU = ($Into.CPU * $Into."Elapsed Time".TotalSeconds + $Entry.CPU * $Entry."Elapsed Time".TotalSeconds) / $Seconds
      }
      $Into."Elapsed Time" += $Entry."Elapsed Time"
      $Into.Runs += $Entry.Runs
    } else {
      if ($Merge) { $Merged[$Key] = $Entry }
      [void]$Entries.Add($Entry)
    }
  }
  if ($UntrackedTime -gt [TimeSpan]::Zero) {
    [void]$Entries.Add([PSCustomObject]@{
      "Build Step" = "(untracked)"; Variant = ""; Platform = ""; Arch = ""; "Elapsed Time" = $UntrackedTime
      CPU = $null; Runs = @(); Children = @()
    })
  }

  function ConvertTo-SummaryRows([PSCustomObject] $Entry, [int] $Depth) {
    $Self = ""
    if ($Entry.Children.Count) {
      $ChildrenTicks = [long] 0
      foreach ($Child in $Entry.Children) { $ChildrenTicks += $Child."Elapsed Time".Ticks }
      $Self = "{0:hh\:mm\:ss\.ff}" -f ($Entry."Elapsed Time" - [TimeSpan]::FromTicks($ChildrenTicks))
    }
    $Run = if ($Depth -gt 0) { "" } elseif ($Entry.Runs.Count -gt 2) { "$($Entry.Runs.Count) runs" } else { $Entry.Runs -join ", " }
    [PSCustomObject]@{
      "Build Step" = "$("  " * $Depth)$($Entry.'Build Step')"
      Variant = $Entry.Variant
      Platform = $Entry.Platform
      Arch = $Entry.Arch
      "Elapsed Time" = "{0:hh\:mm\:ss\.ff}" -f $Entry."Elapsed Time"
      Self = $Self
      "%" = "$([math]::Round($Entry.'Elapsed Time'.TotalSeconds / $TotalTime.TotalSeconds * 100, 1))%"
      CPU = if ($null -ne $Entry.CPU) { "$([math]::Round($Entry.CPU * 100))%" } else { "" }
      Run = $Run
    }
    foreach ($Child in ($Entry.Children | Sort-Object -Descending -Property "Elapsed Time")) {
      ConvertTo-SummaryRows $Child ($Depth + 1)
    }
  }

  $Rows = foreach ($Entry in ($Entries | Sort-Object -Descending -Property "Elapsed Time")) {
    ConvertTo-SummaryRows $Entry 0
  }
  $TotalRow = [PSCustomObject]@{
    "Build Step" = "TOTAL"; Variant = ""; Platform = ""; Arch = ""
    "Elapsed Time" = "{0:hh\:mm\:ss\.ff}" -f $TotalTime; Self = ""; "%" = "100.0%"
    CPU = "$([math]::Round($TotalCPU * 100))%"; Run = ""
  }
  Write-Host -ForegroundColor Cyan "Summary:"
  @($Rows) + $TotalRow | Format-Table -AutoSize | Out-String -Width 300 | Write-Host
}

# Writes the traces of the runs as one, each run with its own tracks.
function Write-BuildRunTraces([string] $Path) {
  $Events = New-Object System.Collections.ArrayList
  $Index = 0
  foreach ($Job in ($CompletedJobs | Sort-Object { $_.Process.StartTime })) {
    $File = $Job.Trace
    if (-not (Test-Path $File)) { continue }
    $Offset = [long](($Job.Process.StartTime - $BuildMultiStart).Ticks / 10)
    $Trace = Get-Content -Raw $File | ConvertFrom-Json
    foreach ($Event in $Trace.traceEvents) {
      $Event.pid = 2 * $Index + $Event.pid
      if ($Event.ph -eq "M") {
        $Event.args.name = "$($Job.Name)$(if ($Event.args.name -eq "lit") { " lit" })"
      } else {
        $Event.ts = [long]$Event.ts + $Offset
      }
      [void]$Events.Add($Event)
    }
    $Index++
  }
  $Json = ConvertTo-Json -Depth 5 -InputObject ([ordered]@{ traceEvents = @($Events) })
  [IO.File]::WriteAllText($Path, $Json)
  Write-Host "Build timeline written to $Path" -ForegroundColor Cyan
}

try {

# The parts of the toolchain, as build.ps1 reads -Toolchain.
$ToolchainPhases = @(switch -Regex ("$Toolchain") {
  '^(\$?true|All)$' { "Bootstrap", "Compilers", "Tools", "Finish" }
  '^Stage2$' { "Compilers", "Tools", "Finish" }
  '^(Bootstrap|Compilers|Tools|Finish)$' { $Matches[1] }
  '^(\$?false|Skip)$' { }
  default { throw "Unknown -Toolchain '$Toolchain', expected All, Bootstrap, Stage2, Compilers, Tools, Finish or Skip" }
})

if ($DependenciesOnly) {
  Invoke-BuildRun "Dependencies" @{ DependenciesOnly = $true }
  exit 0
}

$CompilerTests = @()
$PackageTests = @()
$FoundationTests = @()
$OtherTests = @()
if ($Test -and -not $IsCrossCompiling) {
  $CompilerTests = @($Test | Where-Object { $_ -in "clang", "lld", "lldb", "lldb-swift", "llvm", "swift", "swift-prepare" })
  $PackageTests = @($Test | Where-Object { $_ -in "swift-format", "sourcekit-lsp", "swiftpm", "llbuild" })
  # These take as long as check-swift, so they run on their own.
  $FoundationTests = @($Test | Where-Object { $_ -eq "foundation" })
  # The xctest tests use the FileCheck that the compiler tests build, and the
  # libdispatch that the dispatch tests rebuild.
  $OtherTests = @($Test | Where-Object { $_ -notin $CompilerTests -and $_ -notin $PackageTests -and $_ -notin $FoundationTests })
}

# The runs of build.ps1 that build parts of the toolchain build the compiler
# runtime of the SDKs of -Windows and -Android, but not these SDKs, which other
# runs build, nor the installer and the tests.
function Get-ToolchainRun([string] $Part) {
  return @{ Toolchain = $Part; Test = @(); Package = $false; IncludeSBoM = $false }
}
# The runs that build an SDK, install the toolchain or run tests.
function Get-Run([Hashtable] $Overrides) {
  $Run = @{ Toolchain = "Skip"; Windows = $false; Android = $false; Test = @(); Package = $false; IncludeSBoM = $false }
  foreach ($Override in $Overrides.GetEnumerator()) {
    $Run[$Override.Key] = $Override.Value
  }
  return $Run
}

# The first run cleans the build tree.
if ($ToolchainPhases -contains "Bootstrap") {
  $Run = Get-ToolchainRun "Bootstrap"
  $Run.Clean = [bool]$Clean
  Invoke-BuildRun "Bootstrap" $Run
} elseif ($Clean) {
  Invoke-BuildRun "Clean" (Get-Run @{ Clean = $true })
}

# The projects of the SDK install some files to the same paths for every
# architecture, and the builds with the SDK of the host read them, so the SDKs
# of the other architectures build with their own -BuildRoot, which is merged
# into the image when nothing else uses it.
$StageSDKs = $Windows -and -not ($Package -or $IncludeSBoM -or $Stage) -and ($SelectedWindowsSDKArchitectures -contains $HostSDKArchitecture)
$SDKJobs = @()
$StagedBuildRoots = @()
if ($StageSDKs) {
  foreach ($Architecture in $SelectedWindowsSDKArchitectures) {
    if ($Architecture -eq $HostSDKArchitecture) { continue }
    $Root = "$BinaryCache\SDKStaging\$Architecture"
    Remove-Item -Force -Recurse $Root -ErrorAction Ignore
    $SDKJobs += Start-BuildJob "WindowsSDK-$Architecture" -BelowNormal (Get-Run @{
      Windows = $true;
      WindowsSDKArchitectures = @($Architecture);
      BuildRoot = $Root;
    })
    $StagedBuildRoots += $Root
  }
}

# The Android SDK installs to its own platform.
$AndroidSDKJobs = @()
if ($Android) {
  $AndroidSDKJobs += Start-BuildJob "AndroidSDK" -BelowNormal (Get-Run @{ Android = $true })
}

# The developer tools that only need the SDK of the host build while the Stage2
# compilers build. They build at normal priority, as they mostly compile one
# Swift module at a time, and the Stage2 compilers, which have enough work to
# use all the processors, at the lowest one.
$ToolsJob = $null
if ($ToolchainPhases -contains "Tools") {
  if ($ToolchainPhases -contains "Compilers") {
    $ToolsJob = Start-BuildJob "Tools" (Get-ToolchainRun "Tools")
  } else {
    Invoke-BuildRun "Tools" (Get-ToolchainRun "Tools")
  }
}
if ($ToolchainPhases -contains "Compilers") {
  Invoke-BuildRun "Compilers" (Get-ToolchainRun "Compilers") -Idle
}
if ($ToolsJob) {
  Wait-BuildJobs @($ToolsJob)
}

if ($StageSDKs) {
  # The SDKs of the other architectures are merged into the image, then the SDK
  # of the host is installed again with the settings of the platform, while
  # nothing else uses the image.
  Wait-BuildJobs $SDKJobs
  # A drive like T: would be the current directory of the drive.
  $Destination = [IO.Path]::GetFullPath("$BuildRoot\")
  if ($Destination.Length -gt 3) { $Destination = $Destination.TrimEnd("\") }
  foreach ($Root in $StagedBuildRoots) {
    Write-Host "Merging '$Root' into '$Destination'"
    & robocopy.exe $Root $Destination /E /NFL /NDL /NJH /NJS /NP | Out-Null
    # robocopy reports copied files with exit codes below 8.
    if ($LastExitCode -ge 8) {
      throw "robocopy failed with exit code $LastExitCode"
    }
  }
  Invoke-BuildRun "WindowsSDK-$HostSDKArchitecture" (Get-Run @{
    Windows = $true;
    WindowsSDKArchitectures = @($HostSDKArchitecture);
  })
}

$TestBuildJobs = @()
if ($ToolchainPhases -contains "Finish") {
  # The tests of the compilers only use the Stage2 build tree, so what they need
  # builds now, unless the NoAsserts compilers build in that tree later. It
  # builds many files at the same time, so it does not take the processors from
  # the tools, which build one project at a time.
  if ($CompilerTests -and -not $IncludeNoAsserts) {
    $TestBuildJobs = @(Start-BuildJob "TestBuild" -BelowNormal (Get-Run @{ Test = $CompilerTests; TestPhase = "Build" }))
  }
  Invoke-BuildRun "Finish" (Get-ToolchainRun "Finish")
}
$CompilerTestsBuilt = [bool]$TestBuildJobs

Wait-BuildJobs $AndroidSDKJobs

if ($Package -or $IncludeSBoM -or $Stage -or ($Windows -and -not $StageSDKs)) {
  # The installer patches the compilers, which the tests run, so it builds
  # before them. The Windows SDKs build here, as the installer needs all of
  # them in one run, and the Android SDK again, which it finds built. Nothing
  # else uses the image meanwhile.
  Wait-BuildJobs $TestBuildJobs
  $TestBuildJobs = @()
  $Name = if ($Package -or $IncludeSBoM -or $Stage) { "Package" } else { "WindowsSDK" }
  Invoke-BuildRun $Name (Get-Run @{
    Windows = [bool]$Windows;
    Android = [bool]$Android;
    Package = [bool]$Package;
    IncludeSBoM = [bool]$IncludeSBoM;
    Stage = $Stage;
  })
}

$TestJobs = @()
$Failure = $null
if ($Test -and -not $IsCrossCompiling) {
  # Without the Finish part, this binds the toolchain of the image to the
  # runtime before the tests use it, then builds what the compiler tests need.
  if ($ToolchainPhases -notcontains "Finish") {
    Invoke-BuildRun "TestBuild" (Get-Run @{ Test = $Test; TestPhase = "Build" })
    $CompilerTestsBuilt = $true
  }

  # The package tests build in their own trees, with the toolchain of the image.
  # They run at the same priority as the tests of the compilers, as the tests of
  # SourceKit-LSP time out when they wait for processors.
  if ($FoundationTests) {
    $TestJobs += Start-BuildJob "Test-Foundation" (Get-Run @{ Test = $FoundationTests; TestPhase = "Run" })
  }
  if ($PackageTests) {
    $TestJobs += Start-BuildJob "Test-Packages" (Get-Run @{ Test = $PackageTests; TestPhase = "Run" })
  }

  # The compiler suites build what they need in the same tree, so this happens
  # once before they run.
  Wait-BuildJobs $TestBuildJobs
  if ($CompilerTests -and -not $CompilerTestsBuilt) {
    Invoke-BuildRun "TestBuild" (Get-Run @{ Test = $CompilerTests; TestPhase = "Build" })
  }

  $Tests = @($CompilerTests | Where-Object { $_ -notin "swift", "swift-prepare" }) + $OtherTests
  if ($Tests) {
    $TestJobs += Start-BuildJob "Test-Others" (Get-Run @{ Test = $Tests; TestPhase = "Run" })
  }

  # Run check-swift, the longest suite, here, and report all the failures. It
  # runs many tests at the same time, so it runs at the lowest priority, as the
  # background indexing of SourceKit-LSP does, which its tests wait for.
  if ($CompilerTests -contains "swift") {
    try {
      Invoke-BuildRun "Test-swift" (Get-Run @{ Test = @("swift"); TestPhase = "Run" }) -Idle
    } catch {
      $Failure = $_
    }
  }
}

Wait-BuildJobs $TestJobs
if ($Failure) {
  throw $Failure
}

} catch {
  Stop-BuildJobs
  $BuildFailure = $_
} finally {
  if ($OwnVsDevShellCache) {
    Remove-Item -Recurse -Force $env:SWIFT_BUILD_VSDEVSHELL_CACHE -ErrorAction Ignore
  }
  Write-BuildRuns
  if ($Summary) {
    $EndProcessorTimes = Get-ProcessorTimes
    $BusyTime = $EndProcessorTimes.Busy - $BuildMultiStartProcessorTimes.Busy
    $AllTime = $BusyTime + $EndProcessorTimes.Idle - $BuildMultiStartProcessorTimes.Idle
    try {
      Write-BuildSummary ([DateTime]::Now - $BuildMultiStart) $(if ($AllTime -gt 0) { $BusyTime / $AllTime } else { 0 })
    } catch {
      Write-Warning "Could not write the summary: $_"
    }
  }
  if ($Trace) {
    # Stage the trace with the other CI artifacts so that it can be downloaded.
    $TimingTraceDir = if ($Stage) { $Stage } else { $BinaryCache }
    try {
      Write-BuildRunTraces "$TimingTraceDir\build-timings.json"
    } catch {
      Write-Warning "Could not write the build timeline: $_"
    }
  }
}

if ($BuildFailure) {
  Write-BuildJobFailures
  Write-Host -ForegroundColor Red "Error: $BuildFailure"
  exit 1
}

# The callers read the exit code, which robocopy could have set.
exit 0

}
