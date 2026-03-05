#
# RunProfiler.ps1 - runs a user-specified command while recording a system wide profile .etl file
#
# Example usage: .\RunProfiler.ps1 -Target S:\Path\To\example.exe -TracePath $env:USERPROFILE\example.etl <args to program>
#
# Also see docs/WindowsProfilingTools.md
#

param(
    [string]$Target = ".\example.exe",
    [string]$TracePath = "example_trace.etl",
    [string]$SignalFile = "",
    [switch]$ElevatedHelper,

    [Parameter(Position = 4, ValueFromRemainingArguments = $true)]
    [string[]]$TargetArgs
)

# Resolve script path (so we can re-invoke ourselves elevated and find CpuWithLargeBuffers.wprp)
$scriptPath = $MyInvocation.MyCommand.Path
if (-not $scriptPath) {
    Write-Error "Cannot determine script path. Save this script to a .ps1 file first."
    exit 1
}

function Wait-ForFile {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Path
    )

    $full = [System.IO.Path]::GetFullPath($Path)
    $dir  = [System.IO.Path]::GetDirectoryName($full)
    $file = [System.IO.Path]::GetFileName($full)

    $fsw = New-Object System.IO.FileSystemWatcher
    $fsw.Path = $dir
    $fsw.Filter = $file
    $fsw.NotifyFilter = [System.IO.NotifyFilters]'FileName'
    $fsw.EnableRaisingEvents = $true

    # Unique SourceIdentifier so multiple calls don't collide
    $src = "FileCreated_$([guid]::NewGuid().ToString())"
    $evt = Register-ObjectEvent -InputObject $fsw -EventName Created -SourceIdentifier $src

    try {
        if (Test-Path -LiteralPath $full) {
            return
        }

        Wait-Event -SourceIdentifier $src | Out-Null
    } finally {
        Unregister-Event -SourceIdentifier $src -ErrorAction SilentlyContinue
        $fsw.Dispose()
    }
}

if ($ElevatedHelper) {
    try {
        # === Elevated helper branch (runs as Administrator) ===
        Write-Host "[Elevated] Starting WPR collection..."

        $wprp_file_path = Join-Path ([System.IO.Path]::GetDirectoryName($scriptPath)) "CpuAndWaitsWithLargeBuffers.wprp"
        wpr -start "$wprp_file_path!CpuWithLargeBuffers" -filemode
        if ($LASTEXITCODE -ne 0) {
            Write-Error "[Elevated] Failed to start WPR. Exit code: $LASTEXITCODE"
            exit $LASTEXITCODE
        }

        if (-not $SignalFile) {
            Write-Error "[Elevated] No signal file specified; aborting."
            exit 1
        }

        Write-Host "[Elevated] Waiting for signal file: $SignalFile"
        Wait-ForFile -Path $SignalFile

        # Clean up signal file (optional)
        Remove-Item -LiteralPath $SignalFile -ErrorAction SilentlyContinue

        Write-Host "[Elevated] Stopping WPR. Saving trace to: $TracePath"
        wpr -stop "$TracePath"
        $stopCode = $LASTEXITCODE
        if ($stopCode -ne 0) {
            Write-Error "[Elevated] wpr -stop failed with code: $stopCode"
        } else {
            Write-Host "[Elevated] Trace successfully saved to: $TracePath"
        }

        exit $stopCode
    } finally {
        Read-Host "Press Enter to close this window..."
    }
}

# === Normal controller branch (runs as your user) ===

# Make a unique signal file in %TEMP%
if (-not $SignalFile) {
    $SignalFile = Join-Path $env:TEMP ("wpr_stop_" + [guid]::NewGuid().ToString() + ".signal")
}

# Get an absolute path for the trace file
$callerDir = (Get-Location -PSProvider FileSystem).ProviderPath
$fullTracePath = [System.IO.Path]::GetFullPath([System.IO.Path]::Combine($callerDir, $TracePath))

Write-Host "Trace will be saved to: $fullTracePath"
Write-Host "Signal file: $SignalFile"
Write-Host ""

# Function to start elevated helper (one UAC prompt)
function Start-ElevatedHelper {
    param(
        [string]$ScriptPath,
        [string]$TracePath,
        [string]$SignalFile
    )

    $escapedScript = $ScriptPath.Replace('"', '""')
    $escapedTrace  = $TracePath.Replace('"', '""')
    $escapedSignal = $SignalFile.Replace('"', '""')

    $args = @(
        "-NoProfile"
        "-File"
        "`"$escapedScript`""
        "-ElevatedHelper"
        "-TracePath"
        "`"$escapedTrace`""
        "-SignalFile"
        "`"$escapedSignal`""
    ) -join " "

    $psi = New-Object System.Diagnostics.ProcessStartInfo
    $psi.FileName = "powershell.exe"
    $psi.Arguments = $args
    $psi.Verb = "RunAs"          # <-- single UAC prompt here
    $psi.UseShellExecute = $true

    Write-Host "Requesting elevation for WPR helper..."
    $p = [System.Diagnostics.Process]::Start($psi)
    return $p
}

# 1. Start elevated helper (this will UAC-prompt once)
$elevatedProc = Start-ElevatedHelper -ScriptPath $scriptPath -TracePath $fullTracePath -SignalFile $SignalFile
if (-not $elevatedProc) {
    Write-Error "Failed to start elevated helper process (user may have cancelled UAC)."
    exit 1
}

# Optional: give WPR a moment to start
Start-Sleep -Seconds 2

# 2. Run target as normal user
Write-Host "Running target: $Target"
& $Target @TargetArgs
$targetExit = $LASTEXITCODE
Write-Host "Target exited with code: $targetExit"

# 3. Signal elevated helper to stop WPR
Write-Host "Signalling elevated helper to stop WPR..."
New-Item -ItemType File -Path $SignalFile -Force | Out-Null

# 4. Wait for helper to finish
$elevatedProc.WaitForExit()
Write-Host "Elevated helper exited with code: $($elevatedProc.ExitCode)"

Write-Host ""
Write-Host "Done. ETL trace should be at: $fullTracePath"
exit $targetExit
