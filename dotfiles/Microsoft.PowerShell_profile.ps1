# [In Statements' Order]
# Env Deps: brew
# Pkg Deps: autoconf bash binutils coreutils diffutils ed findutils flex gawk
#       gnu-indent gnu-sed gnu-tar gnu-which gpatch grep gzip less m4 make nano
#       screen watch wdiff wget zip
# Pkg Deps: git
# Pkg Deps: starship

# Homebrew Path
$BREW_BIN = "/usr/local/bin/brew"
if (Test-Path "/opt/homebrew/bin/brew") {
    $BREW_BIN = "/opt/homebrew/bin/brew"
}

# Prioritize GNU Utils over BSD Utils
if ((Get-Command $BREW_BIN -ErrorAction SilentlyContinue)) {
    $brewPrefix = & $BREW_BIN --prefix

    $pathDirs = [Collections.Generic.List[string]]::new()
    $manDirs = [Collections.Generic.List[string]]::new()

    try {
        $optDir = [IO.Path]::Combine($brewPrefix, "opt")
        foreach ($pkgDir in [IO.Directory]::EnumerateDirectories($optDir)) {
            $gnubin = [IO.Path]::Combine($pkgDir, "libexec", "gnubin")
            if ([IO.Directory]::Exists($gnubin)) { $pathDirs.Add($gnubin) }

            $stdbin = [IO.Path]::Combine($pkgDir, "bin")
            if ([IO.Directory]::Exists($stdbin)) { $pathDirs.Add($stdbin) }

            # Man paths
            $gnuman = [IO.Path]::Combine($pkgDir, "libexec", "gnuman")
            if ([IO.Directory]::Exists($gnuman)) { $manDirs.Add($gnuman) }

            $stdman = [IO.Path]::Combine($pkgDir, "share", "man", "man1")
            if ([IO.Directory]::Exists($stdman)) { $manDirs.Add($stdman) }
        }
    } catch {
        # Silently ignore missing dirs
    }

    if ($pathDirs.Count -gt 0) {
        $env:PATH = ($pathDirs -join ':') + ":$env:PATH"
    }
    if ($manDirs.Count -gt 0) {
        $env:MANPATH = ($manDirs -join ':') + ":$env:MANPATH"
    }
}

# Homebrew Binaries
Invoke-Expression (& $BREW_BIN shellenv | Out-String)

# Homebrew Mirrors (See: AutoStarts on Launch)
#$env:HOMEBREW_API_DOMAIN = "https://mirrors.tuna.tsinghua.edu.cn/homebrew-bottles/api"
#$env:HOMEBREW_BOTTLE_DOMAIN = "https://mirrors.tuna.tsinghua.edu.cn/homebrew-bottles"
#$env:HOMEBREW_BREW_GIT_REMOTE = "https://mirrors.tuna.tsinghua.edu.cn/git/homebrew/brew.git"
#$env:HOMEBREW_CORE_GIT_REMOTE = "https://mirrors.tuna.tsinghua.edu.cn/git/homebrew/homebrew-core.git"
#$env:HOMEBREW_PIP_INDEX_URL = "https://pypi.tuna.tsinghua.edu.cn/simple"
# End - Homebrew Mirrors

# Local Binaries
$env:PATH = "$HOME/.local/bin:$env:PATH"

# PS Customization
Invoke-Expression (& starship init powershell)
