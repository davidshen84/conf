export ZSH="$HOME/.oh-my-zsh"

plugins=(
    colorize
    emacs
    emoji
    emotty
    eza
    fzf
    git
    k9s
    kubectl
    kubectx
    ng
    systemd
    themes
    tmux
    ufw
    uv
    z
    zsh-autosuggestions
    zsh-interactive-cd  
    zsh-navigation-tools
    zsh-syntax-highlighting
)

# Initialize zsh completion system
autoload -Uz compinit
compinit

# Modern completion settings
zstyle ':completion:*' menu select
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' auto-description 'always'
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' verbose yes
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|=* r:|=*' 'l:|=* r:|=*'

fpath+=/usr/share/zsh/site-functions/

ZSH_THEME="agnoster"
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=238'
source $ZSH/oh-my-zsh.sh

setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE

alias cat="bat"

export KEYID=4E73C60F64D6824F37D12F6A7E927F94CC52AA1A
export KUBECONFIG="$HOME/.kube/k3s.yaml:$HOME/.kube/xps.yaml"

# Path declarations
export PATH="$HOME/.local/bin:$PATH:$HOME/istio-1.28.0/bin:/home/david/go/bin:$HOME/.pulumi/bin:$HOME/.krew/bin"
export PATH=/home/david/.opencode/bin:$PATH
export PATH="$HOME/.npm-global/bin:$PATH"

source <(istioctl completion zsh)
source <(pulumi gen-completion zsh)

[ -f '/home/david/google-cloud-sdk/path.zsh.inc' ] && source '/home/david/google-cloud-sdk/path.zsh.inc' || :
[ -f '/home/david/google-cloud-sdk/completion.zsh.inc' ] && source '/home/david/google-cloud-sdk/completion.zsh.inc' || :
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && source "$EAT_SHELL_INTEGRATION_DIR/zsh" || :
