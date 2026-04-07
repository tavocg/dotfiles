if [ -d ~/.config/shell/profile.d ] ; then
    for p in ~/.config/shell/profile.d/*.sh; do
        [ -f "$p" ] && . "$p"
    done
    unset p
fi

HISTSIZE=
SAVEHIST=
HISTFILE="$XDG_STATE_HOME/shell/zsh_history"

case $- in
    *i*) ;;
      *) return;;
esac

autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)

autoload -U colors && colors
setopt autocd
stty stop undef
setopt interactive_comments
setopt inc_append_history
bindkey -v
export KEYTIMEOUT=1

if [ -n "$HISTFILE" ] && ! [ -d "${HISTFILE%/*}" ] ; then
    mkdir -p "${HISTFILE%/*}" && touch "$HISTFILE"
fi

_prompt_git_branch() {
    GIT_BRANCH="$(git branch 2>/dev/null | sed '/\*/!d;s/^\*\s*//g;s/\s*$//g')"
    [ -n "$GIT_BRANCH" ] && printf '%s ' "$GIT_BRANCH"
}

precmd() {
    psvar[1]=$(_prompt_git_branch)
}

PS1=$'\e[0;2m%T\e[0m \e[0;34m%~\e[0m \e[0;35;1m%1v\e[0m%(?.%F{green}.%F{red})>\e[0m '

if [ -f ~/.config/shell/aliases.sh ] ; then
    . ~/.config/shell/aliases.sh
fi

if [ -f ~/.config/shell/gui.sh ] ; then
    . ~/.config/shell/gui.sh
fi

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh 2>/dev/null
